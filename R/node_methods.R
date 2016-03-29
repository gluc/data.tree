#
# These are the methods that would normally sit on Node
# However, to reduce the memory footprint of the Node object,
# we only support traditional R methods.
# The first argument of all these methods is node



#' Print a \code{Node} in a human-readable fashion.
#'  
#' @param x The Node
#' @param ... Node attributes to be printed. Can be either a character (i.e. the name of a Node field),
#' a Node method, or a function taking a Node as a single argument. See \code{Get} for details on 
#' the meaning of \code{attribute}.
#' @param pruneMethod The method used to prune for printing. If NULL, the entire tree is displayed. If
#' "simple", then only the first \code{limit} nodes are displayed. If "dist", then Nodes are removed
#' everywhere in the tree, according to their level.
#' @param limit The maximum number of nodes to print. Can be \code{NULL} if the 
#' entire tree should be printed.
#' 
#' 
#' @examples
#' data(acme)
#' print(acme, "cost", "p")
#' print(acme, "cost", probability = "p")
#' print(acme, expectedCost = function(x) x$cost * x$p)
#' do.call(print, c(acme, acme$fieldsAll))
#'
#' @export
print.Node <- function(x, ..., pruneMethod = c("simple", "dist", NULL), limit = 100) {
  pruneMethod <- pruneMethod[1]
  if (length(pruneMethod) > 0 && length(limit) > 0) {
    if (pruneMethod == "simple") {
      x <- PrintPruneSimple(x, limit = limit)    
    } else if (pruneMethod == "dist") {
      x <- PrintPruneDist(x, limit = limit)    
    } else {
      stop (paste0("Unknown pruneMethod ", pruneMethod, "!"))
    }
  } else if(!x$isRoot) {
      #clone s.t. x is root (for pretty level names)
      x <- Clone(x, attributes = TRUE)
      x$parent <- NULL
  }

  df <- ToDataFrameTree(x, ...)
  print(df, na.print = "")
}


#' Aggregate child values of a \code{Node}, standalone or in traversal.
#' 
#' The \code{Aggregate} method lets you fetch an attribute from a \code{Node}'s children, and then aggregate it.
#' For example, you can aggregate cost by summing costs of child \code{Nodes}. This is especially useful in the 
#' context of tree traversal, when using post-order traversal mode.
#' 
#' As with \code{\link{Get}}, the attribute can be a field, a method or a function. If it is a field, and if
#' the node contains the attribute, its value is returned. 
#' Otherwise, \code{aggFun(Aggregate(children, ...))} is called. In that case, 
#' the attribute must be set on the leaf.
#' 
#' @param node the \code{Node} on which to aggregate
#' @param aggFun the aggregation function to be applied to the children's \code{attributes}
#' @param ... any arguments to be passed on to attribute (in case it's a function)
#'
#' @inheritParams Get
#'   
#' @examples
#' data(acme)
#' 
#' #Aggregate on a field
#' Aggregate(acme, "cost", sum)
#' 
#' #Aggregate using Get
#' print(acme, "cost", minCost = acme$Get(Aggregate, "cost", min))
#' 
#' #use Aggregate with a function:
#' acme$Do(function(x) x$expectedCost <- Aggregate(x, 
#'                                                 function(x) x$cost * x$p, 
#'                                                 sum)
#'        , traversal = "post-order")
#'   
#' @seealso \code{\link{Node}}
#' 
#' @export
Aggregate = function(node, 
                     attribute, 
                     aggFun, 
                     ...) {
  
  if("cacheAttribute" %in% names(list(...))) stop("cacheAttribute not supported anymore! Please use Do instead.")
  
  if (isLeaf(node)) return ( GetAttribute(node, attribute, ...) )
  values <- sapply(node$children, 
                   function(x) {
                     v <- GetAttribute(x, attribute, format = identity, ...)
                     if (length(v) > 0 && !is.na(v)) return(v)
                     Aggregate(x, attribute, aggFun, ...)
                   })
  result <- unname(aggFun(values))

  return (result)
}

#' Cumulate values among siblings
#' 
#' For example, you can sum up values of siblings before
#' this \code{Node}.
#' 
#' @param node The node on which we want to cumulate
#' 
#' @inheritParams Aggregate
#' @inheritParams Get
#' 
#' @examples
#' data(acme)
#' acme$Do(function(x) x$cost <- Aggregate(x, "cost", sum), traversal = "post-order")
#' acme$Do(function(x) x$cumCost <- Cumulate(x, "cost", sum))
#' print(acme, "cost", "cumCost")
#' 
#' @export
Cumulate = function(node, attribute, aggFun, ...) {
  if ("cacheAttribute" %in% names(list(...))) stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (node$isRoot) return (GetAttribute(node, attribute, format = identity))
  pos <- node$position
  nodes <- node$parent$children[1:pos]
  res <- aggFun(Get(nodes, attribute, format = identity))
  
  return (res)
}

#' Clone a tree (creates a deep copy)
#' 
#' The method also clones object attributes (such as the formatters), if desired.
#' If the method is called on a non-root, then the parent relationship is not cloned,
#' and the resulting \code{\link{Node}} will be a root.
#' 
#' @param node the root node of the tree or sub-tree to clone
#' @param attributes if FALSE, then R class attributes (e.g. formatters and grViz styles) 
#' are not cloned. This makes the method faster.
#' @return the clone of the tree or sub-tree
#' 
#' @examples
#' data(acme)
#' acmeClone <- Clone(acme)
#' acmeClone$name <- "New Acme"
#' # acmeClone does not point to the same reference object anymore:
#' acme$name
#' 
#' #cloning a subtree
#' data(acme)
#' itClone <- Clone(acme$IT)
#' itClone$isRoot
#' 
#' 
#' @inheritParams Prune
#' 
#' @seealso SetFormat
#' 
#' @export
Clone <- function(node, pruneFun = NULL, attributes = FALSE) {
  .Clone(node, pruneFun, attributes)
}



.Clone <- function(node, pruneFun = NULL, attributes = FALSE, firstCall = TRUE) {

  myclone <- node$clone()
  if (attributes) attributes(myclone) <- attributes(node)
  if (!is.null(pruneFun) && length(node$children) > 0) {
    keep <- sapply(node$children, pruneFun)
    children <- node$children[keep]
    rm(list = names(node$children)[!keep], envir = myclone)
  } else children <- node$children
  myclone$children <- lapply(children, function(x) .Clone(x, pruneFun, attributes, firstCall = FALSE))
  for (child in myclone$children) {
    myclone[[child$name]] <- child
    child$parent <- myclone
  }
  if (length(myclone$children) == 0) myclone$children <- NULL
  if (firstCall) myclone$parent <- NULL #myclone$RemoveAttribute("parent", stopIfNotAvailable = FALSE)
  return (myclone)
}




#' Climb a tree from parent to children, by provided criteria.
#' 
#' 
#' This method lets you climb the tree, from crutch to crutch. On each \code{Node}, the 
#' \code{Climb} finds the first child having attribute value equal to the the provided argument.
#' 
#' @usage #node$Climb(...)
#' Climb(node, ...)
#' 
#' 
#' @param node The root node of the tree or subtree to climb
#' @param ... an attribute name to searched value pairlist. For brevity, you can also provide a character vector.
#' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
#' 
#' @examples
#' data(acme)
#' acme$Climb('IT', 'Outsource')$name
#' acme$Climb('IT')$Climb('Outsource')$name
#' 
#' acme$Climb(name = 'IT')
#' 
#' acme$Climb(position = c(2, 1))
#' #or, equivalent:
#' acme$Climb(position = 2, position = 1)
#' acme$Climb(name = "IT", cost = 250000)
#' 
#' tree <- CreateRegularTree(5, 2)
#' tree$Climb(c("1", "1"), position = c(2, 2))$path
#'
#' @seealso \code{\link{Node}}
#'
#' @export
Climb <- function(node, ...) {
  
  path <- list(...)
  if (length(path) == 0) {
    return (node)
  } else {
    
    #convert args to standard
    #e.g. id = (3, 5), name = "myname"
    #to
    # id = 3, id = 5, name = "mynam"
    # path <- list(id = c(3, 5), "myname", c("bla", "blo"))
    # path <- list(id = 3, id = 5, name = "myname")
    # path <- c("IT")
    mpath <- NULL
    for (i in 1:length(path)) names(path[[i]]) <- rep(names(path)[i], length(path[[i]]))
    for (i in 1:length(path)) mpath <- c(mpath, as.list(path[[i]]))
    
    attribute <- names(mpath)[[1]]
    if (length(attribute) == 0 || is.na(attribute) || nchar(attribute) == 0) attribute <- "name"
    
    
    value <- mpath[[1]]
    
    getA <- Get(node$children, attribute)
    child <- node$children[getA == value]
    if(length(child) == 0) return (NULL)
    
    child <- child[[1]]
    
    if (is.null(child)) {
      return (NULL)
    } else if (length(mpath) == 1) {
      return (child)
    } else {
      return (do.call(Climb, c(node = child, mpath[-1])))
    }
  }
  
}



#' Find a node by name in the (sub-)tree
#' 
#' Scans the entire sub-tree spanned by \code{node} and returns the first \code{\link{Node}}
#' having the \code{name} specified. This is mainly useful for trees whose name is unique.
#' If \code{\link{AreNamesUnique}} is \code{FALSE}, i.e. if there is more than one \code{Node}
#' called \code{name} in the tree, then it is undefined which one will be returned. 
#' Also note that this method is not particularly fast. See examples for a faster way to 
#' index large trees, if you need to do multiple searches. See \code{\link{Traverse}} if
#' you need to find multiple \code{Nodes}.
#' 
#' @param node The root \code{Node} of the tree or sub-tree to search
#' @param name The name of the \code{Node} to be returned
#' 
#' @return The first \code{Node} whose name matches, or \code{NULL} if no such \code{Node} is
#' found.
#' 
#' @examples
#' data(acme)
#' acme$FindNode("Outsource")
#' 
#' #re-usable hashed index for multiple searches:
#' if(!AreNamesUnique(acme)) stop("Hashed index works for unique names only!")
#' trav <- Traverse(acme, "level")
#' names(trav) <- Get(trav, "name")
#' nameIndex <- as.environment(trav)
#' #you could also use hash from package hash instead!
#' #nameIndex <- hash(trav)
#' nameIndex$Outsource
#' nameIndex$IT
#' 
#' 
#' @seealso AreNamesUnique, Traverse
#' 
#' @export
FindNode <- function(node, name) {
  trav <- Traverse(node, filterFun = function(x) x$name == name)
  if (length(trav) == 0) return(NULL)
  return(trav[[1]])
}




#' Get an attribute from a Node.
#' 
#' @param node The \code{\link{Node}} from which the \code{attribute} should be fetched.
#' @param nullAsNa If TRUE (the default), then NULL is returned as NA. Otherwise it is returned as NULL.
#' 
#' 
#' @inheritParams Get
#' 
#' @examples
#' data(acme)
#' GetAttribute(acme$IT$Outsource, "cost")
#' 
#' @export
GetAttribute <- function(node, attribute, ..., format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
  if (is.function(attribute)) {
    #function
    v <- attribute(node, ...)
  } else if(is.character(attribute) && length(attribute) == 1) {
    #property
    v <- node[[attribute]]
    if (is.function(v)) {
      if (is.null(formals(v))) v <- v()
      else if (names(formals(v))[[1]] == "self") v <- v(self = node, ...) #allow storing functions whose first arg is self
      else v <- v(...)
    }
  } else {
    stop("attribute must be a function, the name of a public property, or the name of method")
  }
  
  if (length(v) == 0 && inheritFromAncestors && !node$isRoot) {
    v <- GetAttribute(node$parent, attribute, 
                      ..., 
                      inheritFromAncestors = TRUE,
                      nullAsNa = FALSE)
  }
  
  if (!nullAsNa && length(v) == 0) return (NULL)
  if (length(v) == 0) v <- NA
  if (length(v) == 0) v <- NA
  
  
  
  if (is.null(format) && !is.function(attribute)) {
    #get default formatter
    format <- GetObjectAttribute(node, "formatters")[[attribute]]
  }
  
  if (!is.null(format)) {
    if (!is.function(format)) stop("format must be a function!")
    v <- format(v)
  }
  
  if (FALSE) {
    if (is.vector(v) && length(v) == 1) {
      names(v) <- node$name 
    } else if (length(v) > 1) {
      if (!is.null(names(v))) nms <- names(v)
      else nms = NULL
      dim(v) <- c(length(v), 1)
      colnames(v) <- node$name
      if (!is.null(nms)) rownames(v) <- nms
    }
  }  
  return (v)
}

GetObjectAttribute <- function(node, name) {
  a <- attr(node, name)
  if (length(a) > 0 || node$isRoot) return (a)
  return ( GetObjectAttribute(node$parent, name))
}

#' Set a formatter function on a specific node
#' 
#' Formatter functions set on a Node act as a default formatter when printing and using
#' the \code{\link{Get}} method. The formatter is inherited, meaning that whenever
#' \code{Get} fetches an attribute from a \code{Node}, it checks on the \code{Node} or
#' on any of its ancestors whether a formatter is set.
#' 
#' @param node The node on which to set the formatter
#' @param name The attribute name for which to set the formatter
#' @param formatFun The formatter, i.e. a function taking a value as an input, and formatting
#' returning the formatted value
#' 
#' @examples
#' data(acme)
#' acme$Set(id = 1:(acme$totalCount))
#' SetFormat(acme, "id", function(x) FormatPercent(x, digits = 0))
#' SetFormat(acme$Climb("IT"), "id", FormatFixedDecimal)
#' print(acme, "id")
#' # Calling Get with an explicit formatter will overwrite the default set on the Node:
#' print(acme, id = acme$Get("id", format = function(x) paste0("id:", x)))
#' 
#' # Or, to avoid formatters, even though you set them on a Node:
#' print(acme, id = acme$Get("id", format = identity))
#' 
#' 
#' @seealso Get
#' @seealso print.Node
#' 
#' @export
SetFormat <- function(node, name, formatFun) {
  if (length(attr(node, "formatters")) == 0) attr(node, "formatters") <- list()
  attr(node, "formatters")[[name]] <- formatFun
}





#' Test whether all node names are unique.
#' 
#' This can be useful for some conversions.
#' @param node The root \code{Node} of the \code{data.tree} structure to test
#' 
#' @return \code{TRUE} if all \code{Node$name == TRUE} for all nodes in the tree
#' 
#' @examples
#' data(acme)
#' AreNamesUnique(acme)
#' acme$name <- "IT"
#' AreNamesUnique(acme)
#' 
#' @seealso as.igraph.Node
#' @export
AreNamesUnique <- function(node) {
  mynames <- node$Get("name")
  all(duplicated(mynames) == FALSE)
}