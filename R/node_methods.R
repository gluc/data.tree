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
#' @param limit The maximum number of nodes to print. Can be \code{NULL} if the 
#' entire tree should be printed
#' 
#' @examples
#' data(acme)
#' print(acme, "cost", "p")
#' print(acme, "cost", probability = "p")
#' print(acme, expectedCost = function(x) x$cost * x$p)
#' do.call(print, c(acme, acme$fieldsAll))
#'
#' @export
print.Node <- function(x, ..., limit = 100) {
  # algo
  # 1. find number of nodes to remove, n
  # 2. get leaves where position > 2
  # 3. order by 
  #    a.) parent$count, descending, 
  #    b.) then by level, descending
  #    c.) then by parent$id, descending
  # 4.   
  toBeCropped <- x$totalCount - limit
  if (toBeCropped > 0) x <- PruneNaive(x, limit = limit)
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
#' @param cacheAttribute the name to which results should be stored, if any (NULL otherwise). If not
#' NULL, then the function checks whether this attribute is set, and only evaluates
#' attribute if it is not.
#' If used wisely in connection with post-order traversal, this parameter allows to speed up calculation by breaking
#' recursion.
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
#' #use Aggregate with caching:
#' acme$cost
#' acme$Do(function(x) Aggregate(x, "cost", sum, cacheAttribute = "cost"), traversal = "post-order")
#' acme$cost
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
                     cacheAttribute = NULL,
                     ...) {
  
  
  
  
  #####
  #if(is.function(attribute)) browser()
  #if (!is.function(attribute)) {
  if (length(cacheAttribute) > 0) {
    v <- GetAttribute(node, cacheAttribute, ..., nullAsNa = FALSE)
    if (!length(v) == 0) return (v)
  } 
  
  
  v <- GetAttribute(node, attribute, ..., nullAsNa = FALSE)
  if (!length(v) == 0) result <- v
  else if (node$isLeaf) stop(paste0("Attribute returns NULL on leaf!"))
  
  if (!exists("result", envir = environment()) || length(result) == 0) {
    values <- sapply(node$children, function(x) Aggregate(x, attribute, aggFun, cacheAttribute, ...))
    result <- as.vector(aggFun(values))
  }
  if (length(cacheAttribute) > 0) {
    node[[cacheAttribute]] <- result
  }
  return (result)
}

#' Cumulate values among siblings
#' 
#' For example, you can sum up values of siblings before
#' this \code{Node}.
#' 
#' @param node The node on which we want to cumulate
#' @param cacheAttribute A field into which the results should
#' be cached. Speeds up calculation.
#' 
#' @inheritParams Aggregate
#' @inheritParams Get
#' 
#' @examples
#' data(acme)
#' acme$Do(function(x) Aggregate(x, "cost", sum, "cost"), traversal = "post-order")
#' acme$Do(function(x) Cumulate(x, "cost", sum, "cumCost"))
#' print(acme, "cost", "cumCost")
#' 
#' @export
Cumulate = function(node, attribute, aggFun, cacheAttribute, ...) {
  pos <- node$position
  if(length(cacheAttribute) > 0 || node$isRoot) {
    res <- as.vector(GetAttribute(node, attribute, ..., nullAsNa = FALSE))
    if (pos > 1) {
      res <- aggFun(node$parent$children[[pos - 1]][[cacheAttribute]], res)
    }
    node[[cacheAttribute]] <- res
  } else {
    nodes <- Traverse(node$parent, 
                      pruneFun = function(x) x$level <= (node$level + 1),
                      filterFun = function(x) x$position <= pos)
                    
    res <- aggFun(Get(nodes, attribute))
  }
  return (res)
}

#' Clone a tree (creates a deep copy)
#' 
#' The method also clones object attributes (such as the formatters). 
#' 
#' @param node the root node of the tree or sub-tree to clone
#' @return the clone of the tree
#' 
#' @examples
#' data(acme)
#' acmeClone <- Clone(acme)
#' acmeClone$name <- "New Acme"
#' # acmeClone does not point to the same reference object anymore:
#' acme$name
#' 
#' @seealso SetFormat
#' 
#' @export
Clone <- function(node) {
  l <- as.list(node, mode = "explicit", rootName = node$name)
  clone <- as.Node(l, mode = "explicit")

  #attributes
  filterFun <- function(x) {
    length(attributes(x)) > 1
  }
  t <- Traverse(node, filterFun = filterFun)
  as <- lapply(t, function(x) attributes(x))
  names(as) <- Get(t, "pathString")
  tc <- Traverse(clone, filterFun = function(x) x$pathString %in% names(as))
  Do(tc, function(x) attributes(x) <- as[[x$pathString]])
  return (clone)
}




#' Find a \code{Node} by its path
#' 
#' 
#' This method lets you climb the tree, from crutch to crutch. More precisely,
#' \code{Climb} returns the \code{Node} at path \code{...}. The path is relative to the \code{Node} on which this method is called. Each argument provided corresponds to an 
#' element in the path, specified by the \code{Node}'s name. 
#' 
#' @param node The root node
#' @param ... the names of the nodes in the path
#' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
#' 
#' @examples
#' data(acme)
#' acme$Climb('IT', 'Outsource')$name
#' acme$Climb('IT')$Climb('Outsource')$name
#'
#' @seealso \code{\link{Node}}
#'
#' @export
Climb <- function(node, ...) {
  
  path <- as.character(list(...))
  if (length(path) == 0) {
    return (node)
  } else {
    child <- node$children[[path[1]]]
    if (is.null(child)) {
      return (NULL)
    } else if (length(path) == 1) {
      return (child)
    } else {
      return (do.call(Climb, c(node = child, list(...)[-1])))
    }
  }
  
}


###############################
## Private Methods

GetAttribute <- function(node, attribute, ..., format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
  if(is.function(attribute)) {
    #function
    
    v <- attribute(node, ...)
  } else if(is.character(attribute) && length(attribute) == 1) {
    #property
    v <- node[[attribute]]
    if (is.function(v)) v <- v(...)
  } else {
    stop("attribute must be a function, the name of a public property, or the name of method")
  }
  
  if(length(v) == 0 && inheritFromAncestors && !node$isRoot) {
    v <- GetAttribute(node$parent, attribute, 
                      ..., 
                      inheritFromAncestors = TRUE,
                      nullAsNa = FALSE)
  }
  
  if (!nullAsNa && length(v) == 0) return (NULL)
  if (length(v) == 0) v <- NA
  if (length(v) == 0) v <- NA
  
  if(is.vector(v)) names(v) <- node$name
  
  if(is.null(format) && !is.function(attribute)) {
    format <- GetObjectAttribute(node, "formatters")[[attribute]]
  }
  
  if(!is.null(format)) {
    if (!is.function(format)) stop("format must be a function!")
    v <- format(v)
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