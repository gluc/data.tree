GetNodes = function(node, 
                    traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), 
                    pruneFun = NULL,
                    filterFun = NULL) {
  #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
  traversal = traversal[1]
  nodes <- list()
  if(traversal == "pre-order" || traversal == "post-order") {
    
    if(length(pruneFun) == 0 || pruneFun(node)) {
      
      for(child in node$children) {
        nodes <- c(nodes, GetNodes(child, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun))
      }
      if(length(filterFun) == 0 || filterFun(node)) {
        if(traversal == "pre-order") nodes <- c(node, nodes)
        else nodes <- c(nodes, node)
      }
    }
    
  } else if(traversal == "in-order") {
    if(!node$isBinary) stop("traversal in-order valid only for binary trees")
    if(length(pruneFun) == 0 || pruneFun(node)) {
      if(!node$isLeaf) {
        n1 <- GetNodes(node$children[[1]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
        if(length(filterFun) == 0 || filterFun(node)) n2 <- node
        else n2 <- list()
        n3 <- GetNodes(node$children[[2]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
        nodes <- c(n1, n2, n3)
      } else {
        if(length(filterFun) == 0 || filterFun(node)) n2 <- node
        else n2 <- list()
        nodes <- c(nodes, n2)
      }
    }
    
  } else if (traversal == "ancestor") {
    
    
    if (!node$isRoot) {
      nodes <- GetNodes(node$parent, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
    }
    
    if(length(filterFun) == 0 || filterFun(node)) {
      nodes <- c(node, nodes)
    }
    
  } else if (traversal == "level") {
    
    for(level in 1:node$depth) {
      fifu <- function(x) {
        a <- (length(filterFun) == 0 || filterFun(x))
        b <- x$level == level
        return (a && b)
      }
      nodes <- c(nodes, GetNodes(node, pruneFun = pruneFun, filterFun = fifu))
    }
  } else {
    stop("traversal must be pre-order, post-order, in-order, ancestor, or level")
  }
  return (nodes)
}



#' Traverse a Tree and Collect Values
#' 
#' The \code{Get} method is one of the most important ones of the \code{data.tree} package. It lets you traverse a tree
#' and collect values along the way. Alternatively, you can call a method or a function on each \code{\link{Node}}.
#' 
#' @param node The node on which to perform the Get
#'   @param attribute determines what is collected during traversal. The attribute can be
#'       \itemize{
#'         \item a.) the name of a field of each \code{Node} in the tree 
#'         \item b.) the name of a Method of each \code{Node}.
#'         \item c.) a function, whose first argument must be a node. In that case, the \code{Get} method calls the function by 
#'         passing \code{...} to the function.
#'         }
#' @param ... in case \code{attribute} is a function or a method, the ellipsis is passed to it as additional arguments.
#' @param traversal any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'
#' @param pruneFun allows providing a a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
#' If the pruneFun returns FALSE for a Node, then the Node and all its sub-tree will not be considered.
#' @param filterFun allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
#' Note that if filter returns \code{FALSE}, then the node will be excluded from the result (but not the entire subtree).
#' @param assign can be the name of a variable to which we assign the collected values before \code{format} is called.
#' @param format can be a function that transforms the collected values, e.g. for printing
#' @param inheritFromAncestors if \code{TRUE}, then the path above a \code{Node} is searched to get the \code{attribute} in case it is NULL.
#'  
#' @return a vector containing the \code{atrributes} collected during traversal, in traversal order. \code{NULL} is converted
#' to NA, such that \code{length(Node$Get) == Node$totalCount}
#'  
#' @examples
#'data(acme)
#'acme$Get("level")
#'acme$Get("totalCount")
#'  
#'calculateAggregateChildCost <- function(node, fun) {
#'  if (node$isLeaf) return(node$cost)
#'  fun(sapply(node$children, function(x) x$averageCost))
#'}
#'
#'myFormat <- function(x) {
#'  format(x, nsmall=2, scientific = FALSE)
#'}
#'
#'acme$Get(calculateAggregateChildCost, 
#'         mean, 
#'         traversal = "post-order", 
#'         assign = "averageCost", 
#'         format = myFormat)
#'  
#' @seealso \code{\link{Node}}
#' @seealso \code{\link{Set}}
#'  
#' @export
Get = function(node, 
               attribute, 
               ..., 
               traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), 
               pruneFun = NULL,
               filterFun = NULL, 
               assign = NULL, 
               format = NULL,
               inheritFromAncestors = FALSE) {
  
  nodes <- GetNodes(node, 
                    traversal = traversal, 
                    pruneFun = pruneFun, 
                    filterFun = filterFun)
  
  res <- sapply(nodes, function(x) x$GetAttribute(attribute, 
                                                  ...,
                                                  assign = assign, 
                                                  format = format, 
                                                  inheritFromAncestors = inheritFromAncestors)
         )
  
  
  if (length(assign) == 0) return (res)
  invisible (res)
}

#' Traverse a Tree and Assign Values
#' 
#' The method takes one or more vectors as an argument. It traverses the tree, and assigns values to variables, whereby the values are picked
#' from the vector. Also available as OO-style method on \code{\link{Node}}.
#' 
#' @param node The \code{Node} to traverse
#' @param ... each argument can be a vector of values to be assigned. Recycled.
#' @param traversal any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'
#' @param pruneFun A pruning function, returning \code{TRUE} if a \code{Node} and its sub
#' tree should be kept.
#' @param filterFun A filter function, returning \code{TRUE} if a \code{Node} should be kept.
#'
#' @return invisibly returns the node (useful for chaining)  
#'  
#' @examples
#' data(acme)
#' acme$Set(departmentId = 1:acme$totalCount, openingHours = NULL, traversal = "post-order")
#' acme$Set(head = c("Jack Brown", 
#'                   "Mona Moneyhead", 
#'                   "Dr. Frank N. Stein", 
#'                   "Eric Nerdahl"
#'                   ),
#'          filterFun = function(x) !x$isLeaf
#'         )
#' print(acme, "departmentId", "head")
#'  
#' @seealso \code{\link{Node}}
#' @seealso \code{\link{Get}}
#'  
#' @export
Set <- function(node, 
                ..., 
                traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), 
                pruneFun = NULL, 
                filterFun = NULL) {
  
  traversal <- traversal[1]
  args <- list(...)
  argsnames <- sapply(substitute(list(...))[-1], deparse)
  gargsnames <- names(args)
  if (is.null(gargsnames)) gargsnames <- vector(mode = "character", length = length(args))
  gargsnames[nchar(gargsnames) == 0] <- argsnames[nchar(gargsnames) == 0]
  names(args) <- gargsnames
  
  nodes <- GetNodes(node, 
                    traversal = traversal, 
                    pruneFun = pruneFun, 
                    filterFun = filterFun)
  
  appFun <- function(x, name, arg) {
    x[[name]] <- arg
  }
  
  for(nme in names(args)) {
    arg <- args[[nme]]
    if (length(arg) == 0) arg <- vector("list", 1)
    mapply(appFun, nodes, nme, arg)
  }
  
  invisible (node)

}


#' Traverse a Tree and Perform Aggregation Operations
#' 
#' The \code{Aggregate} method lets you set e.g. a value on the leafs, and then sum them up along the tree.
#' 
#' @param node the \code{Node} on which to aggregate
#' @param attribute the attribute that is being called on every node. The attribute can be 
#' field, a property or a method. If the node contains #' the attribute, its value is return. 
#' Otherwise, \code{fun(children$Aggregate(...))} is called. To use the Attribute method, 
#' the attribute must be set on the leaf.
#' @param fun a function to be applied
#' @param ... any arguments to be passed on to fun
#' 
#' @examples
#' data(acme)
#' acme$Aggregate("cost", sum)
#' acme$Get("Aggregate", "cost", sum)
#' print(acme, totalCost = acme$Get("Aggregate", "cost", sum))
#' 
#' @seealso \code{\link{Node}}
#'
#' @export
Aggregate = function(node, attribute, fun, ...) {
  #if(is.function(attribute)) browser()
  v <- GetAttribute(node, attribute, ..., nullAsNa = FALSE)
  if (!length(v) == 0) {
    return (v)
  }
  if (node$isLeaf) stop(paste0("Attribute returns NULL on leaf!"))
  values <- sapply(node$children, function(x) x$Aggregate(attribute, fun, ...))
  result <- fun(values)
  return (result)
}


#' Checks if a node is a leaf
#' @param node The Node to test.
#' @return TRUE if the Node is a leaf, FALSE otherwise
#' @export
isLeaf = function(node) {
  return (node$isLeaf)
}


GetAttribute = function(node, attribute, ..., assign = NULL, format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
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
    v <- node$parent$GetAttribute(attribute, 
                                  ..., 
                                  inheritFromAncestors = TRUE,
                                  nullAsNa = FALSE)
  }
  
  if (!nullAsNa && length(v) == 0) return (NULL)
  if (length(v) == 0) v <- NA
  if (length(v) == 0) v <- NA
  
  if(!is.null(assign)) node[[assign]] <- v
  if(is.vector(v)) names(v) <- node$name
  
  if(is.null(format) && !is.function(attribute)) {
    format <- GetAttribute(node, function(x) x$formatters[[attribute]], inheritFromAncestors = TRUE, nullAsNa = FALSE)
  }
   
  if(!is.null(format)) {
    if (!is.function(format)) stop("format must be a function!")
    v <- format(v)
  }
  return (v)
}



