#' Traverse a tree or a sub-tree
#' 
#' @param node the root of a tree or a sub-tree that should be traversed
#' @param traversal any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'
#' @param pruneFun allows providing a a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
#' If the pruneFun returns FALSE for a Node, then the Node and all its sub-tree will not be considered.
#' @param filterFun allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
#' Note that if filter returns \code{FALSE}, then the node will be excluded from the result (but not the entire subtree).
#'
#' @return a list of \code{Node}s
#' @export
Traverse = function(node, 
                    traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), 
                    pruneFun = NULL,
                    filterFun = NULL) {
  #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
  traversal = traversal[1]
  nodes <- list()
  if(traversal == "pre-order" || traversal == "post-order") {
    
    if(length(pruneFun) == 0 || pruneFun(node)) {
      
      for(child in node$children) {
        nodes <- c(nodes, Traverse(child, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun))
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
        n1 <- Traverse(node$children[[1]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
        if(length(filterFun) == 0 || filterFun(node)) n2 <- node
        else n2 <- list()
        n3 <- Traverse(node$children[[2]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
        nodes <- c(n1, n2, n3)
      } else {
        if(length(filterFun) == 0 || filterFun(node)) n2 <- node
        else n2 <- list()
        nodes <- c(nodes, n2)
      }
    }
    
  } else if (traversal == "ancestor") {
    
    
    if (!node$isRoot) {
      nodes <- Traverse(node$parent, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
    }
    
    if(length(filterFun) == 0 || filterFun(node)) {
      nodes <- c(node, nodes)
    }
    
  } else if (traversal == "level") {
    
    for(level in node$level:(node$level + node$depth)) {
      fifu <- function(x) {
        a <- (length(filterFun) == 0 || filterFun(x))
        b <- x$level == level
        return (a && b)
      }
      nodes <- c(nodes, Traverse(node, pruneFun = pruneFun, filterFun = fifu))
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
#' @param nodes The nodes on which to perform the Get (e.g. obtained via \code{\link{Traverse}}
#'   @param attribute determines what is collected during traversal. The attribute can be
#'       \itemize{
#'         \item a.) the name of a field of each \code{Node} in the tree 
#'         \item b.) the name of a Method of each \code{Node}.
#'         \item c.) a function, whose first argument must be a node. In that case, the \code{Get} method calls the function by 
#'         passing \code{...} to the function.
#'         }
#' @param ... in case \code{attribute} is a function or a method, the ellipsis is passed to it as additional arguments.
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
#'         format = myFormat)
#'  
#' @seealso \code{\link{Node}}
#' @seealso \code{\link{Set}}
#'  
#' @export
Get = function(nodes, 
               attribute, 
               ..., 
               format = NULL,
               inheritFromAncestors = FALSE) {
  
  
  res <- sapply(nodes, function(x) GetAttribute(x, attribute, 
                                                ...,
                                                format = format, 
                                                inheritFromAncestors = inheritFromAncestors)
  )
  
  
  return (res)
}

#' Executes a function an a set of nodes
#' @param nodes a set of nodes, usually obtained via \code{\link{Traverse}}
#' @param fun the function to execute. The function is expected to be either a Method, or to take a 
#' Node as its first argument
#' @param ... any additional parameters to be passed on to fun
#' 
#' @export
Do <- function(nodes,
               fun, 
               ...) {
  
  f <- function(x) GetAttribute(x, attribute = fun, 
                                ...)
  
  mapply(fun, nodes, MoreArgs = list(...))
  invisible (nodes)
}




#' Traverse a Tree and Assign Values
#' 
#' The method takes one or more vectors as an argument. It traverses the tree, whereby the values are picked
#' from the vector. Also available as OO-style method on \code{\link{Node}}.
#' 
#' @param nodes The \code{Node}s to traverse
#' @param ... each argument can be a vector of values to be assigned. Recycled.
#'
#' @return invisibly returns the nodes (useful for chaining)  
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
Set <- function(nodes, 
                ...) {
  
  args <- list(...)
  argsnames <- sapply(substitute(list(...))[-1], deparse)
  gargsnames <- names(args)
  if (is.null(gargsnames)) gargsnames <- vector(mode = "character", length = length(args))
  gargsnames[nchar(gargsnames) == 0] <- argsnames[nchar(gargsnames) == 0]
  names(args) <- gargsnames
  
  
  
  appFun <- function(x, name, arg) {
    x[[name]] <- arg
  }
  
  for(nme in names(args)) {
    arg <- args[[nme]]
    if (length(arg) == 0) arg <- vector("list", 1)
    mapply(appFun, nodes, nme, arg)
  }
  
  invisible (nodes)
  
}
