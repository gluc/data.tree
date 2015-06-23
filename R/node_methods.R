#' Traverse a Tree and Collect Values
#' 
#' The \code{Get} function is one of the most important ones of the \code{data.tree} package. It lets you traverse a tree
#' and collect values along the way. Alternatively, you can call a method or a function on each \code{Node}.
#' 
#' @param node The node on which to perform the Get
#'   @param attribute determines what is collected during traversal. The attribute can be
#'       \itemize{
#'         \item a.) the name of a field of each \code{Node} in the tree 
#'         \item b.) the name of a Method of each \code{Node}.
#'         \item c.) a function, whose first argument must be a node. In that case, the \code{Get} method calls the function by 
#'         passing \code{...} to the function.
#'        }
#'  @param traversal determines the traversal order. It can be either "pre-order", "post-order", or "ancestor"
#'  @param filterFun allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
#'  Note that if filter returns \code{FALSE}, then the node and its entire subtree are ignored and neither traversed nor returned.
#'  @param assign can be the name of a variable to which we assign the collected values before \code{format} is called.
#'  @param format can be a function that transforms the collected values, e.g. for printing
#'  
#'  @return a vector containing the \code{atrributes} collected during traversal, in traversal order. \code{NULL} is converted
#'  to NA, such that \code{length(Node$Get) == Node$totalCount}
#'  
#'  @examples
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
#'  
#' @keywords internal
Get = function(node, attribute, 
               ..., 
               traversal = "pre-order", 
               filterFun = NULL, 
               assign = NULL, 
               format = NULL,
               inheritFromAncestors = FALSE) {
  #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
  v <- vector()
  if(traversal == "pre-order") {
    
    if(is.null(filterFun) || filterFun(node)) {
      
      for(child in node$children) {
        v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
      }
      me <- node$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
      v <- c(me, v)
    }
    
  } else if (traversal == "post-order") {
    # useful if leafs need to be calculated first
    
    if(is.null(filterFun) || filterFun(node)) {
      for(child in node$children) {
        v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
      }
      me <- node$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
      v <- c(v, me)
    }
    
  } else if (traversal == "ancestor") {
    
    
    if (!node$isRoot) {
      v <- node$parent$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
    }
    if(is.null(filterFun) || filterFun(node)) {
      me <- node$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
      v <- c(me, v)
      
    }
  }
  if (is.null(assign)) return (v)
  invisible (v)
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
                                  assign = assign, 
                                  format = format, 
                                  inheritFromAncestors = inheritFromAncestors)
  }
  
  if (!nullAsNa && length(v) == 0) return (NULL)
  if (length(v) == 0) v <- NA
  if (length(v) == 0) v <- NA
  
  if(!is.null(assign)) node[[assign]] <- v
  names(v) <- node$name
  
  if(!is.null(format)) {
    if (!is.function(format)) stop("format must be a function!")
    v <- format(v)
  }
  return (v)
}