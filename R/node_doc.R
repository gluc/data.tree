

#' Find a \code{Node} by its path
#' 
#' 
#' Find returns the \code{Node} at path \code{...}. The path is relative to the \code{Node} on which this method is called. Each argument provided corresponds to an 
#' element in the path, specified by the \code{Node}'s name.
#' 
#' 
#' @param ... the names of the nodes in the path
#' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
#' 
#' @examples
#' data(acme)
#' acme$Find('IT', 'Outsource')$name
#' #This is equivalent to:
#' acme$Find('IT')$Find('Outsource')$name
#' acme$Find('X', 'Y', 'Z')
#'
#'
#' @seealso \code{\link{Node}}
#'
#' @keywords internal
Find = function(...) {
  stop("This method can only be called on a Node!")
}








#' Sort Children of a Node or an Entire Tree
#' 
#' You can sort with respect to any argument of the tree.
#' 
#' @param attribute a field, method or function. The result of the attribute determines the 
#' sorting. If it is a function, #' the attribute must take a \code{Node} as a first argument.
#' @param ... any parameters to be passed on the the attribute (in case it's a method or a 
#' function)
#' @param decreasing sort order
#' @param recursive if \code{TRUE}, Sort will be called recursively on the \code{Node}'s children. 
#' This allows sorting an entire tree.
#' 
#' @return Returns the node on which Sort is called, invisibly. This can be useful to chain Node methods.
#' 
#' @examples
#' data(acme)
#' acme$Do(function(x) x$totalCost <- Aggregate(x, "cost", sum))
#' acme$Sort("totalCost", decreasing = TRUE)
#' print(acme, "totalCost")
#' 
#' @seealso \code{\link{Node}}
#' @keywords internal
Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
  stop("This method can only be called on a Node!")
}

#' Reverts the sort order of a \code{Node}'s children.
#' 
#' @param recursive If \code{TRUE}, then revert is called recursively on
#' all children.
#' 
#'
#' #' @seealso \code{\link{Node}}
#' @keywords internal 
Revert = function(recursive = TRUE) {
  stop("This method can only be called on a Node!")
}


#' Convert a \code{\link{Node}} object to a nested \code{list}
#' 
#' @param x The Node to convert
#' @param unname If TRUE, then the nested children list will not have named arguments. This
#' can be useful e.g. in the context of conversion to JSON, if you prefer the children to be
#' an array rather than named objects.
#' @param nameName The name that should be given to the name element
#' @param childrenName The name that should be given to the children nested list
#' @param ... Additional parameters
#' 
#' @seealso \code{\link{Node}}, \code{\link{as.list.Node}} 
#' @keywords internal
#' 
ToList <- function(unname = FALSE, 
                   nameName = 'name', 
                   childrenName = 'children', ...) {
  stop("This method can only be called on a Node!")
}




#' Prunes a tree by applying the filterFun to each node. 
#' 
#' You can apply this to any \code{\link{Node}}
#' in a tree; the filterFun is applied to the children of the \code{\link{Node}} only. Note that as
#' soon as the filter criteria is not matched anymore, the algorithm stops, and neither the \code{\link{Node}}
#' nor its children are kept, regardless of whether the child would satisfy the filter.
#' 
#' @param traversal one of the traversal modes (pre-order, post-order)
#' @param filterFun A function taking a \code{\link{Node}} as an argument, an returning \code{TRUE}
#' if the \code{\link{Node}} should be kept, false otherwise.
#' @keywords internal
Prune <- function(traversal = "pre-order", filterFun) {
  stop("This method can only be called on a Node!")
}

