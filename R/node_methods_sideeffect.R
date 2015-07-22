#
# These are methods on Node which have side effects, meaning they
# change a Node object or any of its descendants. To keep the 
# memory footprint of the Node object small, and to be able to 
# document them, they are implemented in traditional R style,
# and their OO part is only a wrapper around the methods here.
#
# Requirements for side effect methods
# 1. they are implement here
# 2. their OO part in Node.R is a wrapper
# 3. the Node documentation links to here
# 4. the methods here are not exported
# 5. the methods here are marked as internal, so a to have roxygen generate documentation
#







#' Sort Children of a Node or an Entire Tree
#' 
#' You can sort with respect to any argument of the tree.
#' @param node The node whose children are to be sorted 
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
#' acme$Do(function(x) x$totalCost <- Aggregate(x, "cost", sum), traversal = "post-order")
#' acme$Sort("totalCost", decreasing = FALSE)
#' print(acme, "totalCost")
#' 
#' @seealso \code{\link{Node}}
#' @keywords internal
Sort <- function(node, attribute, ..., decreasing = FALSE, recursive = TRUE) {
  if (node$isLeaf) return()
  ChildL <- sapply(node$children, function(x) GetAttribute(x, attribute, ...))
  names(ChildL) <- names(node$children)
  node$children <- node$children[names(sort(ChildL, decreasing = decreasing, na.last = TRUE))]
  if (recursive) for(child in node$children) Sort(child, attribute, ..., decreasing = decreasing, recursive = recursive)
  invisible (node)
}


#' Reverts the sort order of a \code{Node}'s children.
#' 
#' @param node the Node whose children's sort order is to be reverted
#' @param recursive If \code{TRUE}, then revert is called recursively on
#' all children.
#' 
#' @return returns the Nodel invisibly (for chaining)
#'
#' @seealso \code{\link{Node}}
#' @keywords internal
Revert <- function(node, recursive = TRUE) {
  
  pf <- function(x) {
    if (recursive) return (TRUE)
    else return (x$level <= (node$level + 1))
  }
  
  t <- Traverse(node, pruneFun = pf)
  
  Set(t, tmp = 1:node$totalCount)
  Sort(node, "tmp", decreasing = TRUE, recursive = recursive)
  Do(t, function(x) rm("tmp", envir = x))
  invisible (node)
}


#' Prunes a tree
#' 
#' @param The node whose children should be pruned
#' @param pruneFun a function taking a \code{\link{Node}} as an argument, and returning TRUE if the Node
#' and its descendants should be kept, FALSE otherwise.
#' @return A Node
#' @keywords internal
Prune <- function(node, pruneFun) { 
  
  if ( self$isLeaf) return()
  for( i in length(self$children):1 ) {
    if ( !pruneFun(self$children[[i]]) ) {
      self$children <- self$children[-i]
    }
  }
  for( child in self$children) {
    Prune(child, pruneFun)
  }
  
}
