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







#' Sort children of a \code{Node} or an entire \code{data.tree} structure
#' 
#' You can sort with respect to any argument of the tree. But note that sorting has
#' side-effects, meaning that you modify the underlying, original data.tree object structure.
#' 
#' @usage # node$Sort(attribute, ..., decreasing = FALSE, recursive = TRUE) 
#' Sort(node, attribute, ..., decreasing = FALSE, recursive = TRUE)
#' 
#' @param node The node whose children are to be sorted 
#' @param ... any parameters to be passed on the the attribute (in case it's a method or a 
#' function)
#' @param decreasing sort order
#' @param recursive if \code{TRUE}, Sort will be called recursively on the \code{Node}'s children. 
#' This allows sorting an entire tree.
#' 
#' @inheritParams Get
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
#' @seealso \code{\link{Revert}}
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
#' @usage # node$Revert(recursive = TRUE)
#' Revert(node, recursive = TRUE)
#' 
#' @param node the Node whose children's sort order is to be reverted
#' @param recursive If \code{TRUE}, then revert is called recursively on
#' all children.
#' 
#' @return returns the Nodel invisibly (for chaining)
#'
#' @seealso \code{\link{Node}}
#' @seealso \code{\link{Sort}}
#' @keywords internal
Revert <- function(node, recursive = TRUE) {
  
  pf <- function(x) {
    if (recursive) return (TRUE)
    else return (x$level <= (node$level + 1))
  }
  
  t <- Traverse(node, pruneFun = pf)
  
  Set(t, .tmp = 1:node$totalCount)
  Sort(node, ".tmp", decreasing = TRUE, recursive = recursive)
  Do(t, function(x) rm(".tmp", envir = x))
  invisible (node)
}


#' Prunes a tree. 
#' 
#' Pruning refers to removing entire subtrees. This function has side-effects, it modifies your data.tree structure!
#' 
#' @usage # node$Prune(pruneFun)
#' Prune(node, pruneFun)
#' 
#' @param node The root of the sub-tree to be pruned
#' @param pruneFun allows providing a a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
#' If the pruneFun returns FALSE for a Node, then the Node and its entire sub-tree will not be considered.
#' @return the number of nodes removed
#' 
#' @examples
#' data(acme)
#' acme$Do(function(x) x$cost <- Aggregate(x, "cost", sum))
#' acme$Prune(function(x) x$cost > 700000)
#' print(acme, "cost")
#' 
#' @seealso \code{\link{Node}}
#' 
#' @keywords internal
Prune <- function(node, pruneFun) { 
  return (.Prune(node, pruneFun, TRUE))
}


.Prune <- function(node, pruneFun, isFirstCall = FALSE) { 
  if (isFirstCall) cnt <- node$totalCount
  if ( node$isLeaf) return (0)
  for( i in length(node$children):1 ) {
    if ( !pruneFun(node$children[[i]]) ) {
      rm(list = names(node$children)[i], envir = node)
      node$children <- node$children[-i]
    }
  }
  for( child in node$children) {
    .Prune(child, pruneFun)
  }
  if (isFirstCall) return (cnt - node$totalCount)
}

