
#' Check if a \code{Node} is the root
#' 
#' @param node The Node to test.
#' @return TRUE if the Node is the root, FALSE otherwise
#' @export
isRoot <- function(node) {
  is.null(node$parent)
}


#' Check if a \code{Node} is not a root
#' 
#' @param node The Node to test.
#' @return FALSE if the Node is the root, TRUE otherwise
#' @export
isNotRoot <- function(node) {
  !isRoot(node)
}

#' Check if a \code{Node} is a leaf
#' 
#' @param node The Node to test.
#' @return TRUE if the Node is a leaf, FALSE otherwise
#' @export
isLeaf <- function(node) {
  length(node$children) == 0
}

#' Check if a \code{Node} is not a leaf
#' 
#' @param node The Node to test.
#' @return FALSE if the Node is a leaf, TRUE otherwise
#' @export
isNotLeaf <- function(node) {
  !isLeaf(node)
}