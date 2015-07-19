#' Checks if a node is the root
#' @param node The Node to test.
#' @return TRUE if the Node is the root, FALSE otherwise
#' @export
isRoot <- function(node) {
  is.null(node$parent)
}


#' Checks if a node is not a root
#' @param node The Node to test.
#' @return FALSE if the Node is the root, TRUE otherwise
#' @export
isNotRoot <- function(node) {
  !isRoot(node)
}

#' Checks if a node is a leaf
#' @param node The Node to test.
#' @return TRUE if the Node is a leaf, FALSE otherwise
#' @export
isLeaf <- function(node) {
  length(node$children) == 0
}

#' Checks if a node is not a leaf
#' @param node The Node to test.
#' @return FALSE if the Node is a leaf, TRUE otherwise
#' @export
isNotLeaf <- function(node) {
  !isLeaf(node)
}