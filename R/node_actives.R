
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



changeName <- function(node, oldName, newName) {
  if(!node$isRoot) {
    rm(list = oldName, envir = node$parent)
    names(node$parent$children)[node$position] <- newName
    node$parent[[as.character(newName)]] <- node
  }
  return (newName)
}


#' @export
.separator <- function(self) {
  if (self$isRoot) return("")
  if (self$position == self$parent$count) mySeparator <- paste0(" ", "\u00B0", "--") 
  else mySeparator <- paste0(" ", "\u00A6", "--")
  return (paste0(.parentSeparator(self$parent), mySeparator))
}

#' @export
.parentSeparator <- function(self) {
  if (self$isRoot) return("")
  if (self$position == self$parent$count) mySeparator <- "    "
  else mySeparator <- paste0(" ", "\u00A6", "  ")
  paste0(.parentSeparator(self$parent), mySeparator)
  
}

#' Calculate the average number of branches each non-leaf has
#' 
#' @param node The node
#' @export
averageBranchingFactor <- function(node) {
  t <- Traverse(node, filterFun = isNotLeaf)
  if (length(t) == 0) return (0)
  cnt <- Get(t, "count")
  if (!is.numeric(cnt)) browser()
  return (mean(cnt))
}

