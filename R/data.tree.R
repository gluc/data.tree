#' data.tree hierarchical data for R
#' 
#' Hierarchical data is ubiquitous. However, no general-use *tree data structure* is available in R. 
#' Where tabular data has \code{data.frame}, hierarchical data is often modeled in lists of lists, or even uglier makeshifts.
#' This is where the \code{data.tree} package steps in. It lets you build trees of hierarchical
#' data for various uses: to print, to rapid prototype search algorithms, to test out new classification algorithms, and much more. 
#' \code{data.tree} allows traversing the trees in various orders, and it lets you run operations on \code{\link{Node}s}. Conversely, you can collect data.
#' The package also contains utility functions to sort, to print, and to construct a tree from a \code{data.frame} and vice versa.
#' The entry point to the package is \code{\link{Node}}. 
#' \code{data.frame} is not optimised for computation speed, but for implementation speed. It is designed to cater many different use cases and is fully
#' extensible.
#' For a quick overview of the features, read the \code{\link{data.tree}} vignette by running \code{vignette("data.tree")}. For an example 
#' in the area of classification, you might be interested in \code{vignette("ID3")}
#'
#' @seealso \code{\link{Node}}
#' @seealso For more details see the \code{data.tree} vignette by running: \code{vignette("data.tree")}
#' @docType package
#' @name data.tree
NULL