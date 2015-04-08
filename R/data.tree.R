#' Manage Hierarchical Data in R
#' 
#' \code{data.tree} is to hierarchical data what `data.frame` is to tabular data: An extensible, general purpose structure to store, manipulate, 
#' and display hierarchical data.
#' 
#' Hierarchical data is ubiquitous. However, no general-use \bold{tree data structure} is available in R. 
#' Where tabular data has \code{data.frame}, hierarchical data is often modeled in lists of lists, or even uglier makeshifts.
#' This is where the \code{data.tree} package steps in. It lets you build trees of hierarchical
#' data for various uses: to print, to rapid prototype search algorithms, to test out new classification algorithms, and much more. 
#' \code{data.tree} allows traversing the trees in various orders (pre-order, in-order, etc.), and it lets you run operations on \code{\link{Node}s}. Conversely, you can collect data.
#' The package also contains utility functions to sort, to print, and to construct a tree from a \code{data.frame} and vice versa.
#' The entry point to the package is \code{\link{Node}}. 
#' 
#' One of most important things to note about `data.tree` is that it exhibits \bold{reference semantics}. In a nutshell, this means that you can modify 
#' your tree along the way, without having to reassing it to a variable after each modification. By and large, this is a rather exceptional behaviour
#' in R, which is following value-semantics most of the time.
#' 
#' \code{data.frame} is not optimised for computation speed, but for implementation speed. As a result, it is useful always when implementation speed
#' is more important than computation speed, which is usually the case when
#' - you want to develop and test a new algorithm
#' - you don't expect large data sets
#' - you just want to play around with data
#' - you want to test another package, to compare it with your own results
#' - you need to do homework
#' 
#' For a quick overview of the features, read the \code{\link{data.tree}} vignette by running \code{vignette("data.tree")}. For an example 
#' in the area of classification trees, you might also be interested in \code{vignette("ID3")}
#'
#' @seealso \code{\link{Node}}
#' @seealso For more details see the \code{data.tree} vignette by running: \code{vignette("data.tree")}
#' @docType package
#' @name data.tree
NULL