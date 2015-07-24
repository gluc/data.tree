#' data.tree: Hierarchical Data Structures
#' 
#' \code{data.tree} is to hierarchical data what \code{data.frame} is to tabular data: An extensible, general purpose structure to store, manipulate, 
#' and display hierarchical data.
#' 
#' @section Introduction:
#' 
#' Hierarchical data is ubiquitous (XML, search trees, family trees, classification, file system, etc.). However, no general-use \bold{tree data structure} is available in R. 
#' Where tabular data has \code{data.frame}, hierarchical data is often modeled in lists of lists or similar makeshifts. These
#' structures are often dificult to manage.
#' This is where the \code{data.tree} package steps in. It lets you build trees of hierarchical
#' data for various uses: to print, to rapid prototype search algorithms, to test out new classification algorithms, and much more. 
#' 
#' @section Tree Traversal:
#' 
#' \code{data.tree} allows to \code{\link{Traverse}} trees in various orders (pre-order, post-order, level, etc.), and it lets you run operations on \code{\link{Node}s} via
#' \code{\link{Do}}. 
#' Similarly, you can collect and store data while traversing a tree using the \code{\link{Get}} and the \code{\link{Set}} methods.
#' 
#' @section Methods:
#' 
#' The package also contains utility functions to \code{\link{Sort}}, to \code{\link{Prune}}, and to \code{\link{print}} in custom formats.
#' 
#' @section Construction and Conversion:
#' 
#' You can construct a tree from a \code{data.frame} using \code{\link{as.Node.data.frame}}, and convert it back using \code{\link{as.data.frame.Node}}.
#' Similar options exist for list of lists. 
#' For more specialized conversions, see \code{\link{as.dendrogram.Node}}, \code{\link{as.Node.dendrogram}}, 
#' \code{\link{as.phylo.Node}} and \code{\link{as.Node.phylo}}
#' 
#' Finally, easy conversion options from and to JSON, YAML, igraph, and more exist.
#'  
#' @section Node and Reference Semantics:
#'  
#' The entry point to the package is \code{\link{Node}}. Each tree is composed of a number of \code{Node}s, referencing each other.
#' 
#' One of most important things to note about \code{data.tree} is that it exhibits \bold{reference semantics}. In a nutshell, this means that you can modify 
#' your tree along the way, without having to reassing it to a variable after each modification. By and large, this is a rather exceptional behaviour
#' in R, where value-semantics is king most of the time.
#' 
#' @section Applications:
#' 
#' \code{data.tree} is not optimised for computational speed, but for implementation speed. However, it can easily handle trees with
#' several thousand nodes. It is always useful when
#' \itemize{
#'  \item{you want to develop and test a new algorithm}
#'  \item{you want to import and convert tree structures (it imports and exports to list-of-list, data.frame, yaml, json, igraph, dendrogram, phylo and more)}
#'  \item{you want to play around with data, display it and get an understanding}
#'  \item{you want to test another package, to compare it with your own results}
#'  \item{you need to do homework}
#' }
#' 
#' For a quick overview of the features, read the \code{\link{data.tree}} vignette by running \code{vignette("data.tree")}. For a stylized
#' application in algorithm development, read \code{vignette("ID3")}
#'
#' @examples
#' data(acme)
#' print(acme)
#' acme$fieldsAll
#' acme$count
#' acme$totalCount
#' acme$isRoot
#' acme$depth
#' print(acme, "p", "cost")
#' 
#' outsource <- acme$Climb("IT", "Outsource")
#' class(outsource)
#' print(outsource)
#' outsource$fields
#' outsource$isLeaf
#' outsource$level
#' outsource$path
#' outsource$p
#' outsource$parent$name
#' outsource$root$name
#' outsource$expCost <- outsource$p * outsource$cost
#' print(acme, "expCost")
#' 
#' acme$Get("p")
#' acme$Do(function(x) x$expCost <- x$p * x$cost)
#' acme$Get("expCost", filterFun = isLeaf)
#' 
#' ToDataFrameTable(acme, "name", "p", "cost", "level", "pathString")
#' ToDataFrameTree(acme, "name", "p", "cost", "level")
#' ToDataFrameTaxonomy(acme, "p", "cost")
#' 
#' 
#' @seealso \code{\link{Node}}
#' @seealso For more details, see the \code{data.tree} vignette by running: \code{vignette("data.tree")}
#' @docType package
#' @name data.tree
NULL