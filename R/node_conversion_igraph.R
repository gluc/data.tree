
#' Convert a \code{data.tree} structure to an igraph network
#' 
#' This requires the igraph package to be installed.
#' Also, this requires the names of the \code{Nodes} to be unique within
#' the \code{data.tree} structure.
#' 
#' @param x The root \code{Node} to convert
#' @param vertexAttributes A vector of strings, representing the attributes 
#' in the \code{data.tree} structure to add as attributes to the vertices of the igraph
#' @param edgeAttributes A vector of strings, representing the attributes
#' in the \code{data.tree} structure to add as edge attributes of the igraph
#' @param ... Currently unused.
#' 
#' @inheritParams igraph::graph_from_data_frame
#' @inheritParams ToDataFrameNetwork
#'
#' @return an \code{igraph} object
#'   
#' @examples
#' data(acme)
#' library(igraph)
#' ig <- as.igraph(acme, "p", c("level", "isLeaf"))
#' plot(ig)
#' 
#' @seealso AreNamesUnique
#' 
#' @export
as.igraph.Node <- function(x, vertexAttributes = character(), edgeAttributes = character(), directed = FALSE, direction = c("climb", "descend"), ...) {
  if (!AreNamesUnique(x)) stop("Node names must be unique within the tree")
  network <- do.call("ToDataFrameNetwork", c(x, "name", vertexAttributes, edgeAttributes, direction = direction))
  data <- network[,c("from", "to", edgeAttributes)]
  vert <- do.call("ToDataFrameTree", c(x, "name", vertexAttributes))[,-1]
  ig <- igraph::graph_from_data_frame(data, 
                                      directed = directed,
                                      vertices = vert)
  return (ig)
}