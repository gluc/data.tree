


#' Convert a \code{data.tree} structure to an igraph network
#' 
#' This requires the igraph package to be installed.
#' Also, this requires the names of the \code{Nodes} to be unique withing
#' the \code{data.tree} structure.
#' 
#' @param node The root \code{Node} to convert
#' @param vertexAttributes A vector of strings, representing the attributes 
#' in the \code{data.tree} structure to add as attributes to the vertices of the igraph
#' @param edgeAttributes A vector of strings, representing the attributes
#' in the \code{data.tree} structure to add as edge attributes of the igraph
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
#' 
#' @export
as.igraph.Node <- function(node, vertexAttributes, edgeAttributes) {
  if (!AreNamesUnique(node)) stop("Node names must be unique withing the tree")
  taxonomy <- do.call("ToDataFrameTaxonomy", c(node, "name", vertexAttributes, edgeAttributes)) 
  data <- taxonomy[,c("children", "parents", edgeAttributes)]
  vert <- do.call("ToDataFrameTree", c(node, "name", vertexAttributes))[,-1]
  ig <- igraph::graph_from_data_frame(data, 
                                      directed = FALSE,
                                      vertices = vert)
  return (ig)
}

#not a god idea -> too complex, as we don't know if 
#it's a tree, cycles, etc.
as.Node.igraph <- function(igraph) {
  res <- igraph::as_data_frame(igraph, what = "both")
  edges <- res$edges
  edges <- edges[,c(2, 1, 3:ncol(edges))]
  colnames(edges)[1:3] <- c("children", "parents", "level")
  n <- FromDataFrameTaxonomy(edges)
  vert <- res$vertices
  for (i in 1:dim(vert)[1]) {
    vals <- vert[i,, drop = FALSE]
    nd <- Traverse(n, filterFun = function(x) x$name == rownames(vals))[[1]]
    for (nm in names(vals)) {
      if(!nm %in% NODE_RESERVED_NAMES_CONST) {
        nd[[nm]] <- vals[[nm]]
      }
    }
  }
}