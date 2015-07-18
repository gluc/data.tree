

#' Convert a \code{\link{dendrogram}} to a data.tree \code{Node}
#' 
#' @param x The dendrogram
#' @param name The name of the root Node
#' @param ... Additional parameters
#' 
#' @return The root \code{Node} of a \code{data.tree}
#' @export
as.Node.dendrogram <- function(x, name = "root", ...) {
  #str(unclass(dend1))
  if (is.leaf(x)) {
    name <- attr(x, 'label')
  } else if(is.null(name)) {
    name <- tempfile(pattern = '', tmpdir = '')
  }
  
  n <- Node$new(name)
  reserved <- c('label', 'class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  for (a in names(attributes(x))[!(attributes(x) %in% NODE_RESERVED_NAMES_CONST) && !(attributes(x) %in% reserved)]) {
    n[[a]] <- attr(x, a)
  }
  
  if (!is.leaf(x)) {
    for (i in 1:length(x)) {
      childNode <- as.Node(x[[i]], name = NULL, ...)
      n$AddChildNode(childNode)
      if(!is.leaf(x[[i]])) {
        name <- as.character(childNode$position)
        childNode$name <- name
      }
    }
  } else {
    n$value <- as.vector(x)
  }
  return (n)
  
}


#' Convert a Node to a dendrogram
#' 
#' @details Convert a Node to a dendrogram
#' 
#' @param object The Node to convert
#' @param ... Additional parameters
#' 
#' @import stats
#' @export
as.dendrogram.Node <- function(object, ...) {
  node <- object
  
  #strange: the original dendrogram will
  # unclass the nested dendrograms as well,
  # while ours won't?
  #
  # hc <- hclust(dist(USArrests), "ave")
  # dend1 <- as.dendrogram(hc)
  # node <- as.Node(dend1)
  # dend2 <- as.dendrogram(node)
  # unclass(dend1)
  # unclass(dend2)
  
  height <- node$height
  if(length(height) == 0) {
    height <- Height(node, 100)
  }
  
  if (node$isLeaf) {
    res <- node$value
    res <- structure(res, 
                     label = node$name, 
                     members = 1,
                     height = height,
                     leaf = node$isLeaf,
                     class = "dendrogram")
    
  } else {
    #res <- list()
    #class(res) <- "dendrogram"
    res <- unname(lapply(node$children, FUN = function(x) as.dendrogram(x, ...)))
    res <- structure(res, 
                     members = node$leafCount,
                     midpoint = node$midpoint,
                     height = height,
                     class = "dendrogram")
    
  }
  
  return (res)
  
}
