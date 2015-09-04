

#' Convert a \code{\link{dendrogram}} to a data.tree \code{Node}
#' 
#' @param x The dendrogram
#' @param name The name of the root Node
#' @param heightName The name under which the dendrogram's height is stored
#' @param ... Additional parameters
#' 
#' @return The root \code{Node} of a \code{data.tree}
#' 
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' dend1 <- as.dendrogram(hc)
#' tree1 <- as.Node(dend1)
#' tree1$fieldsAll
#' tree1$totalCount
#' tree1$leafCount
#' tree1$height
#'   
#' @family as.Node
#' 
#' @export
as.Node.dendrogram <- function(x, name = "root", heightName = "plotHeight", ...) {
  #str(unclass(dend1))
  if (is.leaf(x)) {
    name <- attr(x, 'label')
  } else if(is.null(name) && is.null(attr(x, "edgetext"))) {
    name <- tempfile(pattern = '', tmpdir = '')
  } else if(!is.null(attr(x, "edgetext"))) {
    name <- attr(x, "edgetext")
  }
  
  n <- Node$new(name)
  reserved <- c('label', 'class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp', NODE_RESERVED_NAMES_CONST)
  ats <- names(attributes(x))
  for (a in ats[!(ats %in% reserved)]) {
    n[[a]] <- attr(x, a)
  }
  n[[heightName]] <- attr(x, "height")
  
  if (!is.leaf(x)) {
    for (i in 1:length(x)) {
      childNode <- as.Node.dendrogram(x[[i]], name = NULL, ...)
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


#' Convert a \code{Node} to a \code{dendrogram}
#' 
#' Convert a \code{data.tree} structure to a \code{\link{dendrogram}}
#' 
#' @param object The Node to convert
#' @param heightAttribute The attribute (field name or function) storing the height
#' @param edgetext If TRUE, then the for non-leaf nodes the node name is stored as the dendrogram's edge text.
#' @param ... Additional parameters
#' 
#' @return An object of class dendrogram
#' 
#' @examples
#' data(acme)
#' acmed <- as.dendrogram(acme)
#' plot(acmed, center = TRUE)
#' 
#' #you can take an attribute for the height:
#' acme$Do( function(x) x$myPlotHeight <- (10 - x$level))
#' acmed <- as.dendrogram(acme, heightAttribute = "myPlotHeight")
#' plot(acmed, center = TRUE)
#' 
#' #or directly a function
#' acmed <- as.dendrogram(acme, heightAttribute = function(x) 10 - x$level)
#' plot(acmed)
#' 
#' @family Conversions from Node
#'
#' @import stats
#' @export
as.dendrogram.Node <- function(object, heightAttribute = DefaultPlotHeight, edgetext = FALSE, ...) {
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
  
  height <- as.vector(GetAttribute(node, heightAttribute))

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
    res <- unname(lapply(node$children, FUN = function(x) as.dendrogram(x, heightAttribute, ...)))
    res <- structure(res, 
                     members = node$leafCount,
                     midpoint = node$midpoint,
                     height = height,
                     class = "dendrogram")
    
    if (edgetext) attr(res, "edgetext") <- node$name
    
  }
  
  return (res)
  
}
