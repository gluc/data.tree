

#' Convert a \code{Node} to a phylo object from the ape package.
#' 
#' This method requires the ape package to be installed and loaded.
#' 
#' @param x The root \code{Node} of the tree or sub-tree to be converted
#' @param heightAttribute The attribute (field name or function) storing the height
#' @param ... any other argument
#' 
#' @examples
#' library(ape)
#' data(acme)
#' acmephylo <- as.phylo(acme)
#' plot(acmephylo)
#' 
#' 
#' @family ape phylo conversions
#' 
#' @export
as.phylo.Node <- function(x, heightAttribute = DefaultPlotHeight, ...) {
  txt <- ToNewick(x, heightAttribute)
  return (ape::read.tree(text = txt))
}


#' Convert a \code{phylo} object from the ape package to a \code{Node}
#' 
#' @param x The phylo object to be converted
#' @param heightName If the phylo contains edge lengths, then they will be converted
#' to a height and stored in a field named according to this parameter (the default is "height")
#' @param replaceUnderscores if TRUE (the default), then underscores in names are replaced with spaces
#' @param ... any other parameter to be passed to sub-implementations
#' 
#' @examples
#' #which bird familes have the max height?
#' library(ape)
#' data(bird.families)
#' bf <- as.Node(bird.families)
#' height <- bf$height
#' t <- Traverse(bf, filterFun = function(x) x$level == 25)
#' Get(t, "name")
#' 
#' @family ape phylo conversions
#' @family as.Node
#' 
#' @export
as.Node.phylo <- function(x, heightName = "plotHeight", replaceUnderscores = TRUE, ...) {
  
  #find root node
  rootNr <- unique(x$edge[,1][!x$edge[,1] %in% x$edge[,2]])
  
  #names
  nodeNrs <- c(rootNr, unique(x$edge[,2]))
  leafNrs <- 1:length(x$tip.label)
  nms <- x$tip.label
  names(nms) <- leafNrs
  if("node.label" %in% names(x)) {
    nms2 <- x$node.label
  } else {
    nms2 <- (max(leafNrs) + 1):max(nodeNrs)
  }
  names(nms2) <- (max(leafNrs) + 1):max(nodeNrs)
  nms <- c(nms2, nms)
  root <- Node$new(rootNr)
  for (i in 1:nrow(x$edge)) {
    e <- x$edge[i,]
    fifu <- function(x) x$name == as.character(e[1])
    parent <- Traverse(root, filterFun = fifu)[[1]]
    child <- parent$AddChild(as.character(e[2]))
  }
  if (length(x$edge.length) > 0) {
    t <- Traverse(root, filterFun = isNotRoot)
    Set(t, edgeLength = x$edge.length)
    #try converting edge length to height
    root[[heightName]] <- 0
    ehf <- function(x) x[[heightName]] <- x$parent[[heightName]] - x$edgeLength 
    Do(t, ehf)
    corr <- min(Get(t, heightName))
    root$Do(function(x) x[[heightName]] <- x[[heightName]] - corr)
    Do(t, function(x) rm("edgeLength", envir = x))
  }
  
  setName <- function(x) {
    if(replaceUnderscores) nm <- str_replace_all( nms[[x$name]], "_", " ")
    else nm <- nms[[x$name]]
    x$name <- nm
  }
  root$Do(setName)
  
  return (root)
  
}




#' Determine the number a \code{Node} has after conversion to a phylo object
#' 
#' Use this function when plotting a Node as a phylo, e.g. to set custom
#' labels to plot.
#' 
#' @param x The Node
#' @param type Either "node" (the default) or "edge" (to get the number of the edge from \code{x} to its parent)
#' @return an integer representing the node
#' 
#' @examples
#' library(ape)
#' library(data.tree)
#' data(acme)
#' ap <- as.phylo(acme)
#' plot(ap)
#' nodelabels("IT Dep.", GetPhyloNr(acme$Climb("IT")))
#' edgelabels("Good!", GetPhyloNr(acme$Climb("IT", "Switch to R"), "edge"))
#' 
#' @family ape phylo conversions
#' 
#' @export
GetPhyloNr <- function(x, type = c("node", "edge")) {
  type <- type[1]
  if (type == "node") {
    t <- c(Traverse(x$root, filterFun = isLeaf), Traverse(x$root, filterFun = isNotLeaf))    
  } else if (type == "edge") {
    t <- Traverse(x$root, filterFun = isNotRoot)
  } else {
    stop("Only node or edge allowed as type")
  }
  res <- which(sapply(t, function(z) identical(z, x)))
  return (res)
}
