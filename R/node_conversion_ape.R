

#' Convert a node to a phylo object from the ape package.
#' This requires the ape package.
#' 
#' @param x The \code{Node} to convert
#' @param heightAttributeName To use custom heights
#' @param ... any other argument
#' 
#' @export
as.phylo.Node <- function(x, heightAttributeName = "Height", ...) {
  txt <- ToNewick(x, heightAttributeName)
  return (ape::read.tree(text = txt))
}


#' Converts a phylo from the ape package to a Node
#' 
#' @param x The phylo object
#' @param heightName If the phylo contains edge lengths, then they will be stored in 
#' an attribute according to this parameter (the default is "edgeLength")
#' @param replaceUnderscores if TRUE (the default), then underscores in names are replaced with spaces
#' @param ... any other parameter to be passed to sub-implementations
#' 
#' @export
as.Node.phylo <- function(x, heightName = "height", replaceUnderscores = TRUE, ...) {
  
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
    root$Set(edgeLength = x$edge.length, filterFun = function(x) !x$isRoot)
    #try converting edge length to height
    root[[heightName]] <- 0
    ehf <- function(x) x$parent[[heightName]] - x$edgeLength 
    corr <- min(root$Get(ehf, filterFun = function(x) !x$isRoot, assign = heightName))
    root$Get(function(x) x[[heightName]] - corr, assign = heightName)
    root$Get(function(x) rm("edgeLength", envir = x), filterFun = function(x) !x$isRoot)
  }
  getName <- function(x) {
    if(replaceUnderscores) str_replace_all( nms[[x$name]], "_", " ")
    else nms[[x$name]]
  }
  root$Get(getName, assign = "name")
  
  return (root)
  
}


#' Get a Phylo Label for a single Node
#' @param x The Node
#' @param labelFun The function that compiles the label
#' @param type either "node" (the default) or "edge"
#' @param ... any argument to be passed to the labelFun
#' @return a string that can be used as a Phylo Node label
#' @export
GetPhyloLabel <- function(x, labelFun, type = c("node", "edge"), ...) {
  
  label <- labelFun(x, ...)
  nodeNr <- GetPhyloNr(x, type)
  
  attr(label, "nodes") <- nodeNr
  
  return (label)
}

#' Gets the node nr in phylo context
#' 
#' @param x The Node
#' @param type Either "node" (the default) or "edge"
#' @return an integer representing the node
#' @export
GetPhyloNr <- function(x, type = c("node", "edge")) {
  type <- type[1]
  if (type == "node") {
    if (x$isLeaf) {
      leaves <- x$root$leaves
      for (i in 1:length(leaves)) leaves[[i]]$tmp <- i 
    } else {
      n <- x$root$totalCount - x$root$leafCount
      x$root$Set(tmp = x$root$leafCount + (1:n), filterFun = function(x) !x$isLeaf)
    }
  } else {
    x$root$Set(tmp = (1:x$root$totalCount) - 1)
  }
  res <- x$tmp
  x$root$Set("tmp", NULL)
  return (res)
}
