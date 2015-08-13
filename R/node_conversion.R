


#' Convert an object to a \code{data.tree} data structure
#' 
#' @param x The object to be converted
#' @param ... Additional arguments
#' 
#' @family as.Node
#' 
#' @export
as.Node <- function(x, ...) {
  UseMethod("as.Node")
}


  


#' Write a \code{data.tree} structure to Newick notation
#' 
#' To read from Newick, you can use the \code{ape} package, and convert the resulting \code{phylo}
#' object to a \code{data.tree} structure.
#' 
#' @param node The root \code{Node} of a tree or sub-tree to be converted
#' @param heightAttribute The attribute (field name, method, or function) storing or calculating the height for each \code{Node}
#' @param ... parameters that will be passed on the the heightAttributeName, in case it is a function
#' 
#' @import stringr
#' 
#' @examples
#' data(acme)
#' ToNewick(acme)
#' ToNewick(acme, heightAttribute = NULL)
#' ToNewick(acme, heightAttribute = function(x) DefaultPlotHeight(x, 200))
#' ToNewick(acme, rootHeight = 200)
#' 
#' @family Conversions from Node
#' 
#' @keywords Newick
#' 
#' @export 
ToNewick <- function(node, heightAttribute = DefaultPlotHeight, ...) {

  deparse <- function(x) {
    name <- str_replace_all(x$name, " ", "_")
    name <- str_replace_all(name, ",", "")
    if(!x$isRoot && length(heightAttribute) > 0) {
      edge <- GetAttribute(x$parent, heightAttribute, ...) - GetAttribute(x, heightAttribute, ...) 
      me <- paste0(name, ":", edge)
    } else {
      me <- name
    }
    return(me)
  }
  
  Newick <- function(x) {
    if(x$isLeaf) {
      return (deparse(x))
    }
    chNewick <- sapply(x$children, Newick)
    chNewickStr <- paste(chNewick, collapse = ",")
    res <- paste0("(", chNewickStr, ")", deparse(x))
  }
  
  res <- Newick(node)
  res <- paste0(res, ";")
  return (res)
  
}

