#
# These are the methods that would normally sit on Node
# However, to reduce the memory footprint of the Node object,
# we only support traditional R methods.
# The first argument of all these methods is node




#' Aggregate child values of a \code{Node}, standalone or in traversal.
#' 
#' The \code{Aggregate} method lets you fetch an attribute from a \code{Node}'s children, and then aggregate it.
#' For example, you can aggregate cost by summing costs of child \code{Nodes}. This is especially useful in the 
#' context of tree traversal, when using post-order traversal mode.
#' 
#' As with \code{\link{Get}}, the attribute can be a field, a method or a function. If it is a field, and if
#' the node contains the attribute, its value is returned. 
#' Otherwise, \code{aggFun(Aggregate(children, ...))} is called. In that case, 
#' the attribute must be set on the leaf.
#' 
#' @param node the \code{Node} on which to aggregate
#' @param aggFun the aggregation function to be applied to the children's \code{attributes}
#' @param ... any arguments to be passed on to aggFun
#'
#' @inheritParams Get
#'   
#' @examples
#' data(acme)
#' 
#' #Aggregate on a field
#' Aggregate(acme, "cost", sum)
#' 
#' #Aggregate on a function
#' print(acme, "cost", minCost = acme$Get(Aggregate, "cost", min))
#' 
#' #use Aggregate in traversal:
#' acme$Do(function(x) x$cost <- Aggregate(x, "cost", sum), traversal = "post-order")
#' 
#' 
#' @seealso \code{\link{Node}}
#'
#' @export
Aggregate = function(node, attribute, aggFun, ...) {
  #if(is.function(attribute)) browser()
  #if (!is.function(attribute)) {
    v <- GetAttribute(node, attribute, ..., nullAsNa = FALSE)
    if (!length(v) == 0) {
      return (v)
    }
    if (node$isLeaf) stop(paste0("Attribute returns NULL on leaf!"))
  #}
  values <- sapply(node$children, function(x) Aggregate(x, attribute, aggFun, ...))
  result <- aggFun(values)
  return (result)
}

#' Clones a tree (creates a deep copy)
#' 
#' @param node the root node of the tree or sub-tree to clone
#' @return the clone of the tree
#' 
#' @examples
#' data(acme)
#' 
#' @export
Clone <- function(node) {
  clone <- node$clone()
  filterFun <- function(x) {
    length(attributes(x)) > 1
  }
  t <- Traverse(node, filterFun = filterFun)
  as <- lapply(t, function(x) attributes(x))
  names(as) <- Get(t, "pathString")
  tc <- Traverse(clone, filterFun = function(x) x$pathString %in% names(as))
  Do(tc, function(x) attributes(x) <- as[[x$pathString]])
  return (clone)
}




#' Find a \code{Node} by its path
#' 
#' 
#' This method lets you climb the tree, from crutch to crutch. More precisely,
#' \code{Climb} returns the \code{Node} at path \code{...}. The path is relative to the \code{Node} on which this method is called. Each argument provided corresponds to an 
#' element in the path, specified by the \code{Node}'s name. 
#' 
#' @param node The root node
#' @param ... the names of the nodes in the path
#' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
#' 
#' @examples
#' data(acme)
#' acme$Climb('IT', 'Outsource')$name
#' acme$Climb('IT')$Climb('Outsource')$name
#'
#' @seealso \code{\link{Node}}
#'
#' @export
Climb <- function(node, ...) {
  
  path <- as.character(list(...))
  if (length(path) == 0) {
    return (node)
  } else {
    child <- node$children[[path[1]]]
    if (is.null(child)) {
      return (NULL)
    } else if (length(path) == 1) {
      return (child)
    } else {
      return (do.call(Climb, c(node = child, list(...)[-1])))
    }
  }
  
}


###############################
## Private Methods

GetAttribute <- function(node, attribute, ..., format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
  if(is.function(attribute)) {
    #function
    
    v <- attribute(node, ...)
  } else if(is.character(attribute) && length(attribute) == 1) {
    #property
    v <- node[[attribute]]
    if (is.function(v)) v <- v(...)
  } else {
    stop("attribute must be a function, the name of a public property, or the name of method")
  }
  
  if(length(v) == 0 && inheritFromAncestors && !node$isRoot) {
    v <- GetAttribute(node$parent, attribute, 
                      ..., 
                      inheritFromAncestors = TRUE,
                      nullAsNa = FALSE)
  }
  
  if (!nullAsNa && length(v) == 0) return (NULL)
  if (length(v) == 0) v <- NA
  if (length(v) == 0) v <- NA
  
  if(is.vector(v)) names(v) <- node$name
  
  if(is.null(format) && !is.function(attribute)) {
    format <- GetObjectAttribute(node, "formatters")[[attribute]]
  }
  
  if(!is.null(format)) {
    if (!is.function(format)) stop("format must be a function!")
    v <- format(v)
  }
  return (v)
}

GetObjectAttribute <- function(node, name) {
  a <- attr(node, name)
  if (length(a) > 0 || node$isRoot) return (a)
  return ( GetObjectAttribute(node$parent, name))
}

#' Set a formatter function on a specific node
#' 
#' @param node The node on which to set the formatter
#' @param name The attribute name for which to set the formatter
#' @param formatFun The formatter, i.e. a function taking a value as an input, and formatting
#' returning the formatted value
#' 
#' @export
SetFormat <- function(node, name, formatFun) {
  if (length(attr(node, "formatters")) == 0) attr(node, "formatters") <- list()
  attr(node, "formatters")[[name]] <- formatFun
}