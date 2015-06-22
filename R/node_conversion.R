
#' Print a Node
#' 
#' Print a Node in a human-readable fashion.
#' 
#' @param x The Node
#' @param ... Additional parameters
#' 
#' @details Print the Node in a human-readable fashion.
#'
#' @export
print.Node <- function(x, ...) {
  print(as.data.frame(x, row.names = NULL, optional = FALSE, ...), na.print = "")
}


#' Convert an object to a Node
#' 
#' @param x The object to be converted
#' @param ... Additional arguments
#' 
#' @details Convert an object to a Node
#' 
#' @export
as.Node <- function(x, ...) {
  UseMethod("as.Node")
}


#' Converts a list to a Node
#' 
#' @param x The list to be converted.
#' @param nameName The name of the element that should be used as the name, can be NULL if the children lists are named
#' @param childrenName The name of the element that contains the child list
#' @param nodeName The name of x, if no nameName is available
#' @param ... Any other argument to be passed to generic sub implementations
#' 
#' @details x should be of class list, and contain named elements, where the nameName will be converted to the Node's public attribute
#' name, and the value to its value.
#' x should not contain any element whose name is in NODE_RESERVED_NAMES_CONST, except the ones mentioned below.
#' x has to contain an element called nameName, which must be convertible to a character string.
#' x can contain an element called childrenName, which should be of class list. This will be converted to the Node's
#' children.
#' 
#' @export
as.Node.list <- function(x, nameName = "name", childrenName = 'children', nodeName = "", ...) {
  if(is.null(nameName) || is.null(x[[nameName]])) {
    if (length(nodeName)==0) myName <- tempfile(pattern = '', tmpdir = '')
    else myName <- nodeName
  } else {
    myName <- x[[nameName]]
  }
  n <- Node$new(myName)
  
  for (name in names(x)[!(names(x) %in% NODE_RESERVED_NAMES_CONST)]) {
    if (is.null(nameName) || name != nameName) n[[name]] <- x[[name]]
  }
  
  #children
  children <- x[[childrenName]]
  if (is.null(children)) return (n)
  for (i in 1:length(children)) {
    if (!is.null(names(children))) {
      childName <- names(children)[i]
    } else {
      childName <- ""
    }
    child <- children[[i]]
    n$AddChildNode(as.Node(child, nameName, childrenName, nodeName = childName, ...))
  }
  
 
  return (n)
  
}

#' Convert a Node to a list
#' 
#' @details Convert a Node to a list
#' 
#' @param x The Node to convert
#' @param unname If TRUE, then the nested children list will not have named arguments. This
#' can be useful e.g. in the context of conversion to JSON, if you prefer the children to be
#' an array rather than named objects.
#' @param nameName The name that should be given to the name element
#' @param childrenName The name that should be given to the children nested list
#' @param ... Additional parameters
#' 
#' 
#' @export
as.list.Node <- function(x, 
                         unname = FALSE, 
                         nameName = ifelse(unname, "name", ""), 
                         childrenName = 'children',
                         ...) {
  self <- x
  res <- list()
  if (nchar(nameName) != 0) res[nameName] <- x$name
  if (nchar(nameName) == 0 && x$isRoot) res["name"] <- x$name
  for (fieldName in ls(self)) {
    #print(fieldName)
    field <- self[[fieldName]]
    if(!is.function(field) 
       && !is.environment(field)
       && !(fieldName %in% NODE_RESERVED_NAMES_CONST)
    ) {
      res[fieldName] <- field
    }
  }
  if(!self$isLeaf) {
    res[[childrenName]] <- lapply(self$children, FUN = function(x) as.list(x, unname, nameName, childrenName, ...))
    if (unname) res[[childrenName]] <- unname(res[[childrenName]])
  }
  return (res)
  
}


#' Convert a \code{\link{Node}} to a \code{data.frame}
#' 
#' @param x The root node to convert to a data.frame
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. 
#' Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names 
#' (to syntactic names: see make.names) is optional.
#' @param ... the attributes to be added as columns of the data.frame. There are various
#' options:
#' \itemize{
#'  \item a string corresponding to the name of a node attribute
#'  \item the result of the \code{Node$Get} method
#' }
#' If a specific Node does not contain the attribute, \code{NA} is added to the data.frame.
#' @param filterFun a function which filters the Nodes added to the \code{data.frame}. The function must
#' take a \code{Node} as an input, and it must return \code{TRUE} or \code{FALSE}, depending on whether the
#' @param inheritFromAncestors if TRUE, then any attribute specified in \code{...} is fetched from a Node, or from any
#' of its ancestors
#' \code{Node} and its subtree should be displayed.
#' 
#' @export
as.data.frame.Node <- function(x, 
                               row.names = NULL, 
                               optional = FALSE, 
                               ..., 
                               filterFun = NULL,
                               inheritFromAncestors = FALSE
) {
  if(is.null(row.names)) {
    if(optional) {
      row.names <- rep("", x$totalCount)
    } else {
      row.name <- 1:x$totalCount
    }
  }
  
  if( !is.null(filterFun) || !x$isRoot) {
    x <- x$Clone()
  }
  
  if( !is.null(filterFun)) {
    x$Prune(filterFun)
  }
  
  df <- data.frame( levelName = format(x$Get('levelName')),
                    row.names = row.names,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- x$Get(col, inheritFromAncestors = inheritFromAncestors)
      colName <- col
    } else {
      it <- col
      colName <- names(cols)[i]
    }
    
    
    df[colName] <- it
    
  }
  
  return (df)
  
}

#' Convert a data.frame to a data.tree
#' 
#' @param x The data.frame
#' @param ... Any other argument
#' @param pathName The name of the column in x containing the path of the row
#' @param pathDelimiter The delimiter used
#' @param colLevels Nested vector of column names, determining on what node levels the attributes are written to.
#' @param na.rm If \code{TRUE}, then NA's are treated as NULL and values will not be set on nodes
#' 
#' @details x should be of class x
#' 
#' @export
as.Node.data.frame <- function(x, 
                               ..., 
                               pathName = 'pathString', 
                               pathDelimiter = '/', 
                               colLevels = NULL,
                               na.rm = FALSE) {
  root <- NULL
  mycols <- names(x)[ !(names(x) %in% c(NODE_RESERVED_NAMES_CONST, pathName)) ]
  for (i in 1:nrow(x)) {
    myrow <- x[ i, ]
    mypath <- myrow[[pathName]]
    myvalues <- myrow[!colnames(myrow) == pathName]
    
    #create node and ancestors if necessary (might already have been created)
    paths <- strsplit(mypath, pathDelimiter, fixed = TRUE)[[1]]
    if (is.null(root)) root <- Node$new(paths[1])
    mynode <- root
    colsToSet <- mycols
    colsToSetForLeaf <- mycols
    for (path in paths[-1]) {
      child <- mynode$Find(path)
      
      if( is.null(child)) {
        mynode <- mynode$AddChild(path)
      } else {
        mynode <- child
      }
      
      if( length(colLevels) >= mynode$level ) {
        colsToSet <- intersect(colLevels[[mynode$level + 1]], mycols) 
        
        #fill values on appropriate level
        for (mycol in colsToSet) {
          if ( !( na.rm && is.na(myrow[[mycol]]) )) {
            mynode[[mycol]] <- myrow[[mycol]]
          }
        }
        colsToSetForLeaf <- colsToSetForLeaf[!(colsToSetForLeaf %in% colsToSet)]
      }
      
    }
    
    #put the rest in the leaf
    for (mycol in colsToSetForLeaf) {
      if ( !( na.rm && is.na(myrow[[mycol]]) )) {
        mynode[[mycol]] <- myrow[[mycol]]
      }
      #remove
    }    
    
    
  }
  return (root)
}




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
  self <- object
  
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
  
  height <- self$height
  if(is.null(height)) {
    height <- self$Height(100)
  }
  
  if (self$isLeaf) {
    res <- self$value
    res <- structure(res, 
                     label = self$name, 
                     members = 1,
                     height = height,
                     leaf = self$isLeaf,
                     class = "dendrogram")
    
  } else {
    #res <- list()
    #class(res) <- "dendrogram"
    res <- unname(lapply(self$children, FUN = function(x) as.dendrogram(x, ...)))
    res <- structure(res, 
                     members = self$leafCount,
                     midpoint = self$midpoint,
                     height = height,
                     class = "dendrogram")
    
  }
  
  return (res)
  
}


#' Write a data.tree to Newick notation
#' 
#' @param node The node to convert
#' @param heightAttributeName The name of the attribute or function storing the height
#' @param ... parameters that will be passed on the the heightAttributeName, in case it is a function
#' 
#' @import stringr
#' @export 
ToNewick <- function(node, heightAttributeName = "Height", ...) {

  deparse <- function(x) {
    name <- str_replace_all(x$name, " ", "_")
    name <- str_replace_all(name, ",", "")
    if(!x$isRoot) {
      edge <- x$parent$GetAttribute(heightAttributeName) - x$GetAttribute(heightAttributeName, ...) 
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

#' Convert a node to a phylo object from the ape package.
#' 
#' @param x The \code{Node} to convert
#' @param heightAttributeName To use custom heights
#' @param ... any other argument
#' 
#' @export
as.phylo.Node <- function(x, heightAttributeName = "Height", ...) {
  txt <- x$ToNewick(heightAttributeName)
  return (ape::read.tree(text = txt))
}



GetPhyloLabels <- function(x, attribute, ...) {
  labels <- x$Get(attribute, ..., filterFun = function(x) !x$isLeaf)
  nodeNr <- x$leafCount + (1:length(labels))
  attr(labels, "nodes") <- nodeNr
  return(labels)
}
