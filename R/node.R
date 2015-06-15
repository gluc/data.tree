#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' Attribute names.
#' 
NODE_RESERVED_NAMES_CONST <- c( 'AddChild',
                                'AddChildNode',
                                'AddSibling',
                                'AddSiblingNode',
                                'Aggregate',
                                'children',
                                'Clone',
                                'count',
                                'fields',
                                'Find',
                                'Get',
                                'GetAttribute',
                                'initialize',
                                'isLeaf',
                                'isRoot',
                                'leaves',
                                'level',
                                'levelName',
                                'name',
                                'parent',
                                'path',
                                'pathString',
                                'position', 
                                'root',
                                'Set',
                                'SetAttribute',
                                'Sort',
                                'ToDataFrame',
                                'ToList',
                                'totalCount')


#' Create Trees With \code{Node}s
#' 
#' @description \code{Node} is at the very heart of the \code{data.tree} package. All trees are constructed
#' by tying toghether \code{Node} objects.
#' @docType class
#' @examples
#' library(data.tree)
#' acme <- Node$new("Acme Inc.")
#' accounting <- acme$AddChild("Accounting")
#' print(acme)
#' @seealso For more details see the \code{data.tree} vignette: \code{vignette("data.tree")}
#' @importFrom R6 R6Class
#' @field children A list of children \code{Node}s
#' @field parent The node's parent \code{Node}
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Node$new(name)}}{Creates a new \code{Node} called \code{name}. Often used to construct the root.}
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node}.}
#'   \item{\code{\link{Find}(...)}}{Find a node with path \code{...}, where the \code{...} arguments are the \code{name}s of the \code{Node}s }
#'   \item{\code{\link{Get}(attribute, ..., traversal = "pre-order", filterFun = function(x) TRUE, assign = NULL, format = NULL)}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Set}(..., traversal = "pre-order", returnValues = FALSE)}}{Traverses the tree and assigns attributes along the way.}
#'   \item{\code{\link{Aggregate}(attribute, fun, ...)}}{Traverses the tree and calls \code{fun(children$Aggregate(...))} on each node. }
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE)}}{Sorts the children of a node according to \code{attribute}}
#'   \item{\code{Clone()}}{Creates a deep copy of a \code{Node} and all its sub-nodes}
#'   \item{\code{\link{ToDataFrame}(..., filterFun = function(x) TRUE, inheritFromAncestors)}}{Converts the tree below this \code{Node} to a \code{data.frame}}
#'   \item{\code{\link{ToList}(..., unname = FALSE, nameName = 'name', childrenName = 'children')}}{Converts the tree below this \code{Node} to a \code{list}}
#'
#' }
#' 
#' @section Properties:
#'   
#' \describe{
#'  \item{\code{children}}{Returns a list containing all the children of this \code{Node}}
#'  \item{\code{parent}}{Returns the parent \code{Node} of this \code{Node}}
#'  \item{\code{name}}{Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}}
#'  \item{\code{fields}}{Gets the names of the set properties of a \code{Node}}
#'  \item{\code{isLeaf}}{Returns \code{TRUE} if the \code{Node} is a leaf, \code{FALSE} otherwise}
#'  \item{\code{isRoot}}{Returns \code{TRUE} if the \code{Node} is the root, \code{FALSE} otherwise}
#'  \item{\code{count}}{Returns the number of children of a \code{Node}}
#'  \item{\code{totalCount}}{Returns the total number of \code{Node}s in the tree}
#'  \item{\code{path}}{Returns a vector of mode \code{character} containing the names of the \code{Node}s in the path from the root to this \code{Node}}
#'  \item{\code{pathString}}{Returns a string representing the path to this \code{Node}, separated by backslash}
#'  \item{\code{levelName}}{Returns the name of the \code{Node}, preceded by level times '*'. Useful for printing.}
#'  \item{\code{leaves}}{Returns a list containing all the leaf \code{Node}s }
#'  \item{\code{level}}{Returns an integer representing the level of a \code{Node}. For example, the root has level 0.}
#'  \item{\code{root}}{Returns the root \code{Node} of a \code{Node}'s tree}
#'  
#' }
#' 
#' @export
#' @format An \code{\link{R6Class}} generator object
Node <- R6Class("Node",
                lock = FALSE,
                    public = list(
                      children = list(),
                      parent = NULL,
                      
                      initialize=function(name, ...) {
                        if (!missing(name)) self$name <- name
                        invisible (self)
                      },
                      
                      
                      AddChild = function(name) {
                        child <- Node$new(as.character(name))
                        invisible (self$AddChildNode(child))
                      },
                      
                      AddChildNode = function(child) {
                        self$children[[child$name]] <- child
                        child$parent <- self
                        invisible (child)
                      },
                      
                      
                      AddSibling = function(name) {
                        sibling <- Node$new(name)
                        invisible (self$AddSiblingNode(sibling))
                      },
                      
                      AddSiblingNode = function(sibling) {
                        if (self$isRoot) stop("Cannot add sibling to root!")
                        self$parent$AddChildNode(sibling)
                        invisible (sibling)
                      },
                      
                      
                      Find = function(...) {
                        path <- as.character(list(...))
                        if (length(path) == 0) {
                          return (self)
                        } else {
                          child <- self$children[[path[1]]]
                          if (is.null(child)) {
                            return (NULL)
                          } else if (length(path) == 1) {
                            return (child)
                          } else {
                            return (do.call(child$Find, list(...)[-1]))
                            #return (child$Find( path[ length(path) - ( ( length(path) - 2 ) : 0 ) ] ) )
                          }
                        }
                      },
                      
                      
                      Get = function(attribute, 
                                     ..., 
                                     traversal = "pre-order", 
                                     filterFun = NULL, 
                                     assign = NULL, 
                                     format = NULL,
                                     inheritFromAncestors = FALSE) {
                        #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
                        v <- vector()
                        if(traversal == "pre-order") {
                          
                          if(!self$isLeaf) {
                            
                            for(child in self$children) {
                              v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
                            }
                            
                          }
                          
                          if(is.null(filterFun) || filterFun(self)) {
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(me, v)
                          }
                        
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          
                          v <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              v <- c(childValues, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
                            }
                          }
                          if(is.null(filterFun) || filterFun(self)) {
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(v, me)
                          }
                          
                        } else if (traversal == "ancestor") {
                          v <- vector()
                          
                          if (!self$isRoot) {
                            v <- self$parent$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                          }
                          if(is.null(filterFun) || filterFun(self)) {
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(v, parentV)
                            
                          }
                        }
                        return (v)
                      },
                      
                      Set = function(..., traversal = "pre-order", returnValues = FALSE) {
                        args <- list(...)
                        argsnames <- sapply(substitute(list(...))[-1], deparse)
                        gargsnames <- names(args)
                        if (is.null(gargsnames)) gargsnames <- vector(mode = "character", length = length(args))
                        gargsnames[nchar(gargsnames) == 0] <- argsnames[nchar(gargsnames) == 0]
                        names(args) <- gargsnames
                        
                        if(traversal == "pre-order") {
                          
                          #for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          args <- Map(function(name, arg) self$SetAttribute(name, arg), names(args), args)
                          
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              
                              args <- do.call(child$Set, c(args, traversal = traversal, returnValues = TRUE))
                            }
                          }
                          
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              args <- do.call(child$Set, c(args, traversal = traversal, returnValues = TRUE))
                            }
                          }
                          for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          
                          
                        } else if (traversal == "ancestor") {
                          for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          if (!self$isRoot) {
                            args <- do.call(self$parent$Set, c(args, traversal = traversal, returnValues = TRUE))
                          }
                        }
                        if (returnValues) invisible (args)
                        else invisible (self)
                      },
                      
                      Aggregate = function(attribute, fun, ...) {
                        v <- self[[attribute]]
                        if (!is.null(v)) {
                          if (is.function(v)) v <- v(...)
                          return (v)
                        }
                        if (self$isLeaf) stop(paste0("Cannot find attribute ", attribute, "!"))
                        values <- sapply(self$children, function(x) x$Aggregate(attribute, fun, ...))
                        result <- fun(values)
                        return (result)
                      },
                                            
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        if (self$isLeaf) return()
                        ChildL <- sapply(self$children, function(x) x$GetAttribute(attribute, ...))
                        names(ChildL) <- names(self$children)
                        self$children <- self$children[names(sort(ChildL, decreasing = decreasing, na.last = TRUE))]
                        if (recursive) for(child in self$children) child$Sort(attribute, ..., decreasing = decreasing, recursive = recursive)
                      },
                      
                      
                      SetAttribute = function(attribute, values) {
                        
                        if (length(values) == 1) {
                          self[[attribute]] <- values
                          return (values)
                        } else if (length(values) > 1) {
                          self[[attribute]] <- values[1]
                          return (values[-1])
                        } else if(is.null(values)) {
                          self[[attribute]] <- NULL
                          return (NULL)
                        } else {
                          stop("length of values must be 1 or equal to the number of nodes")
                        }
                      },
                      
                      
                                          
                      GetAttribute = function(attribute, ..., assign = NULL, format = NULL, inheritFromAncestors = FALSE) {
                        if(is.function(attribute)) {
                          #function
                          v <- attribute(self, ...)
                        } else if(is.character(attribute) && length(attribute) == 1) {
                          #property
                          v <- self[[attribute]]
                          if (is.function(v)) v <- v(...)
                        } else {
                          stop("attribute must be a function, the name of a public property, or the name of method")
                        }
                        #if(attribute == "start") browser()
                        if(is.null(v) && inheritFromAncestors && !self$isRoot) {
                          v <- self$parent$GetAttribute(attribute, 
                                                        ..., 
                                                        assign = assign, 
                                                        format = format, 
                                                        inheritFromAncestors = inheritFromAncestors)
                        }
                        if (is.null(v)) v <- NA
                        if (length(v) == 0) v <- NA
                        if(!is.null(assign)) self[[assign]] <- v
                        names(v) <- self$name
                        
                        if(!is.null(format)) {
                          if (!is.function(format)) stop("form must be a function!")
                          v <- format(v)
                        }
                        return (v)
                      },
                      
                      Clone = function() {
                        return (as.Node(self$ToList()))
                      },
                      
                      #Filter = function(){}
                      
                      ToDataFrame = function(..., filterFun = NULL, inheritFromAncestors = FALSE) {
                        as.data.frame(self, row.names = NULL, optional = FALSE, ..., filterFun = filterFun, inheritFromAncestors = inheritFromAncestors)
                      }, 
                      
                      
                      ToList = function(..., unname = FALSE, 
                                        nameName = 'name', 
                                        childrenName = 'children') {
                        as.list(self, ..., unname, nameName, childrenName)
                      }
                      
                                            
                    ),
                
                
                    active = list(
                      
                      name = function(value) {
                        if (missing(value)) return (private$p_name)
                        else private$p_name <- value
                      },
                      
                      isLeaf = function() {
                        return (length(self$children) == 0) 
                      },
                      
                      isRoot = function() {
                        return (is.null(self$parent))
                      },
                      
                      count = function() {
                        return (length(self$children))
                      },
                      
                      totalCount = function() {
                        return (1 + sum(as.numeric(sapply(self$children, function(x) x$totalCount))))
                      }, 
                      
                      path = function() {
                        c(self$parent$path, self$name)
                      }, 
                      
                      pathString = function() {
                        paste(self$path, collapse="/")
                      },
                      
                      position = function() {
                        if (self$isRoot) return (0)
                        match(self$name, names(self$parent$children))
                      },
                      
                      levelName = function() {
                        paste0(self$.separator, self$name)
                      },
                      
                      
                      fields = function() {
                        ls(self)[!(ls(self) %in% NODE_RESERVED_NAMES_CONST)]
                      },
                      
                      
                      .separator = function() {
                        if (self$isRoot) return("")
                        if (self$position == self$parent$count) mySeparator <- paste0(" ", "\u00B0", "--") 
                        else mySeparator <- paste0(" ", "\u00A6", "--")
                        return (paste0(self$parent$.parentSeparator, mySeparator))
                      },
                      
                      .parentSeparator = function() {
                        if (self$isRoot) return("")
                        if (self$position == self$parent$count) mySeparator <- "    "
                        else mySeparator <- paste0(" ", "\u00A6", "  ")
                        paste0(self$parent$.parentSeparator, mySeparator)
                        
                      },
                      
                      leaves = function() {
                        if (self$isLeaf) {
                          return (self)
                        } else {
                          l <- unlist(sapply(self$children, function(x) x$leaves))
                        }
                      },
                      
                      level = function() {
                        if (self$isRoot) {
                          return (0)
                        } else {
                          return (1 + self$parent$level)
                        }
                      },
                      
                      root = function() {
                        if (self$isRoot) {
                          invisible (self)
                        } else {
                          invisible (self$parent$root)
                        }
                      }
                      
                      
                      
                      
                    ),
                
                    private = list(
                      
                      p_name = ""
                      
                      
                    )
                  )


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
  print(as.data.frame(x, row.names = NULL, optional = FALSE, ...))
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
#' @param nameName The name of the element that should be used as the name
#' @param childrenName The name of the element that contains the child list
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
as.Node.list <- function(x, nameName = 'name', childrenName = 'children', ...) {
  n <- Node$new(x[[nameName]])
  
  for (name in names(x)[!(names(x) %in% NODE_RESERVED_NAMES_CONST)]) {
    if (name != nameName) n[[name]] <- x[[name]]
  }
  
  #children
  for (child in x[[childrenName]]) {
    n$AddChildNode(as.Node(child, nameName, childrenName, ...))
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
as.list.Node <- function(x, ...,
                         unname = FALSE, 
                         nameName = 'name', 
                         childrenName = 'children') {
  self <- x
  res <- list()
  res[nameName] <- x$name
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
  
  df <- data.frame( levelName = format(x$Get('levelName', filterFun = filterFun)),
                    row.names = row.names,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- x$Get(col, filterFun = filterFun, inheritFromAncestors = inheritFromAncestors)
      colName <- col
    } else {
      it <- col
      colName <- names(cols)[i]
    }
    
    
    df[colName] <- it
    
  }
  if(!is.null(filterFun)) {

    df <- df[ , -1]
  }
  return (df)
  
}

#' Convert a data.frame to a data.tree
#' 
#' @param x The data.frame
#' @param ... Any other argument
#' @param pathName The name of the column in x containing the path of the row
#' @param pathDelimiter The delimiter used
#' @param colLevels Nested vector of column names, determining on what node levels the values are written to.
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
        colsToSet <- intersect(colLevels[[mynode$level]], mycols) 
        
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
#' @param ... Additional parameters
#' 
#' @return The root \code{Node} of a \code{data.tree}
#' @export
as.Node.dendrogram <- function(x, ...) {
  #str(unclass(dend1))
  if (!is.leaf(x)) {
    name <- tempfile(pattern = '', tmpdir = '')
  } else {
    name <- attr(x, 'label')
  }
  
  n <- Node$new(name)
  reserved <- c('label', 'class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  for (a in names(attributes(x))[!(attributes(x) %in% NODE_RESERVED_NAMES_CONST) && !(attributes(x) %in% reserved)]) {
    n[[a]] <- attr(x, a)
  }
  
  if (!is.leaf(x)) {
    for (i in 1:length(x)) {
      n$AddChildNode(as.Node(x[[i]], ...))
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
  
  #NOT YET WORKING
  
  if (self$isLeaf) {
    res <- self$value
    class(res) <- "dendrogram"
    attr(res, 'label') <- self$name

  } else {
    #res <- list()
    #class(res) <- "dendrogram"
    res <- lapply(self$children, FUN = function(x) as.dendrogram(x, ...))
    class(res) <- "dendrogram"
    res <- unname(res)
  }
  
  for (fieldName in ls(self)) {
    #print(fieldName)
    field <- self[[fieldName]]
    if(!is.function(field) 
       && !is.environment(field)
       && !(fieldName %in% NODE_RESERVED_NAMES_CONST)
    ) {
      attr(res, fieldName) <- field
    }
  }
  
  attr(res, "members") <- self$totalCount
  
  return (res)
  
}

