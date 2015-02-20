#' Node
#' 
#' @description A generic node in a tree
#' @docType class
#' @importFrom R6 R6Class
#' @field children A list of children
#' @field parent The node's parent Node
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node}.}
#'   \item{\code{\link{Get}(attribute, ..., traversal = "pre-order", assign = NULL, format = NULL)}}{Traverse the tree and collect values along the way.}
#' }
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
                        child <- Node$new(name)
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
                            return (child$Find( path[ length(path) - ( ( length(path) - 2 ) : 0 ) ] ) )
                          }
                        }
                      },
                      
                      
                      Get = function(attribute, ..., traversal = "pre-order", assign = NULL, format = NULL) {
                        #traverses in pre-order. See http://en.wikipedia.org/wiki/Tree_traversal
                        
                        if(traversal == "pre-order") {
                          # nice for printing. e.g. L1, L1.1 , L1.1.1, L1.1.2, L1.2, etc.
                          v <- self$GetAttribute(attribute, ..., assign = assign, format = format)
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              v <- c(v, child$Get(attribute, ..., traversal = traversal, assign = assign, format = format))
                            }
                          }
                        
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              childValues <- c(childValues, child$Get(attribute, ..., traversal = traversal, assign = assign, format = format))
                            }
                          }
                          v <- self$GetAttribute(attribute, ..., assign = assign, format = format)
                          v <- c(childValues, v)
                          
                        } else if (traversal == "ancestor") {
                          v <- self$GetAttribute(attribute, ..., format = format)
                          if (!self$isRoot) {
                            parentV <- self$parent$Get(attribute, ..., traversal = traversal, assign = assign, format = format)
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
                      
                      
                                          
                      GetAttribute = function(attribute, ..., assign = NULL, format = NULL) {
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
                      
                      ToDataFrame = function(...) {
                        as.data.frame(self, row.names = NULL, optional = FALSE, ...)
                      }
                      
                                            
                    ),
                
                
                    active = list(
                      
                      
                      
                      
                      name = function(value) {
                        if (missing(value)) return (self$p_name)
                        else self$p_name <- value
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
                      
                      levelName = function() {
                        paste0(paste(rep("* ", self$level), collapse=""), self$name)
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

#' Traverses the tree and collects values, results of method calls, or results of function calls along the way.
#' 
#' 
#'   @param attribute determines what is collected during traversal. The attribute can be
#'       \itemize{
#'         \item a.) the name of a field of each \code{Node} in the tree 
#'         \item b.) the name of a Method of each \code{Node}.
#'         \item c.) a function, whose first argument must be a node. In that case, the \code{Get} method calls the function by 
#'         passing \code{...} to the function.
#'        }
#'  @param traversal determines the traversal order. It can be either "pre-order", "post-order", or "ascendant"
#'  @param assign can be the name of a variable to which we assign the collected values before \code{format} is called.
#'  @param format can be a function that transforms the collected values, e.g. for printing
#'  
#'  @return a vector containing the \code{atrributes} collected during traversal, in traversal order. NULL is converted
#'  to NA, such that \code{length(Node$Get) == Node$totalCount}
#'  
#'  @examples
#'data(acme)
#'acme$Get("level")
#'acme$Get("totalCount")
#'  
#'calculateAggregateChildCost <- function(node, fun) {
#'  if (node$isLeaf) return(node$cost)
#'  fun(sapply(node$children, function(x) x$averageCost))
#'}
#'
#'myFormat <- function(x) {
#'  format(x, nsmall=2, scientific = FALSE)
#'}
#'
#'acme$Get(calculateAggregateChildCost, mean, traversal = "post-order", assign = "averageCost", format = myFormat)
#'  
#'  
#'  
#' @export
Get = function(attribute, ..., traversal = "pre-order", assign = NULL, format = NULL) {
  stop("This method can only be called on a Node!")
}




#' @export
print.Node <- function(x, ...) {
  print(as.data.frame(x, row.names = NULL, optional = FALSE, ...))
}

#' Convert a tree to a data.frame. 
#' 
#' @param x The root node to convert to a data.frame
#' @param row.names
#' @param optional
#' @param ... the attributes to be added as columns of the data.frame. There are various
#' options:
#' \itemize{
#'  \item a string corresponding to the name of a node attribute
#'  \item the result of the \code{Node$Get} method
#' }
#' If a specific Node does not contain the attribute, the data.frame will contain NA.
#' 
#' @export
as.data.frame.Node <- function(x, row.names = NULL, optional = FALSE, ...) {
  if(is.null(row.names)) {
    if(optional) {
      row.names <- rep("", x$totalCount)
    } else {
      row.name <- 1:x$totalCount
    }
  }
  
  df <- data.frame( levelName = format(x$Get('levelName')),
                    row.names = row.names,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- x$Get(col)
      colName <- col
    } else {
      it <- col
      colName <- names(cols)[i]
    }
    
    df[colName] <- it
    
  }
  return (df)
  
}


