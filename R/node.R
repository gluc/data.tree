#' Node
#' 
#' @description A generic node in a tree
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @usage Node$new()
#' @field children A list of children
#' @field parent The node's parent Node
#' @section Methods:
#' \describe{
#'   \item{\code{AddChild(name = "MyNode")}}{Creates a new Node called \code{name} and adds it to this node.}
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
                      
                      Set = function(attribute, values, traversal = "pre-order", returnValues = FALSE) {
                        if(traversal == "pre-order") {
                          
                          values <- self$SetAttribute(attribute, values)
                          
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              values <- child$Set(attribute, values, traversal = traversal, returnValues = TRUE)
                            }
                          }
                          
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              values <- child$Set(attribute, values, traversal = traversal, returnValues = TRUE)
                            }
                          }
                          values <- self$SetAttribute(attribute, values)
                          
                          
                        } else if (traversal == "ancestor") {
                          values <- self$SetAttribute(attribute, values)
                          if (!self$isRoot) {
                            values <- self$parent$Set(attribute, values, traversal = traversal, returnValues = TRUE)
                          }
                        }
                        if (returnValues) invisible (values)
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
                        
                        if(!is.null(format)) v <- format(v)
                        return (v)
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





#' @export
print.Node <- function(node, ...) {
  print(as.data.frame(node, ...))
}


#' @export
as.data.frame.Node <- function(node, ...) {
  df <- data.frame( levelName = format(node$Get('levelName')),
                    row.names = 1:node$totalCount,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- node$Get(col)
      colName <- col
    } else {
      it <- col
      colName <- names(cols)[i]
    }
    
    df[colName] <- it
    
  }
  return (df)
  
}


#' @export
as.Node <- function(node) {
  UseMethod("as.Node", node)
}

#' @export
as.Node.AhpNode <- function(node) {
  class(node) <- c("Node", "R6")
}