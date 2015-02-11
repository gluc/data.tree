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
                      
                      
                      Traverse = function(attribute, ..., mode = "pre-order", assign = NULL, format = NULL) {
                        #traverses in pre-order. See http://en.wikipedia.org/wiki/Tree_traversal
                        
                        if(mode == "pre-order") {
                          # nice for printing. e.g. L1, L1.1 , L1.1.1, L1.1.2, L1.2, etc.
                          v <- self$GetSetAttribute(attribute, ..., assign = assign, format = format)
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              v <- c(v, child$Traverse(attribute, ..., mode = mode, assign = assign, format = format))
                            }
                          }
                        
                        } else if (mode == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              childValues <- c(childValues, child$Traverse(attribute, ..., mode = mode, assign = assign, format = format))
                            }
                          }
                          v <- self$GetSetAttribute(attribute, ..., assign = assign, format = format)
                          v <- c(childValues, v)
                          
                        } else if (mode == "ancestor") {
                          v <- self$GetSetAttribute(attribute, ..., assign = assign, format = format)
                          if (!self$isRoot) {
                            parentV <- self$parent$Traverse(attribute, ..., mode = mode, assign = assign, format = format)
                            v <- c(v, parentV)
                          }
                        }
                        return (v)
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
                        ChildL <- sapply(self$children, function(x) x$GetSetAttribute(attribute, ...))
                        names(ChildL) <- names(self$children)
                        self$children <- self$children[names(sort(ChildL, decreasing = decreasing, na.last = TRUE))]
                        if (recursive) for(child in self$children) child$Sort(attribute, ..., decreasing = decreasing, recursive = recursive)
                      },
                      
                                          
                      GetSetAttribute = function(attribute, ..., assign = NULL, format = NULL) {
                        if(is.function(attribute)) {
                          #function
                          v <- attribute(self, ...)
                        } else if(is.character(attribute) && length(attribute) == 1) {
                          #property
                          v <- self[[attribute]]
                          if (is.function(v)) v <- v(...)
                        } else {
                          #used for setting only
                          v <- attribute
                        }
                        
                        if (!is.null(assign)) {
                          self[[assign]] <- v
                        }
                        if (is.null(v)) v <- NA
                        if (length(v) == 0) v <- NA
                        names(v) <- self$name
                        #x <- try(names(v) <- self$name)
                        #if(class(x) == "try-error") browser()
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
  df <- data.frame( levelName = format(node$Traverse('levelName')),
                    row.names = 1:node$totalCount,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- node$Traverse(col)
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