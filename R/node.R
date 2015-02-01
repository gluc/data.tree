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
                      
                      
                      
                      Iterate = function(attribute, ...) {
                        v <- self[[attribute]]
                        if (is.function(v)) v <- v(...)
                        childV <- as.vector(
                                    unlist(
                                      sapply(
                                        self$children, 
                                        function(x) x$Iterate(attribute, ...)
                                      )
                                    )
                                  )
                        #browser()
                        x <- c(v, childV)
                        return (x)
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
as.data.frame.Node <- function(node, 
                               cols = c("level"), 
                               args = list(), 
                               format = list()
                               ) {
  df <- data.frame( levelName = format(node$Iterate('levelName')),
                    stringsAsFactors = FALSE)
  
  for (i in 1:length(cols)) {
    v <- cols[i]
    
    if (!is.null(names(v)) && nchar(names(v)) > 0) {
      vn <- names(v)
    } else {
      vn <- v
    }
    
    it <- node$Iterate(v, args[[vn]])
    
    if (!is.null(format[[vn]])) {
      it <- format[[vn]](it)
    }
    
    df[vn] <- it
    
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