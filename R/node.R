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
                      
                      
                      
                      
                      
                      Find = function(path) {
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
                      
                      
                      IterateAttributes = function(attribute) {
                        mypath <- self[[attribute]]
                        childPaths <- as.vector(unlist(sapply(self$children, function(x) x$IterateAttributes(attribute))))
                        #browser()
                        x <- c(mypath, childPaths)
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
                      }
                      
                      
                      
                      
                    ),
                
                    private = list(
                      p_name = ""
                    )
                  )




#' @export
print.Node <- function(node) {
  #print(as.data.frame(node), right=FALSE)
  print(as.data.frame(node))
}

#' @export
as.data.frame.Node <- function(node) {
  
  df <- data.frame( name = format(node$IterateAttributes('levelName')),
                    
                    level = node$IterateAttributes('level'),
                    #  row.names = node$IterateAttributes('pathString'),
                    #row.names = node$IterateAttributes('levelName'),
                    stringsAsFactors = FALSE)
  
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