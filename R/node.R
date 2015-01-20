library(R6)

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





print.Node <- function(root) {
  print(as.data.frame(root))
}


as.data.frame.Node <- function(root) {
  
  df <- data.frame( #name = root$IterateAttributes('name'),
                    
                    level = root$IterateAttributes('level'),
                    #  row.names = root$IterateAttributes('pathString'),
                    row.names = root$IterateAttributes('levelName'),
                    stringsAsFactors = FALSE)
  
  return (df)
                                  
}
