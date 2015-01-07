library(R6)

Node <- R6Class("Node",
                lock = FALSE,
                    public = list(
                      children = list(),
                      childPriorities = NA,
                      preferenceMatrix = NA,
                      parents = list(),
                      
                      initialize=function(name, ...) {
                        
                        if (!missing(name)) self$name <- name
                        invisible (self)
                      },
                      
                      CalculatePreferences = function(preferenceFunction) {
                        combo <- self$preferenceCombinations
                        prefs <- apply(combo, 2, function(x) preferenceFunction(self$children[[ x[1] ]], self$children[[ x[2] ]]))
                        mat <- AhpMatrix(combo[1,], combo[2,], prefs)
                        self$SetPreferenceMatrix(mat)
                        invisible (self)
                      },
                      
                      
                      SetPreferenceMatrix = function(preferenceMatrix) {
                        
                        if (missing(preferenceMatrix)) return (self$preferenceMatrix)
                        
                        self$preferenceMatrix <- preferenceMatrix
                        self$childPriorities <- Ahp(preferenceMatrix)$ahp
                      },
                      
                      AddAlternatives = function(alternativesList) {
                        
                        for (child in self$children) {
                          if (child$count == 0) {
                            #leave
                            for (alternative in alternativesList) {
                              child$AddChildNode(alternative)
                            }
                          } else {
                            child$AddAlternatives(alternativesList)
                          }
                        }
                        invisible (self)
                        
                      },
                      
                      
                      AddChild = function(name) {
                        child <- Node$new(name)
                        invisible (self$AddChildNode(child))
                      },
                      
                      AddChildNode = function(child) {
                        self$children[[child$name]] <- child
                        child$parents[[self$name]] <- self
                        invisible (child)
                      },
                      
                      
                      AddSibling = function(name) {
                        sibling <- Node$new(name)
                        invisible (self$AddSiblingNode(sibling))
                      },
                      
                      AddSiblingNode = function(sibling) {
                        if (self$isRoot) stop("Cannot add sibling to root!")
                        for (parent in self$parents) {
                          parent$AddChildNode(sibling)
                        }
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
                      }
                      
                      
                      
                      
                      
                    ),
                    active = list(
                      
                      
                      
                      
                      name = function(value) {
                        if (missing(value)) return (self$p_name)
                        else self$p_name <- value
                      },
                      
                      
                      priority = function() {
                        if (self$isRoot) {
                          return (1)
                        } else if (length(self$parents) > 1) {
                          stop("Cannot find priority if node has more than one parents.")
                        } else {
                          return (self$parents[[1]]$childPriorities[[self$name]])
                        }
                      },
                      
                      isRoot = function() {
                        return (length(self$parents) == 0)
                      },
                      
                      count = function() {
                        return (length(self$children))
                      },
                      
                      preferenceCombinations = function() {
                        combn(names(self$children), 2)
                      },
                      
                      globalPriority = function() {
                        if (!self$isRoot) {
                          return ( sum( sapply (self$parents, function(x) x$childPriorities[self$name] * x$globalPriority) ) )
                        } else {
                          return (1)
                        }
                      },
                      
                      globalChildPriorities = function() {
                        return (self$childPriorities * self$globalPriority)
                      }
                      
                    ),
                
                
                    private = list(
                      p_name = ""
                    )
                  )



print.Node <- function(root, level = 0) {
  nq <- noquote(paste0(paste(rep("* ", level), collapse=""), "", root$name, "\n"))
  cat(nq)
  for (child in root$children) {
    print(child, level+1)
  }
}

