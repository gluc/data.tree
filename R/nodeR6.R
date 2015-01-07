library(R6)

Node <- R6Class("Node",
                    public = list(
                      name = NA,
                      preferences = NA,
                      children = list(),
                      priority = NA,
                      parent = NULL,
                      
                      SetPreferences = function(preferences) {
                        self$preferences <- preferences
                        priorities <- Ahp(preferences)$ahp
                        for (childName in names(priorities)) {
                          self$children[[childName]]$priority <<- priorities[[childName]]
                        }
                          
                      },
                      
                      
                      Count = function() {
                        return (length(self$children))
                      },
                      
                      AddAlternatives = function(alternativesList) {
                        for (child in self$children) {
                          if (child$Count() == 0) {
                            #leave
                            for (alternative in alternativesList) {
                              child$AddChild(alternative)
                            }
                          } else {
                            child$AddAlternatives(alternativesList)
                          }
                        }
                        
                      },
                      
                      GetPreferenceCombinations = function() {
                          combn(names(self$children), 2)
                      },
                      
                      AddChild = function(child) {
                        self$children[[child$name]] <- child
                        child$parent <- self
                      },
                      
                      IsRoot = function() {
                        return (is.null(self$parent))
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
                      
                      GetGlobalPriority = function() {
                        if (!self$IsRoot()) {
                          return (self$priority * self$parent$GetGlobalPriority())
                        } else {
                          return (1)
                        }
                      },
                      
                      initialize=function(name, ...) {
                      
                        if (!missing(name)) self$name <- name
                        
                      }
                      
                    )
                  )



print.Node <- function(root, level = 0) {
  nq <- noquote(paste0(paste(rep("* ", level), collapse=""), "", root$name, "\n"))
  cat(nq)
  for (child in root$children) {
    print(child, level+1)
  }
}

