#' @include node.R
NULL

#' A node in the Ahp Tree
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @seealso \code{\link{Node}}
AhpNode <- R6Class("AhpNode",
                inherit = Node,
                lock = FALSE,
                public = list(
                  
                  initialize=function(name, priority, ...) {
                    if (!missing(name)) self$name <- name
                    if (!missing(priority)) self$priority <- priority
                    else self$priority <- NA
                    invisible (self)
                  },
                  
                  childConsistency = NA,
                  
                  CalculatePreferences = function(FUN, ...) {
                    
                    pp <- self$GetChildCombinations()
                    
                    
                    for(i in 1:dim(pp)[1]) {
                      name1 <- as.character(pp[i, 'a'])
                      name2 <- as.character(pp[i, 'b'])
                      pp <- setPreference(pp, 
                                          name1, 
                                          name2, 
                                          FUN(
                                            self$children[[name1]]$alternative, 
                                            self$children[[name2]]$alternative, 
                                            ...)
                                          )                      
                    }
                    
                   
                    preferenceMatrix <- AhpMatrix(pp)
                    self$SetChildPreferenceMatrix(preferenceMatrix)
                    invisible (self)
                  },
                  
                  
                  
                  AddAlternatives = function(alternativesList) {
                    
                    for (child in self$children) {
                      child$AddAlternatives(alternativesList)
                    }
                    
                    if (self$count == 0) {
                      #leaf
                      for (alternative in alternativesList) {
                        #if (is.na(self$parent$classifierName) || alternative[[self$parent$classifierName]] == self$name) {
                        alternativeNode <- AhpAlternativeNode$new(alternative)
                        super$AddChildNode(alternativeNode)
                        #}
                      }
                    }
                    
                    invisible (self)
                    
                  },
                  
                  
                  
      
                  AddChild = function(name) {
                    child <- AhpNode$new(name)
                    invisible (super$AddChildNode(child))
                  },
                  
                  
                  GetChildCombinations = function() {
                    combos <- t(combn(names(self$children), 2))
                    colnames(combos)=c("a", "b")
                    df <- data.frame(combos)
                    df$pref <- 1
                    return (df)
                  },
                  
                  SetChildPreferenceMatrix = function(preferenceMatrix) {
                    #cannot use active, as chaining doesn't work (e.g. root$Find("Cost")$childPreferenceMatrix <- p)
                                        
                    self$preferenceMatrix <- preferenceMatrix
                    
                    ahpResult <- Ahp(preferenceMatrix)
                    
                    priorities <- ahpResult$ahp
                    self$childConsistency <- ahpResult$consistency
                    
                    sapply(self$children, function(x) x$priority <- as.numeric(priorities[x$name]))
                    
                    invisible (self)
                  },
                  
                  
                  ApplyPreferenceMatrix = function(preferenceMatrix, fieldName) {
                    cats <- dimnames(preferenceMatrix)[1]
                    mat <- matrix(1, nrow = length(self$children), length(self$children), byrow = TRUE, dimnames = list(names(self$children), names(self$children)))
                    for (i in self$children) {
                      for (j in self$children) {
                        class1 <- i$alternative[[fieldName]]
                        class2 <- j$alternative[[fieldName]]
                        mat[i$name, j$name] <- preferenceMatrix[class1, class2]
                      }
                    }
                    self$SetChildPreferenceMatrix(mat)
                  }
                  
                  
                  
                  
                ),
                active = list(
                  
                  priority = function(value) {
                    
                    if (missing(value)) {
                              
                      if(self$isRoot) {
                        
                        return (1)
                      } else {
                        #return (0.2)
                        return (self$p_priority)
                      }
                    } else {
                      self$p_priority <- value
                      #print(self$p_priority)
                    }
                  },
 
                  
       
                  
                  globalPriority = function() {
                    if(self$isRoot) {
                      return (self$priority)
                    }
                    return (self$priority * self$parent$globalPriority)
                  }
                  
                  
                ),
                
                private = list(
                  p_priority = NA,
                  preferenceMatrix = NA
                )
)


#' @export
priorities <- function(ahpNode) {
  sapply(ahpNode$children, function(x) x$priority)
}

#' @export
globalPriorities <- function(ahpNode) {
  sapply(ahpNode$children, function(x) x$globalPriority)
}



#' @export
as.data.frame.AhpNode <- function(root) {
  
  df <- as.data.frame.Node(root)
  df$priority <- FormatPercent(root$IterateAttributes('priority'))
  df$globalPriority <- FormatPercent(root$IterateAttributes('globalPriority'))
  df$childConsistency <- root$IterateAttributes('childConsistency')
  
  return (df)
  
}
