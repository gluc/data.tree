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
                    combo <- self$preferenceCombinations
                    prefs <- apply(combo, 2, function(x) FUN(self$children[[ x[1] ]], self$children[[ x[2] ]], ...))
                    mat <- AhpMatrix(combo[1,], combo[2,], prefs)
                    self$SetPreferenceMatrix(mat)
                    invisible (self)
                  },
                  
                  
                  
                  AddAlternatives = function(alternativesTree) {
                    
                    if(length(self$parents) == 1) {
                      parent = self$parents[[1]]
                      if(!is.na(parent$classifierName)) {
                        alternativesList <- alternativesList[which(sapply(alternativesList, function(x) x[[parent$classifierName]]) == self$name)]
                      }
                    }
                    
                    for (child in self$children) {
                      child$AddAlternatives(alternativesList)
                    }
                    
                    if (self$count == 0) {
                      #leaf
                      for (alternative in alternativesList) {
                        #if (is.na(self$parent$classifierName) || alternative[[self$parent$classifierName]] == self$name) {
                        self$AddChildNode(alternative)
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
print.Node <- function(root) {
  print(as.data.frame(root))
}


#' @export
as.data.frame.AhpNode <- function(root) {
  
  df <- as.data.frame.Node(root)
  df$priority <- FormatPercent(root$IterateAttributes('priority'))
  df$globalPriority <- FormatPercent(root$IterateAttributes('globalPriority'))
  df$childConsistency <- root$IterateAttributes('childConsistency')
  
  return (df)
  
}
