library(R6)
library(data.table)

AhpNode <- R6Class("AhpNode",
                inherit = Node,
                lock = FALSE,
                public = list(
                  priority = 1,
                  
                  
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
                    
                    priorities <- Ahp(preferenceMatrix)$ahp
                    
                    sapply(self$children, function(x) x$priority <- as.numeric(priorities[x$name]))
                    
                    invisible (self)
                  }
    
                  
                  
                ),
                active = list(
                  
                  
                  
 
                  
       
                  
                  globalPriority = function() {
                    if(self$isRoot) {
                      return (self$priority)
                    }
                    return (self$priority * self$parent$globalPriority)
                  }
                  
                  
                ),
                
                private = list(
                  p_name = "",
                  preferenceMatrix = NA
                )
)



priorities <- function(ahpNode) {
  sapply(ahpNode$children, function(x) x$priority)
}

globalPriorities <- function(ahpNode) {
  sapply(ahpNode$children, function(x) x$globalPriority)
}




print.Node <- function(root, level = 0) {
  nq <- noquote(paste0(paste(rep("* ", level), collapse=""), 
                       root$name,
                       "                             "))
                
  nq <- paste0(substr(nq, 1, 25), 
               " ",
               FormatPercent(root$priority, 1),
               "                        ")
  
  
  nq <- paste0(substr(nq, 1, 35), 
               " ",
               FormatPercent(root$globalPriority, 1),
               "                        ")
  
  nq <- paste0(nq, "\n")
  
  
  cat(nq)
  for (child in root$children) {
    print(child, level+1)
  }
}

