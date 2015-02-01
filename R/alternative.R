#' @include alternative_node.R
NULL


#' AHP alternative
#' 
#' @description An alternative in the AHP problem.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @usage Alternative$new()
#' @keywords ahp
Alternative <- R6Class("Alternative",
                        lock = FALSE,
                        public = list(
                          name = "",
                          alternativeNodes = list(),
                          
                          initialize=function(name, ...) {
                            if (!missing(name)) {
                              self$name <- name
                            }
                            
                            invisible (self)
                          }
                        ) 
                  
 
)






#' Constructor for a list of alternatives
#' 
#' @seealso \code{\link{Alternative}}
#' @export
AlternativesList <- function(...) {
  alternatives <- list(...)
  class(alternatives) <- append("AlternativesList", class(alternatives))
  return (alternatives)
}


#' @export
print.AlternativesList <- function(alternatives) {
  print(as.data.frame(alternatives))
}


#' Convert an AlternativesList object into a dataframe.
#' This is very useful for printing the results of an AHP study.
#' 
#' @export
as.data.frame.AlternativesList <- function(alternatives) {
  
  goal <- alternatives[[1]]$alternativeNodes[[1]]$root
  
  o <- order(sapply(alternatives, function(x) goal$GetAlternativePriority(x$name)), decreasing = TRUE)
  alternatives <- alternatives[o]
  
  cols <- c("isLeaf", "consistency", "globalPriority", rep("GetAlternativePriority", length(alternatives)))
 
  names(cols) <- c("isLeaf", "consistency", "globalPriority", names(alternatives))
  args <- sapply(alternatives, function(x) x$name)
  names(args) <- names(alternatives)
  format <- as.list(rep(0, 2 + length(alternatives)))
  format[[1]] <- PrintFixedDecimal
  for(i in 2:length(format)) format[[i]] <- FormatPercent
  names(format) <- c("consistency", "globalPriority", names(alternatives))
  
  df <- as.data.frame(goal, cols = cols, args = args, format = format)
  df <- df[!df$isLeaf,-2]
  
  return (df)  
}


#' @export
priorities.AlternativesList <- function(alternatives) {
  sapply(alternatives, function(y) sum(sapply(y$alternativeNodes, function(x) x$globalPriority)))
}
