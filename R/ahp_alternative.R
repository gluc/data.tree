


#' AHP alternative
#' 
#' @description An alternative in the AHP problem.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @usage AhpAlternative$new()
#' @keywords ahp
AhpAlternative <- R6Class("AhpAlternative",
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



