


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
#' @seealso \code{\link{AhpAlternative}}
#' @export
AlternativesList <- function(...) {
  alternatives <- list(...)
  class(alternatives) <- append(class(alternatives), "AlternativesList")
  return (alternatives)
}

#' @export
print.AlternativesList <- function(x, ...) {
  stop("TODO!")
}



