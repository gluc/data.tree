#' @include ahp_node.R
NULL

#' An alternative
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @seealso \code{\link{AhpNode}}
AhpAlternativeNode <- R6Class("AhpAlternativeNode",
                        inherit = AhpNode,
                        lock = FALSE,
                        public = list(
                          alternative = NULL,
                          
                          initialize=function(alternative, ...) {
                            if (!missing(alternative)) {
                              self$name <- alternative$name
                              self$priority <- NA
                              self$alternative <- alternative
                              alternative$alternativeNodes <- c(alternative$alternativeNodes, self)
                            } else {
                              stop("Cannot initialise AhpAlternativeNode without Alternative")
                            }
                            
                            invisible (self)
                          }
                          
                        ) 
                  
 
)



