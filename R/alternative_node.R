#' @include ahp_node.R
NULL

#' An alternative
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @seealso \code{\link{AhpNode}}
AlternativeNode <- R6Class("AlternativeNode",
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
                              stop("Cannot initialise AlternativeNode without Alternative")
                            }
                            
                            invisible (self)
                          }
                          
                        ) 
                  
 
)



