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
                          alternative = NULL
                        ) 
                  
 
)



