#' acme is a simple company with three departments. 
#' 
#' acme's tree representation is accessed through its root, acme.
#' 
#' \itemize{
#'   \item cost, only available for leaf nodes. Cost of the project.
#'   \item p probability that a project will be undertaken.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name acme
#' @usage data(acme)
#' @format A data.tree root Node
NULL


#' ball is a simple dataset used by the ID3 vignette. 
#' 
#' Ball contains daily weather conditions, and whether hypothetical Jack, a tennis player, 
#' played ball on a given day. We can use this data for classification trees, to check whether
#' we can predict if Jack will play ball on a given day, given the current weather conditions.
#' ball contains five factor attributes:
#' 
#' \itemize{
#'   \item outlook the weather outlook
#'   \item temperature The temperature of the day (Hot, Mild, Cool, Cold)
#'   \item humidity The humidity (High, Normal, Low)
#'   \item wind The wind on the day (Weak, Strong)
#'   \item play Whether Jack plays tennis on the given days (Yes, No)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name ball
#' @usage data(ball)
#' @format data.frame
NULL

#' mushroom is a simple dataset used by the ID3 vignette. 
#' 
#' mushroom contains attributes of mushrooms. We can use this data to predict a
#' mushroom's toxicity based on its attributes.
#' The attributes available in the data set are:
#' 
#' \itemize{
#'   \item color the weather outlook
#'   \item size The temperature of the day (Hot, Mild, Cool, Cold)
#'   \item points The humidity (High, Normal, Low)
#'   \item edibility The wind on the day (Weak, Strong)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name mushroom
#' @usage data(mushroom)
#' @format data.frame
NULL