

#' Convert a a \code{SplitNode} from the party package to a \code{data.tree} structure.
#' 
#' @param splittingNode a SplittingNode object
#' 
#' @examples 
#' library(party)
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- ctree(Ozone ~ ., data = airq, 
#'                controls = ctree_control(maxsurrogate = 3))
#'                
#' tree <- CreateNodeFromParty(airct@tree)
#' tree
#' 
#' print(tree, 
#'       "label", 
#'       criterion = function(x) round(x$criterion$maxcriterion, 3),
#'       statistic = function(x) round(max(x$criterion$statistic), 3)
#'       )
#' 
#' tree$FindNode(6)$path
#' 
#' 
#' @export
#'  
CreateNodeFromParty <- function(splittingNode, left = TRUE) {
  node <- Node$new(splittingNode$nodeID,
                   weights = splittingNode$weights,
                   criterion = splittingNode$criterion,
                   psplit = splittingNode$psplit,
                   ssplit = splittingNode$ssplit,
                   label = GetSplittingNodeLabel(splittingNode, left))
  
  if (!splittingNode$terminal) {
    node$AddChildNode( CreateNodeFromParty(splittingNode$left) )
    node$AddChildNode( CreateNodeFromParty(splittingNode$right, left = FALSE) )
  }
  
  return (node)
}


GetSplittingNodeLabel <- function(splittingNode, left) {
  if( splittingNode$terminal ) {
    paste0("weights = ", sum(splittingNode$weights))
  } else {
    as.character.orderedSplit(splittingNode$psplit, left)
  }
}



as.character.orderedSplit <- function(x, left = TRUE, ...) 
{
  if (!is.null(attr(x$splitpoint, "levels"))) {
    sp <- attr(x$splitpoint, "levels")[x$splitpoint]
  }
  else {
    sp <- x$splitpoint
  }
  if (!is.null(x$toleft)) 
    left <- as.logical(x$toleft) == left
  if (left) {
    res <- paste0(x$variableName, " <= ", sp)
  }
  else {
    res <- paste0(x$variableName, " > ", sp)
  }
  return (res)
}