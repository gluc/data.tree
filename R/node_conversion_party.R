
#' Convert a a \code{SplitNode} from the party package to a \code{data.tree} structure.
#' 
#' @param x The BinaryTree
#' @param ... additional arguments (unused)
#' 
#' @examples 
#' library(party)
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- ctree(Ozone ~ ., data = airq, 
#'                controls = ctree_control(maxsurrogate = 3))
#'                
#' tree <- as.Node(airct)
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
as.Node.BinaryTree <- function(x, ...) {
  CreateNodeFromSplittingNode(x@tree)
}







CreateNodeFromSplittingNode <- function(splittingNode, left = TRUE) {
  node <- Node$new(splittingNode$nodeID,
                   weights = splittingNode$weights,
                   criterion = splittingNode$criterion,
                   psplit = splittingNode$psplit,
                   ssplit = splittingNode$ssplit,
                   label = GetSplittingNodeLabel(splittingNode, left))
  
  if (!splittingNode$terminal) {
    node$AddChildNode( CreateNodeFromSplittingNode(splittingNode$left) )
    node$AddChildNode( CreateNodeFromSplittingNode(splittingNode$right, left = FALSE) )
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





#' Convert a a \code{party} from the partykit package to a \code{data.tree} structure.
#' 
#' @param x The party object
#' @param ... other arguments (unused)
#' 
#' @examples 
#' library(partykit)
#' data("WeatherPlay", package = "partykit")
#' ### splits ###
#' # split in overcast, humidity, and windy
#' sp_o <- partysplit(1L, index = 1:3)
#' sp_h <- partysplit(3L, breaks = 75)
#' sp_w <- partysplit(4L, index = 1:2)
#' 
#' ## query labels
#' character_split(sp_o)
#' 
#' ### nodes ###
#' ## set up partynode structure
#' pn <- partynode(1L, split = sp_o, kids = list(
#'   partynode(2L, split = sp_h, kids = list(
#'       partynode(3L, info = "yes"),
#'       partynode(4L, info = "no"))),
#'   partynode(5L, info = "yes"),
#'   partynode(6L, split = sp_w, kids = list(
#'       partynode(7L, info = "yes"),
#'       partynode(8L, info = "no")))))
#' pn
#' ### tree ###
#' ## party: associate recursive partynode structure with data
#' py <- party(pn, WeatherPlay)
#' tree <- as.Node(py)
#' 
#' print(tree, 
#'       "splitname",
#'       count = function(node) nrow(node$data), 
#'       "splitLevel")
#' 
#' SetNodeStyle(tree, 
#'              label = function(node) paste0(node$name, ": ", node$splitname), 
#'              tooltip = function(node) paste0(nrow(node$data), " observations"),
#'              fontname = "helvetica")
#' SetEdgeStyle(tree, 
#'              arrowhead = "none", 
#'              label = function(node) node$splitLevel,
#'              fontname = "helvetica",
#'              penwidth = function(node) 12 * nrow(node$data)/nrow(node$root$data),
#'              color = function(node) {
#'                paste0("grey", 
#'                       100 - as.integer( 100 * nrow(node$data)/nrow(node$root$data))
#'                       )
#'              }
#'              )
#' Do(tree$leaves, 
#'    function(node) {
#'      SetNodeStyle(node, 
#'                   shape = "box", 
#'                   color = ifelse(node$splitname == "yes", "darkolivegreen4", "lightsalmon4"),
#'                   fillcolor = ifelse(node$splitname == "yes", "darkolivegreen1", "lightsalmon"),
#'                   style = "filled,rounded",
#'                   penwidth = 2
#'                   )
#'    }
#'    )
#' 
#' plot(tree)
#' 
#' 
#' @export
as.Node.party <- function(x, ...) {
  
  tree <- FromParty(x, x$node)
  tree$Do(function(node) node$splitLevel <- node$parent$splitlevels[node$position], filterFun = isNotRoot)
  return (tree)
}



FromParty <- function(party, partynode) {
  stopifnot(inherits(party, "party"))
  node <- Node$new(partynode$id)
  for (childnode in partynode$kids) {
    childid <- childnode$id
    childparty <- party[[as.character(childid)]]
    node$AddChildNode(FromParty(childparty, childnode))
  }
  node$data <- party$data
  node$fitted <- party$fitted
  node$partyinfo <- party$info
  node$nodeinfo <- partynode$info
  node$terms <- party$terms
  node$split <- partynode$split
  formatInfo <- partykit::formatinfo_node(partynode)
  if (length(partynode) > 0) {
    csplit <- partykit::character_split(partynode$split, party$data)
    node$splitlevels <- csplit$levels
    node$splitname <- csplit$name
  } else if (identical(nchar(formatInfo) > 0, TRUE)) {
    node$splitname <- formatInfo
  }
  
  
  return (node)
}
