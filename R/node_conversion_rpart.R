#' Convert an \code{\link{rpart}} object to a \code{data.tree} structure
#'
#' @param x the \code{rpart} object to be converted
#' @param digits the number of digits to be used for numeric values in labels
#' @param use.n logical. Add cases to labels, see \code{\link{text.rpart}} for further
#'              information
#' @param ... any other argument to be passed to generic sub implementations
#'
#' @return a \code{data.tree} object. The tree contains a field \code{rpart.id} which
#'         references back to the original node id in the row names of the \code{rpart} object.
#' @export
#'
#' @examples
#' if (require(rpart)) {
#'    fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#'    as.Node(fit)
#' }
#' @family as.Node
as.Node.rpart <- function(x, 
                          digits = getOption("digits") - 3,
                          use.n  = FALSE,
                          ...) {
  frame       <- x$frame
  ylevels     <- attr(x, "ylevels")
  nodes       <- as.numeric(rownames(frame))
  leaves      <- frame$var == "<leaf>"
  leaf_labels <- x$functions$text(
    yval   = if (is.null(frame$yval2)) frame$yval[leaves] else frame$yval2[leaves, ], 
    dev    = frame$dev[leaves], 
    wt     = frame$wt[leaves], 
    ylevel = ylevels, 
    digits = digits, 
    n      = frame$n[leaves], 
    use.n  = use.n)
  node_labels <- setNames(c(labels(x)[which(!leaves) + 1L],
                            leaf_labels),
                          c(nodes[!leaves], nodes[leaves]))
  network_df  <- data.frame(from     = node_labels[as.character(floor(nodes[-1L] / 2L))],
                            to       = node_labels[as.character(nodes[-1L])],
                            rpart.id = nodes[-1L])
  tree <- FromDataFrameNetwork(network_df)
  tree$rpart.id <- nodes[1L]
  tree
}