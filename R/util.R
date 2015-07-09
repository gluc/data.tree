

#' Format a Number as a Percentage
#' 
#' This utility method can be used as a format function when converting trees to a \code{data.frame}
#' 
#' @param x A number
#' @param digits The number of digits to print
#' @param format The format to use
#' @param ... Any other argument passed to formatC
#' @return A string corresponding to x, suitable for printing
#' @seealso formatC
#' @export
FormatPercent <- function(x, digits = 2, format = "f", ...) {
  ifelse(is.na(x), "", paste(formatC(100 * x, format = format, digits = digits, ...), "%"))
}

#' Format a Number as a Decimal
#' 
#' Simple function that can be used as a format function when converting trees to a \code{data.frame}
#' 
#' @param x a numeric scalar or vector
#' @param digits the number of digits to print after the decimal point
#' @return A string corresponding to x, suitable for printing
#' @export
FormatFixedDecimal <- function(x, digits = 3) {
  ifelse(is.na(x), "", sprintf(paste0("%.",digits, "f"),x))
}




#' Print a Node
#' 
#' Print a Node in a human-readable fashion.
#' 
#' @param x The Node
#' @param ... Additional parameters
#' 
#' @details Print the Node in a human-readable fashion.
#'
#' @export
print.Node <- function(x, ...) {
  print(as.data.frame(x, row.names = NULL, optional = FALSE, ...), na.print = "")
}



#'   Calculates the height of a \code{Node} given the hight of the root, 
#'   assuming that nodes are equally distributed. Useful for easy printing
#'   as dendrogram.
#'   
#'   @param node The node
#'   @param rootHeight The height of the root
Height <- function(node, rootHeight = 100) {
  if (node$isRoot) return ( rootHeight )
  if (node$isLeaf) return ( 0 )
  h <- Height(node$parent, rootHeight) * (1 - 1 / node$depth)
  return (h)
}
