

#' Format a number as a percentage, suitable for printing
#' 
#' @param x A number
#' @param digits The number of digits to print
#' @param format The format to use
#' @param ... Any other argument passed to formatC
#' @return A string corresponding to x, suitable for printing
#' @seealso formatC
#' @export FormatPercent
FormatPercent <- function(x, digits = 2, format = "f", ...) {
  ifelse(is.na(x), "", paste(formatC(100 * x, format = format, digits = digits, ...), " %"))
}


#' @export
PrintFixedDecimal <- function(x, digits = 3) {
  ifelse(is.na(x), "", sprintf(paste0("%.",digits, "f"),x))
}