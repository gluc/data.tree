

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
PrintFixedDecimal <- function(x, digits = 3) {
  ifelse(is.na(x), "", sprintf(paste0("%.",digits, "f"),x))
}