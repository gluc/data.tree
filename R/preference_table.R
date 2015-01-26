#' @export
setPreference <- function(p, catA, catB, preference) {
  p[p$a==catA   & p$b==catB   ,"pref"] <- preference
  invisible (p)
}

#' @export
printForInput <- function(preferenceTable) {
  variableName <- as.list(match.call()[-1])[1]
  do.call(function(a, b, pref) printLineForInput(a, b, pref, variableName), preferenceTable)
}


printLineForInput <- function(a, b, pref, variableName) {
  cat(paste0(variableName, " <- setPreference(", variableName, ", \"", a, "\", \"", b, "\", ", pref, ")\n"))
}