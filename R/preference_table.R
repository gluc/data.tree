#' @export
setPreference <- function(p, catA, catB, preference) {
  p[p$a==catA   & p$b==catB   ,"pref"] <- preference
  invisible (p)
}