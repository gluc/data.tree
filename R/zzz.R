.onLoad <- function(libname, pkgname) {
  if(getRversion() >= "3.6.0") {
    s3_register("ape::as.phylo", "Node")
  }
  invisible()
}