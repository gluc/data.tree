# nocov start
.onLoad <- function(libname, pkgname) {
  if(getRversion() >= "3.6.0") {
    # register S3-methods from Suggested packages
    s3_register("igraph::as.igraph", "Node")
    s3_register("ape::as.phylo", "Node")
  }
  invisible()
}
# nocov end