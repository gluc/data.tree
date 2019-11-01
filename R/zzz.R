.onLoad <- function() {
  if(getRversion() >= "3.6.0") {
    S3method(ape::as.phylo, Node)
  }
}