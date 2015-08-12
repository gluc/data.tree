context("tree conversion ape")


test_that("as.Node.phylo owls", {
  t <- "owls(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"

  p <- ape::read.tree(text = t)
  n <- as.Node(p, replaceUnderscore = F)
  expect_equal(n$totalCount, 7)
  expect_equal(as.vector(n$Get("name")), c("5", "6", "7", "Strix_aluco", "Asio_otus", "Athene_noctua", "Tyto_alba"))
  expect_equal(as.vector(n$Get("level")), c(1, 2, 3, 4, 4, 3, 2))
  
})

test_that("as.Node.phylo height", {
  t <- "(A:5,B:5,(C:10,D:10)E:5):0;"
  
  p <- ape::read.tree(text = t)
  n <- as.Node(p)
  expect_equal(n$totalCount, 6)
  expect_equal(as.vector(n$Get("name")), c("", "A", "B", "E", "C", "D"))
  expect_equal(as.vector(n$Get("level")), c(1, 2, 2, 2, 3, 3))
  expect_equal(as.vector(n$Get("plotHeight")), c(15, 10, 10, 10, 0, 0))
})

test_that("as.Node.phylo no height", {
  t <- "(A,B,(C,D)E)F;"
  
  p <- ape::read.tree(text = t)
  n <- as.Node(p)
  expect_equal(n$totalCount, 6)
  expect_equal(as.vector(n$Get("name")), c("F", "A", "B", "E", "C", "D"))
  expect_equal(as.vector(n$Get("level")), c(1, 2, 2, 2, 3, 3))
  expect_true(all(is.na(n$Get("edgeLength"))))
})



test_that("as.Node.phylo height non standard", {
  t <- "(A:5,B:5,(C:10,D:10):5):0;"
  
  p <- ape::read.tree(text = t)
  n <- as.Node(p, heightName = "edge")
  expect_equal(n$totalCount, 6)
  expect_equal(as.vector(n$Get("name")), c("5", "A", "B", "6", "C", "D"))
  expect_equal(as.vector(n$Get("level")), c(1, 2, 2, 2, 3, 3))
  expect_equal(as.vector(n$Get("edge")), c(15, 10, 10, 10, 0, 0))
})



test_that("as.phylo.Node heightAttributeName", {
  
  data(acme)
  #needs explicit generics as library ape is not loaded
  p <- as.phylo.Node(acme)
  n <- as.Node(p)
  
  expect_equal(n$Get("name"), acme$Get("name"))
  
})


test_that("as.phylo.Node heightAttributeName", {

  data(acme)
  height <- function(x) x$edgeHeight <- DefaultPlotHeight(x) + 1
  acme$Do(height)
  #needs explicit generics as library ape is not loaded
  
  p <- as.phylo.Node(acme, heightAttributeName = "edgeHeight")
  n <- as.Node(p)
  
  expect_equal(n$Get("name"), acme$Get("name"))
  gh <- function(x) {
    if (x$isRoot) {
      x$edgeHeight <- 0
      return()
    }
    if (x$parent$isRoot) ph <- 0
    else ph <- x$parent$edgeLength
    x$edgeHeight <- x$edgeLength - ph
  }
  n$Do(gh)
  expect_equal(n$Get("edgeLength"), acme$Get("edgeLength"))
  
})


test_that("GetPhyloNumber node", {
  data(acme)
  acme$Do(function(x) x$phyloNr <- GetPhyloNr(x, "node"))
  expect_equal(as.vector(acme$Get("phyloNr")), c(8,9,1,2,10,3,4,11,5,6,7))
})

test_that("GetPhyloNumber edge", {
  data(acme)
  acme$Do(function(x) x$phyloNr <- GetPhyloNr(x, "edge"))
  expect_equal(as.vector(acme$Get("phyloNr")), c(NA, 1:10))
})

