context("tree conversion dendrogram")


test_that("as.Node.dendrogram", {
  hc <- hclust(dist(USArrests), "ave")
  dend1 <- as.dendrogram(hc)
  root <- as.Node(dend1)
  expect_equal(root$totalCount, 99)
  expect_true(root$isBinary)
})


test_that("as.dendrogram.Node", {
  data(acme)
  acmed <- as.dendrogram(acme)
  expect_equal(class(acmed), "dendrogram")
  expect_equal(nobs(acmed), acme$leafCount)
  expect_equal(attr(acmed, "height"), 100)
})

