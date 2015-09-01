context("tree conversion igraph")


test_that("as.Node.igraph undirected", {
  data(acme)
  ig <- as.igraph.Node(acme, "p", c("level", "isLeaf"), directed = FALSE)
  #expect_true(is_hierarchical(ig))
  expect_false(igraph::is_directed(ig))
  expect_equal(igraph::gsize(ig), acme$totalCount - 1)
})


test_that("as.Node.igraph directed", {
  data(acme)
  ig <- as.igraph.Node(acme, "p", c("level", "isLeaf"), directed = TRUE)
  #expect_true(is_hierarchical(ig))
  expect_true(igraph::is_directed(ig))
  expect_equal(igraph::gsize(ig), acme$totalCount - 1)
})



