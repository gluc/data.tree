context("util")


test_that("create dummy", {
  lvls <- 3
  children <- 4
  t <- CreateDummyTree(lvls, children)
  expect_equal(t$leafCount, children ^ lvls)
  expect_equal(t$depth, lvls + 1)
  expect_true(all(t$Get(function(x) x$count, filterFun = isNotLeaf) == 4))
})




test_that("PruneNaive 1", {
  data(acme)
  acme1 <- data.tree:::PruneNaive(acme, limit = 1)
  expect_equal(acme1$totalCount, 2)
  expect_equal(acme1$children[[1]]$name, "... 3 nodes w/ 7 sub")
})




test_that("PruneNaive 2", {
  data(acme)
  acme1 <- data.tree:::PruneNaive(acme, limit = 6)
  expect_equal(acme1$totalCount, 5)
  expect_equal(acme1$children[[2]]$name, "... 2 nodes w/ 5 sub")
})