context("util")


test_that("createRegular", {
  lvls <- 3
  children <- 4
  t <- CreateRegularTree(height = 3, branchingFactor = 4)
  expect_equal(t$leafCount, children ^ (lvls - 1))
  expect_equal(t$height, lvls)
  expect_true(all(t$Get(function(x) x$count, filterFun = isNotLeaf) == 4))
})


test_that("createRandomTree", {
  t <- CreateRandomTree(nodes = 100)
  expect_equal(t$totalCount, 101)

})




test_that("PruneDist 1", {
  data(acme)
  acme1 <- data.tree:::PrintPruneDist(acme, limit = 1)
  expect_equal(acme1$totalCount, 2)
  expect_equal(acme1$children[[1]]$name, "... 3 nodes w/ 7 sub")
})




test_that("PruneDist 2", {
  data(acme)
  acme1 <- data.tree:::PrintPruneDist(acme, limit = 5)
  expect_equal(acme1$totalCount, 8)
  expect_equal(acme1$IT$children[[1]]$name, "... 3 nodes w/ 0 sub")
})

test_that("PruneSimple", {
  data(acme)
  acme1 <- data.tree:::PrintPruneSimple(acme, limit = 5)
  expect_equal(acme1$totalCount, 5)
  expect_equal(acme1$children[[2]]$name, "... 2 nodes w/ 5 sub")
})