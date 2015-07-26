context("util")


test_that("create dummy", {
  lvls <- 3
  children <- 4
  t <- CreateDummyTree(lvls, children)
  expect_equal(t$leafCount, children ^ lvls)
  expect_equal(t$depth, lvls + 1)
  expect_true(all(t$Get(function(x) x$count, filterFun = isNotLeaf) == 4))
})


