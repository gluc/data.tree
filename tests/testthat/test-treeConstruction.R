#library(data.tree)
context("tree construction")

data(acme)

test_that("isRoot", {
  data(acme)
  
  expect_equal(acme$isRoot, TRUE)
  
  expect_equal(acme$IT$isRoot, FALSE)
  
  expect_equal(acme$IT$`Go agile`$isRoot, FALSE)
})


test_that("count", {
  expect_equal(acme$count, 3)
})

test_that("totalCount", {
  
  expect_equal(acme$totalCount, 11)
})

test_that("Climb", {
  
  
  node <- Climb(acme, "Accounting", "New Accounting Standards")
  expect_equal(node$name, "New Accounting Standards")
  
  node <- Climb(acme, "Not existing node")
  expect_equal(node, NULL)
  
})

test_that("isLeaf", {
  
  node <- Climb(acme, "Accounting", "New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})


test_that("level", {
  
  expect_equal(acme$isLeaf, FALSE)
  
  accounting <- Climb(acme, "Accounting")
  expect_equal(accounting$isLeaf, FALSE)
  
  node <- accounting$Climb("New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})




