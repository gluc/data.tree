#library(data.tree)
context("tree construction")

data(acme)

test_that("isRoot", {
  
  expect_equal(acme$isRoot, TRUE)
  
  it <- acme$Find("IT")
  expect_equal(it$isRoot, FALSE)
  
  agile <- it$Find("Go agile")
  expect_equal(agile$isRoot, FALSE)
})


test_that("count", {
  expect_equal(acme$count, 3)
})

test_that("totalCount", {
  
  expect_equal(acme$totalCount, 11)
})

test_that("Find", {
  
  
  node <- acme$Find("Accounting", "New Accounting Standards")
  expect_equal(node$name, "New Accounting Standards")
  
  node <- acme$Find("Not existing node")
  expect_equal(node, NULL)
  
})

test_that("isLeaf", {
  
  node <- acme$Find("Accounting", "New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})


test_that("level", {
  
  expect_equal(acme$isLeaf, FALSE)
  
  accounting <- acme$Find("Accounting")
  expect_equal(accounting$isLeaf, FALSE)
  
  node <- accounting$Find("New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})




