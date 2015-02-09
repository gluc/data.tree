library(ahp)
context("Tree construction")

createTestTree <- function() {
  acme <- Node$new("Acme Inc.")
  accounting <- acme$AddChild("Accounting")
  software <- accounting$AddChild("New Software")
  standards <- accounting$AddChild("New Accounting Standards")
  research <- acme$AddChild("Research")
  newProductLine <- research$AddChild("New Product Line")
  newLabs <- research$AddChild("New Labs")
  it <- acme$AddChild("IT")
  outsource <- it$AddChild("Outsource")
  agile <- it$AddChild("Go agile")
  goToR <- it$AddChild("Switch to R")
  return (acme)
}

test_that("isRoot", {
  tree <- createTestTree()
  expect_equal(tree$isRoot, TRUE)
  
  it <- tree$Find("IT")
  expect_equal(it$isRoot, FALSE)
  
  agile <- it$Find("Go agile")
  expect_equal(agile$isRoot, FALSE)
})


test_that("count", {
  tree <- createTestTree()
  expect_equal(tree$count, 3)
})

test_that("totalCount", {
  tree <- createTestTree()
  expect_equal(tree$totalCount, 11)
})

test_that("Find", {
  tree <- createTestTree()
  
  node <- tree$Find("Accounting", "New Accounting Standards")
  expect_equal(node$name, "New Accounting Standards")
  
  node <- tree$Find("Not existing node")
  expect_equal(node, NULL)
  
})

test_that("isLeaf", {
  tree <- createTestTree()
  node <- tree$Find("Accounting", "New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})


test_that("level", {
  tree <- createTestTree()
  expect_equal(tree$isLeaf, FALSE)
  
  accounting <- tree$Find("Accounting")
  expect_equal(accounting$isLeaf, FALSE)
  
  node <- accounting$Find("New Accounting Standards")
  expect_equal(node$isLeaf, TRUE)
  
})




