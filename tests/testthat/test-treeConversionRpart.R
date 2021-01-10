context("tree conversion rpart")

test_that("Conversion from rpart", {
  skip_if_not_installed("rpart")

  fit  <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
  tree <- as.Node(fit)
  expect_equal(tree$totalCount,
               NROW(fit$frame))
  expect_true(tree$isBinary)
  expect_equal(tree$leafCount, 
               sum(fit$frame$var == "<leaf>"))
  expect_true(all(tree$Get("name", filterFun = isNotLeaf) %in% labels(fit)))
  expect_equivalent(tree$Get("rpart.id"), 
                    as.numeric(rownames(fit$frame)))
})
