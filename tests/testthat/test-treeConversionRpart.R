context("tree conversion rpart")

check_rpart_installed <- function() {
  if (!suppressWarnings(require(rpart))) {
    skip("library 'rpart' not available")
  }
}

test_that("Conversion from rpart", {
  check_rpart_installed()
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