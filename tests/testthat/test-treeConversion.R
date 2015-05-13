context("tree conversion")

data(acme)

test_that("as.list.Node", {
  
  
  l <- acme$ToList()
    
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('name', 'children'), names(l))
  expect_equal(0.9, l$children$Research$children$`New Labs`$p)
 
})


test_that("as.Node.list", {
  
  l <- as.Node(acme$ToList())
  
  expect_equal("Acme Inc.", l$name)
  expect_equal(3, acme$count)
  expect_equal(11, acme$totalCount)
  expect_equal(0.05, acme$Find("IT", "Go agile")$p)
  
})

