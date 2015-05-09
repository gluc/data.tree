context("tree conversion")

data(acme)

test_that("as.list.Node", {
  
  
  l <- acme$ToList()
    
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('name', 'children'), names(l))
  expect_equal(0.9, l$children$Research$children$`New Labs`$p)
 
})

