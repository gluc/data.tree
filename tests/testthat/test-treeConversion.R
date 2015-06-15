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
  
  n <- as.Node(acme$ToList())
  
  expect_equal("Acme Inc.", n$name)
  expect_equal(3, n$count)
  expect_equal(11, n$totalCount)
  expect_equal(0.05, n$Find("IT", "Go agile")$p)
  
})


test_that("as.list.Node unname", {
  data(acme)
  l <- acme$ToList(unname = TRUE, nameName = 'id', childrenName = 'sub')
  
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('id', 'sub'), names(l))
  expect_equal(0.9, l$sub[[2]]$sub[[2]]$p)
  expect_equal('New Product Line', l$sub[[2]]$sub[[1]]$id)
  
})

test_that("as.Node.list unname", {
  
  l <- acme$ToList(unname = TRUE, nameName = 'id', childrenName = 'sub')
  n <- as.Node(l, nameName = 'id', childrenName = 'sub')
  
  expect_equal("Acme Inc.", n$name)
  expect_equal(3, n$count)
  expect_equal(11, n$totalCount)
  expect_equal(0.05, n$Find("IT", "Go agile")$p)
  
})


test_that("as.Node.data.frame", {
  data(acme)
  acmedf <- as.data.frame(acme, row.names = NULL, optional = FALSE, 'p', 'cost', 'pathString')
  acme2 <- as.Node(acmedf, na.rm = TRUE)
  expect_equal(as.list(acme), as.list(acme2))
  expect_equal(as.data.frame(acme, row.names = NULL, optional = FALSE, 'p', 'cost'), as.data.frame(acme2, row.names = NULL, optional = FALSE, 'p', 'cost'))
  #test that if they are not different it fails
  # acc2 <- acme2$Find("Accounting")
  # acc2$newField <- 'new value'
  # expect_equal(as.list(acme), as.list(acme2))
  
})

