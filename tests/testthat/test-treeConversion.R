context("tree conversion")

data(acme)

test_that("as.list.Node explicit", {
  data(acme)
  
  l <- acme$ToList(mode = "explicit")
    
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('name', 'children'), names(l))
  expect_equal(c('children'), names(l$children$Research))
  expect_equal(0.9, l$children$Research$children$`New Labs`$p)
 
})


test_that("as.list.Node explicit nameName=name", {
  data(acme)
  
  l <- acme$ToList(mode = "explicit", nameName = 'name')
  
  expect_equal(class(l), "list")
  expect_equal(length(l), 2)
  expect_equal(names(l), c('name', 'children'))
  expect_equal(names(l$children$Research), c('name', 'children'))
  expect_equal(l$children$Research$children$`New Labs`$p, 0.9)
  
})

test_that("as.list.Node explicit nameName=id", {
  data(acme)
  
  l <- acme$ToList(mode = "explicit", nameName = 'id')
  
  expect_equal(class(l), "list")
  expect_equal(length(l), 2)
  expect_equal(names(l), c('id', 'children'))
  expect_equal(names(l$children$Research), c('id', 'children'))
  expect_equal(l$children$Research$children$`New Labs`$p, 0.9)
  
})


test_that("as.list.Node simple", {
  
  data(acme)
  l <- acme$ToList()
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c("name", "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})

test_that("as.list.Node simple unname no effect", {
  
  data(acme)
  l <- acme$ToList()
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c("name", "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})


test_that("as.list.Node simple nameName=name", {
  
  data(acme)
  l <- acme$ToList(nameName = 'name')
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c('name', "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("name", "New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})

test_that("as.list.Node explicit nameName=id", {
  
  
  l <- acme$ToList(nameName = 'id')
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c('id', "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("id", "New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})


test_that("as.Node.list", {
  data(acme)
  n <- as.Node(acme$ToList())
  
  expect_equal("Acme Inc.", n$name)
  expect_equal(3, n$count)
  expect_equal(11, n$totalCount)
  expect_equal(0.05, n$Find("IT", "Go agile")$p)
  
})


test_that("as.list.Node unname", {
  data(acme)
  l <- acme$ToList(mode = "explicit", unname = TRUE, nameName = 'id', childrenName = 'sub')
  
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


test_that("as.data.frame.Node", {
  data(acme)
  acmedf <- as.data.frame(acme, 
                          row.names = NULL, 
                          optional = FALSE, 
                          myp = 'p', 
                          'cost', 
                          pstr = function(x) x$pathString,
                          sg = acme$Get( function(x) x$p)
                          )
  expect_equal(names(acmedf), c("levelName", "myp", "cost", "pstr", "sg"))
  expect_equal(acmedf[2, 4], "Acme Inc./Accounting")
  expect_equal(acmedf$sg, acmedf$sg)
})


test_that("as.data.frame.Node", {
  data(acme)
  acme$myfield <- "yes"
  acmedf <- acme$ToDataFrameTable(myp = "p", "cost", "myfield", pstr = function(x) x$pathString)
  expect_equal(names(acmedf), c("myp", "cost", "myfield", "pstr"))
  expect_equal(acmedf[2, 4], "Acme Inc./Accounting/New Accounting Standards")
  expect_equal(nrow(acmedf), acme$leafCount)
  expect_true(all(acmedf$myfield == "yes"))
})

