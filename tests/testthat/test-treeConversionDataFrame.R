context("tree conversion data.frame")

data(acme)



test_that("as.Node.data.frame", {
  data(acme)
  acmedf <- as.data.frame(acme, row.names = NULL, optional = FALSE, 'p', 'cost', 'pathString')
  acme2 <- as.Node(acmedf, na.rm = TRUE)
  expect_equal(as.list(acme), as.list(acme2))
  expect_true(is.null(acme2$children[[1]]$p))
  expect_equal(as.data.frame(acme, row.names = NULL, optional = FALSE, 'p', 'cost'), as.data.frame(acme2, row.names = NULL, optional = FALSE, 'p', 'cost'))
  #test that if they are not different it fails
  # acc2 <- acme2$Climb("Accounting")
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


test_that("ToDataFrameTable", {
  data(acme)
  acme$myfield <- "yes"
  acmedf <- ToDataFrameTable(acme, myp = "p", "cost", "myfield", pstr = function(x) x$pathString)
  expect_equal(names(acmedf), c("myp", "cost", "myfield", "pstr"))
  expect_equal(acmedf[2, 4], "Acme Inc./Accounting/New Accounting Standards")
  expect_equal(nrow(acmedf), acme$leafCount)
  expect_true(all(acmedf$myfield == "yes"))
})




test_that("ToDataFrameNetwork climb", {
  data(acme)
  acmedf <- ToDataFrameNetwork(acme, "p", direction = "climb")
  expect_equal(names(acmedf), c("from", "to", "p"))
  expect_equal(acmedf$to, c("Accounting", "Research", "IT", "New Software", "New Accounting Standards", "New Product Line", "New Labs", "Outsource", "Go agile", "Switch to R"))
  expect_equal(acmedf$from, c("Acme Inc.", "Acme Inc.", "Acme Inc.", "Accounting", "Accounting", "Research", "Research", "IT", "IT", "IT"))
})


test_that("ToDataFrameNetwork descend", {
  data(acme)
  acmedf <- ToDataFrameNetwork(acme, "p", direction = "descend")
  expect_equal(names(acmedf), c("from", "to", "p"))
  expect_equal(acmedf$from, c("Accounting", "Research", "IT", "New Software", "New Accounting Standards", "New Product Line", "New Labs", "Outsource", "Go agile", "Switch to R"))
  expect_equal(acmedf$to, c("Acme Inc.", "Acme Inc.", "Acme Inc.", "Accounting", "Accounting", "Research", "Research", "IT", "IT", "IT"))
})




test_that("ToDataFrame sub-tree", {
  data(acme)
  it <- acme$Climb("IT")
  df <- ToDataFrameTree(it)
  expect_equal(dim(df), c(4, 1))
  expect_equal(str_sub(df[1, 1], 1, 2), 'IT')  
})


test_that("ToDataFrameTypeCol level", {
  data(acme)
  acme$IT$Outsource$AddChild("India")
  acme$IT$Outsource$AddChild("Poland")
  
  acmedf <- ToDataFrameTypeCol(acme)
  expect_equal(names(acmedf), c('level_1', 'level_2', 'level_3', 'level_4'))
  expect_true( all(acmedf$level_1 == 'Acme Inc.'))
  expect_equal(acmedf$level_2, c('Accounting', 'Accounting', 'Research', 'Research', 'IT', 'IT', 'IT', 'IT'))
  expect_equal(acmedf$level_3, c('New Software', 'New Accounting Standards', 'New Product Line', 'New Labs', 'Outsource', 'Outsource', 'Go agile', 'Switch to R'))
  expect_equal(acmedf$level_4, c(NA, NA, NA, NA, 'India', 'Poland', NA, NA))  
})

test_that("ToDataFrameTypeCol type", {
  data(acme)
  acme$IT$Outsource$AddChild("India")
  acme$IT$Outsource$AddChild("Poland")
  acme$Set(type = c('company', 'department', 'project', 'project', 'department', 'project', 'project', 'department', 'program', 'project', 'project', 'project', 'project'))
  
  acmedf <- ToDataFrameTypeCol(acme, type = 'type', prefix = NULL)
  expect_equal(names(acmedf), c('company', 'department', 'program', 'project'))
  expect_true( all(acmedf$company == 'Acme Inc.'))
  expect_equal(acmedf$department, c('Accounting', 'Accounting', 'Research', 'Research', 'IT', 'IT', 'IT', 'IT'))
  expect_equal(acmedf$program, c(NA, NA, NA, NA, 'Outsource', 'Outsource', NA, NA))  
  expect_equal(acmedf$project, c('New Software', 'New Accounting Standards', 'New Product Line', 'New Labs', 'India', 'Poland', 'Go agile', 'Switch to R'))
})


test_that("FromDataFrameTable col-levels", {
  data(acme)
  acme$Set(floor = c(1, 2, 3),  filterFun = function(x) x$level == 2)
  x <- ToDataFrameTable(acme, "pathString", "floor", "p", "cost") 
  xN <- FromDataFrameTable(x, colLevels = list(NULL, "floor", c("p", "cost")), na.rm = TRUE)
  expect_equal(xN$Climb("Accounting")$floor, 1)
  expect_true(is.null(xN$Climb("Accounting", "New Accounting Standards")$floor))
  expect_true(is.null(xN$floor))
  expect_equal(xN$Climb("Accounting", "New Accounting Standards")$p, 0.75)
  
})

test_that("FromDataFrameNetwork descend", {
  data(acme)
  x <- ToDataFrameNetwork(acme, "p", "cost", direction = "descend")
  xN <- FromDataFrameNetwork(x)
  expect_equal(xN$totalCount, acme$totalCount)
  expect_equal(xN$Get("name"), acme$Get("name"))
  expect_equal(xN$Get("p"), acme$Get("p"))
  expect_equal(xN$height, acme$height)
  expect_equal(xN$Get("level"), acme$Get("level"))
  expect_equal(xN$Get(function(x) x$parent$name), acme$Get(function(x) x$parent$name))
  expect_equal(xN$Get("isLeaf"), acme$Get("isLeaf"))
})


test_that("FromDataFrameNetwork climb", {
  data(acme)
  x <- ToDataFrameNetwork(acme, "p", "cost", direction = "climb")
  xN <- FromDataFrameNetwork(x)
  expect_equal(xN$totalCount, acme$totalCount)
  expect_equal(xN$Get("name"), acme$Get("name"))
  expect_equal(xN$Get("p"), acme$Get("p"))
  expect_equal(xN$height, acme$height)
  expect_equal(xN$Get("level"), acme$Get("level"))
  expect_equal(xN$Get(function(x) x$parent$name), acme$Get(function(x) x$parent$name))
  expect_equal(xN$Get("isLeaf"), acme$Get("isLeaf"))
})


test_that("FromDataFrameNetwork order", {
  data(acme)
  x <- ToDataFrameNetwork(acme, "p", "cost")
  odr <- c(4, 1, 6, 8, 9, 10, 2, 7, 5, 3)
  x <- x[odr, ]
  xN <- FromDataFrameNetwork(x)
  expect_equal(xN$Get("name"), acme$Get("name"))

})





