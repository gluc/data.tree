context("tree conversion data.frame")

data(acme)



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


test_that("ToDataFrameTable", {
  data(acme)
  acme$myfield <- "yes"
  acmedf <- ToDataFrameTable(acme, myp = "p", "cost", "myfield", pstr = function(x) x$pathString)
  expect_equal(names(acmedf), c("myp", "cost", "myfield", "pstr"))
  expect_equal(acmedf[2, 4], "Acme Inc./Accounting/New Accounting Standards")
  expect_equal(nrow(acmedf), acme$leafCount)
  expect_true(all(acmedf$myfield == "yes"))
})




test_that("ToDataFrameTaxonomy", {
  data(acme)
  acmedf <- ToDataFrameTaxonomy(acme, "p")
  expect_equal(acmedf$children, c("Accounting", "Research", "IT", "New Software", "New Accounting Standards", "New Product Line", "New Labs", "Outsource", "Go agile", "Switch to R"))
  expect_equal(acmedf$parents, c("Acme Inc.", "Acme Inc.", "Acme Inc.", "Accounting", "Accounting", "Research", "Research", "IT", "IT", "IT"))
})

