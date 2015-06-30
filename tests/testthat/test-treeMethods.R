context("tree methods")



test_that("Find NULL", {
  data(acme)
  expect_equal(acme$Find('X'), NULL)
  expect_equal(acme$Find('X', 'Y', 'Z'), NULL)
  expect_equal(acme$Find('IT', 'X'), NULL)
 
})


test_that("Find Equivalent", {
  data(acme)
  expect_equal(acme$Find('IT', 'Go agile'), acme$Find('IT')$Find('Go agile'))
  
})



test_that("Find 3rd Level", {
  data(acme)
  acme$Find('IT', 'Go agile')$AddChild('MyTest')$AddChild('MyTest2')
  expect_equal("MyTest2", acme$Find('IT', 'Go agile', 'MyTest', 'MyTest2')$name )
  
})



test_that("Get prune", {
  data(acme)
  acme$Set(myvalue = c(1.3, 1.5, 0.9, 1, 2, 1.1, 0.8, -1, 0.7, 1.0, 1.01))
  
  myFilter <- function(x) {
    return (!is.null(x$myvalue) && x$myvalue > 1)
  }
  
  
  get <- acme$Get("myvalue", pruneFun = myFilter)
  #NOTE: 1.01 is filtered out because its parent is -1!
  exp <- c(1.3, 1.5, 2, 1.1)
  names(exp) <- c('Acme Inc.', 'Accounting', 'Research', 'New Product Line')
  
  expect_equal(get, exp)
  
})


test_that("Get filter", {
  data(acme)
  acme$Set(myvalue = c(1.3, 1.5, 0.9, 1, 2, 1.1, 0.8, -1, 0.7, 1.0, 1.01))
  
  myFilter <- function(x) {
    return (!is.null(x$myvalue) && x$myvalue > 1)
  }
  
  
  get <- acme$Get("myvalue", filterFun = myFilter)
  
  exp <- c(1.3, 1.5, 2, 1.1, 1.01)
  names(exp) <- c('Acme Inc.', 'Accounting', 'Research', 'New Product Line', 'Switch to R')
  
  expect_equal(get, exp)
  
})
  


test_that("Get pre-order", {
  data(acme)
  get <- acme$Get("name", traversal = "pre-order")
  
  exp <- c('Acme Inc.', 
           'Accounting', 
           'New Software', 
           'New Accounting Standards', 
           'Research', 
           'New Product Line', 
           'New Labs',
           'IT', 
           'Outsource', 
           'Go agile',
           'Switch to R' 
           )
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})


test_that("Get post-order", {
  data(acme)
  get <- acme$Get("name", traversal = "post-order")
  exp <- c('New Software',
           'New Accounting Standards', 
           'Accounting',
           'New Product Line', 
           'New Labs', 
           'Research', 
           'Outsource', 
           'Go agile', 
           'Switch to R', 
           'IT', 
           'Acme Inc.')
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})


test_that("Get ancestor", {
  data(acme)
  get <- acme$Find('Research', 'New Labs')$Get("name", traversal = "ancestor")
  
  exp <- c('New Labs', 
           'Research', 
           'Acme Inc.')
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})



test_that("Get format", {
  
  data(acme)
  
  calculateAggregateChildCost <- function(node, fun) {
    if (node$isLeaf) return(node$cost)
    fun(sapply(node$children, function(x) x$averageCost))
  }
  
  myFormat <- function(x) {
    format(x, nsmall=2, scientific = FALSE)
  }
  
  get <- acme$Get(calculateAggregateChildCost, mean, traversal = "post-order", assign = "averageCost", format = myFormat)["New Product Line"]
  
  expect_equal(as.character(get), "2000000.00")
  

})



test_that("Get assign", {
  
  data(acme)
  
  calculateAggregateChildCost <- function(node, fun) {
    if (node$isLeaf) return(node$cost)
    fun(sapply(node$children, function(x) x$averageCost))
  }
  
  myFormat <- function(x) {
    format(x, nsmall=2, scientific = FALSE)
  }
  
  acme$Get(calculateAggregateChildCost, mean, traversal = "post-order", assign = "averageCost", format = myFormat)
  get <- acme$Get('averageCost', traversal = "post-order")["New Product Line"]
  
  
  expect_equal(as.numeric(get), 2000000)
  
  
})



test_that("post-order", {
  
  data(acme)
  
  acme$Set(myval = 1:acme$totalCount, traversal = "post-order")
  
  expect_equal(acme$myval, 11)
  expect_equal(acme$Find("Research")$myval, 6)
  
})


test_that("level", {
  
  data(acme)
  
  acme$Set(myval = 1:acme$totalCount, traversal = "level")
  
  expect_equal(acme$myval, 1)
  expect_equal(acme$Find("Research")$myval, 3)
  expect_equal(acme$Find("IT", "Go agile")$myval, 10)
  
})


test_that("prune", {
  
  data(acme)
  
  acme$Set(myval = 1:8, pruneFun = function(x) x$name != "Research")
  
  expect_equal(acme$myval, 1)
  expect_true(is.null(acme$Find("Research")$myval))
  expect_true(is.null(acme$Find("Research", "New Labs")$myval))
  expect_equal(acme$Find("IT", "Go agile")$myval, 7)
  
})


test_that("filter", {
  
  data(acme)
  
  acme$Set(myval = 1:10, filterFun = function(x) x$name != "Research")
  
  expect_equal(acme$myval, 1)
  expect_true(is.null(acme$Find("Research")$myval))
  expect_equal(acme$Find("Research", "New Labs")$myval, 6)
  expect_equal(acme$Find("IT", "Go agile")$myval, 9)
  
})




test_that("isBinary", {
  
  node <- Node$new("0")
  
  addBinChildren <- function(node, n) {
    for (i in 1:2) {
      child <- node$AddChild(paste0(node$name, ".", i))
      if (n > 0) addBinChildren(child, n-1)
    }
  }
  
  addBinChildren(node, 3)
  
  expect_true(node$isBinary)
  
})


test_that("in-order", {
  
  node <- Node$new("0")
  
  addBinChildren <- function(node, n) {
    for (i in 1:2) {
      child <- node$AddChild(paste0(node$name, ".", i))
      if (n > 0) addBinChildren(child, n-1)
    }
  }
  
  addBinChildren(node, 2)
  
  #make sure the tree is irregular
  addBinChildren(node$Find("0.1", "0.1.2", "0.1.2.1"), 0)
  
  g <- node$Get("name", traversal = "in-order")
  
  expected <- c("0.1.1.1",
                "0.1.1",
                "0.1.1.2",
                "0.1",
                "0.1.2.1.1",
                "0.1.2.1",
                "0.1.2.1.2",
                "0.1.2",
                "0.1.2.2",
                "0",
                "0.2.1.1",
                "0.2.1",
                "0.2.1.2",
                "0.2",
                "0.2.2.1",
                "0.2.2",
                "0.2.2.2")
  names(expected) <- expected
  expect_equal(g, expected)
  
})


test_that("Set recycling", {
  
  
    data(acme)
    acme$Find("Accounting", "New Accounting Standards")$AddChild("ICI 320")
    acme$Set(myval = 1:6)
    expect_equal(acme$myval, 1)
    expect_equal(acme$Find("Research", "New Labs")$myval, 2)
    expect_equal(acme$Find("IT", "Go agile")$myval, 5)
       
})





test_that("Aggregate", {
  data(acme)
  expect_equal(acme$Aggregate("cost", sum), 4950000)
  
})


test_that("Sort", {
  
  acme$Get("Aggregate", "cost", sum, assign = "totalCost")
  acme$Sort("totalCost", decreasing = FALSE)
  get <- acme$Get('totalCost')
  exp <- c(4950000, 700000, 50000, 250000, 400000, 1500000, 500000, 1000000, 2750000, 750000, 2000000)
  names(exp) <- c('Acme Inc.',
                  'IT',
                  'Switch to R', 
                  'Go agile',
                  'Outsource',
                  'Accounting',
                  'New Accounting Standards',
                  'New Software',
                  'Research',
                  'New Labs',
                  'New Product Line'
                  )
  
  expect_equal(get, exp)
  
  acme$Sort("totalCost", decreasing = TRUE)
  get <- acme$Get('totalCost')
  
  expect_false(identical(all.equal(get, exp), TRUE))
  
  
})


test_that("Clone", {
  data(acme)
  n <- acme$Clone()
  
  expect_equal(class(n), class(acme))
  expect_equal(n$name, acme$name)
  expect_equal(n$count, acme$count)
  expect_equal(n$totalCount, acme$totalCount)
  expect_equal(n$Find("IT", "Go agile")$p, acme$Find("IT", "Go agile")$p)
  
  expect_equal(n$ToList(), acme$ToList())
  acme2 <- acme
  expect_identical(acme, acme2)
  
  #expect_false(n, is_identical_to(acme))
  n$name <- 'Acme2'
  expect_false(n$name == acme$name)
  
})

test_that("Clone formatter", {
  data(acme)
  acme$formatters$count <- FormatFixedDecimal
  n <- acme$Clone()
  
  expect_equal(names(n$formatters), "count")
  expect_true(is.function(n$formatters$count))
  
})


test_that("Clone subtree", {
  data(acme)
  it <- acme$Find("IT")
  n <- it$Clone()
  
  expect_equal(class(n), class(it))
  expect_equal(n$name, it$name)
  expect_equal(n$count, it$count)
  expect_equal(n$totalCount, it$totalCount)
  expect_equal(n$Find("Go agile")$p, it$Find("Go agile")$p)
    
})


test_that("Aggregate", {
  data(acme)
  
  g <- acme$Get(Aggregate, "p", sum)
  expect_false(is.na(g[1]))
  expect_equal(3.65, as.vector(g[1]))  
})


test_that("Aggregate function", {
  data(acme)
  
  g <- acme$Get(Aggregate, function(x) x$p * x$cost, sum)
  expect_false(is.na(g[1]))

})


test_that("Formatter Get", {
  data(acme)
  acme$formatters$p <- FormatPercent
  p <- acme$Get("p")
  expect_equal(p[["Go agile"]], "5.00 %")
})

test_that("Formatter Get Hierarchy", {
  data(acme)
  acme$formatters$p <- FormatPercent
  acme$p <- 1
  n <- acme$Find("IT")
  n$formatters$p <- FormatFixedDecimal
  p <- acme$Get("p")
  expect_equal(p[["Acme Inc."]], "100.00 %")
  expect_equal(p[["Outsource"]], "0.200")
  
  p <- acme$Get("p", format = FormatFixedDecimal)
  expect_equal(p[["Acme Inc."]], "1.000")

  p <- acme$Get("p", format = function(x) x)
  expect_equal(p[["Acme Inc."]], 1)
  expect_true(is.numeric(p[["Acme Inc."]]))
  expect_equal(p[["Outsource"]], 0.2)
  
  
  
})



test_that("Set pre-order", {
  data(acme)
  acme$Set(mycnt = 1:acme$totalCount)
  expect_equal( acme$Find("IT")$mycnt, 8)
})


test_that("Set post-order", {
  data(acme)
  acme$Set(mycnt = 1:acme$totalCount, traversal = "post-order")
  expect_equal( acme$Find("IT")$mycnt, 10)
  expect_equal( acme$mycnt, 11)
})


test_that("Set filter", {
  data(acme)
  acme$Set(mycnt = 1:3, filterFun = function(x) x$level == 1)
  expect_equal( acme$Find("IT")$mycnt, 3)
  expect_equal( acme$mycnt, NULL)
})



test_that("Revert", {
  data(acme)
  acme$Set(id = 1:acme$totalCount)
  acme$Revert()
  ids <- unname(acme$Get("id"))
  expected = c(1, 8, 11, 10, 9, 5, 7, 6, 2, 4, 3)
  expect_equal(ids, expected)
})


test_that("fieldsAll", {
  data(acme)
  fa <- acme$fieldsAll
  expect_equal(fa, c("cost", "p"))
})


test_that("depth", {
  data(acme)
  expect_equal(acme$depth, 3)
  expect_equal(acme$Find("IT")$depth, 2)
  acme$Find("IT", "Outsource")$AddChild("New")
  
  expect_equal(acme$depth, 4)
})


test_that("isRoot", {
  data(acme)
  expect_true(acme$isRoot)
  expect_false(acme$Find("IT")$isRoot)
  expect_equal(acme$Find("IT")$depth, 2)
  isRoot <- acme$Get("isRoot")
  expect_equal(sum(isRoot), 1)
  
})


test_that("isLeaf", {
  data(acme)
  expect_false(acme$isLeaf)
  expect_true(acme$Find("Research", "New Labs")$isLeaf)
  
  isLeaf <- acme$Get("isLeaf")
  leaves <- names(isLeaf)[isLeaf]
  exp <- c("New Software", "New Accounting Standards", "New Product Line", "New Labs", 
           "Outsource", "Go agile", "Switch to R")
  expect_equal(leaves, exp)
  
})
