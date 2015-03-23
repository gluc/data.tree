context("tree methods")

data(acme)

test_that("Find NULL", {
  
  expect_equal(acme$Find('X'), NULL)
  expect_equal(acme$Find('X', 'Y', 'Z'), NULL)
  expect_equal(acme$Find('IT', 'X'), NULL)
 
})


test_that("Find Equivalent", {
  
  expect_equal(acme$Find('IT', 'Go agile'), acme$Find('IT')$Find('Go agile'))
  
})

test_that("Find 3rd Level", {
  acme$Find('IT', 'Go agile')$AddChild('MyTest')$AddChild('MyTest2')
  expect_equal("MyTest2", acme$Find('IT', 'Go agile', 'MyTest', 'MyTest2')$name )
  data(acme)
})



test_that("Get filter", {
  acme$Set(myvalue = c(1.3, 1.5, 0.9, 1, 2, 1.1, 0.8, -1, 0.7, 1.0, 1.01))
  
  myFilter <- function(x) {
    return (!is.null(x$myvalue) && x$myvalue > 1)
  }
  
  
  get <- acme$Get("myvalue", filterFun = myFilter)
  #NOTE: 1.01 is filtered out because its parent is -1!
  exp <- c(1.3, 1.5, 2, 1.1)
  names(exp) <- c('Acme Inc.', 'Accounting', 'Research', 'New Product Line')
  
  expect_equal(get, exp)
  data(acme)
  
})
  
  

test_that("Get pre-order", {
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
  get <- acme$Find('Research', 'New Labs')$Get("name", traversal = "ancestor")
  
  exp <- c('New Labs', 
           'Research', 
           'Acme Inc.')
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})



test_that("Get format", {
  
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



test_that("Get filter", {
  
})



test_that("Aggregate", {
  
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

