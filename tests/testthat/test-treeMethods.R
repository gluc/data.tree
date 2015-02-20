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


test_that("Get pre-order", {
  get <- acme$Get("name", traversal = "pre-order")
  
  exp <- c('Acme Inc.', 
           'Research', 
           'New Labs', 
           'New Product Line', 
           'Accounting', 
           'New Software', 
           'New Accounting Standards', 
           'IT', 
           'Outsource', 
           'Switch to R', 
           'Go agile')
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})


test_that("Get post-order", {
  get <- acme$Get("name", traversal = "post-order")
  
  exp <- c('New Labs', 
           'New Product Line', 
           'Research', 
           'New Software', 
           'New Accounting Standards', 
           'Accounting', 'Outsource', 
           'Switch to R', 
           'Go agile', 
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
  
  get <- acme$Get(calculateAggregateChildCost, mean, traversal = "post-order", assign = "averageCost", format = myFormat)
  
  exp <- c("750000.00", "2000000.00", "1375000.00", "1000000.00", "500000.00", "750000.00", "400000.00", "50000.00", "250000.00", "233333.33", "786111.11")
  names(exp) <- c('New Labs',
                  'New Product Line',
                  'Research',
                  'New Software',
                  'New Accounting Standards',
                  'Accounting',
                  'Outsource',
                  'Switch to R',
                  'Go agile',
                  'IT',
                  'Acme Inc.')
  
  expect_equal(get, exp)
  

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
  get <- acme$Get('averageCost', traversal = "post-order")
  
  exp <- c(750000.00, 2000000.00, 1375000.00, 1000000.00, 500000.00, 750000.00, 400000.00, 50000.00, 250000.00, 233333.33, 786111.11)
  names(exp) <- c('New Labs',
                  'New Product Line',
                  'Research',
                  'New Software',
                  'New Accounting Standards',
                  'Accounting',
                  'Outsource',
                  'Switch to R',
                  'Go agile',
                  'IT',
                  'Acme Inc.')
  
  expect_equal(get, exp)
  
  
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

