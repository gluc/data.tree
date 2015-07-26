context("tree methods side effects")



test_that("Sort", {
  data(acme)
  acme$Do(function(x) x$totalCost <- Aggregate(x, "cost", sum))
  Sort(acme, "totalCost", decreasing = FALSE)
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
  
  Sort(acme, "totalCost", decreasing = TRUE)
  get <- acme$Get('totalCost')
  
  expect_false(identical(all.equal(get, exp), TRUE))
  
  
})



test_that("Prune leaves", {
  data(acme)
  
  acme$Prune(function(x) is.null(x$cost) || x$cost < 1000000)
  expect_equal(acme$leafCount, 5)
  expect_equal(acme$totalCount, 9)
  expect_true(all(acme$Get("cost", filterFun = isLeaf) < 1000000))
  
})


test_that("Prune name", {
  data(acme)
  
  acme$Prune(function(x) x$name != "IT")
  expect_equal(acme$leafCount, 4)
  expect_equal(acme$totalCount, 7)
  
  expect_true(is.null(acme$Climb("IT")))
  
})

