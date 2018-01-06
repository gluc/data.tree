context("tree conversion party")


test_that("party on", {
  
  airq <- subset(airquality, !is.na(Ozone))
  airct <- party::ctree(Ozone ~ ., data = airq, 
                        controls = party::ctree_control(maxsurrogate = 3))
  
  tree <- as.Node(airct)
  
  res <- as.numeric(unname(tree$Get("name")))
  
  expect_equal(res, 1:9)
  
  res <- tree$Get("label")
  
  exp <- c(`1` = "Temp <= 82",
          `2` = "Wind <= 6.9",
          `3` = "weights = 10",
          `4` = "Temp > 77",
          `5` = "weights = 48",
          `6` = "weights = 21",
          `7` = "Wind > 10.3",
          `8` = "weights = 30",
          `9` = "weights = 7" )
  
  expect_equal(res, exp)

})



test_that("partykid", {
  #hack but needed, otherwise extree_data cannot be found
  library(partykit)
  airq <- subset(airquality, !is.na(Ozone))
  airct <- partykit::ctree(Ozone ~ ., data = airq)
  
  tree <- as.Node(airct)
  
  res <- as.numeric(unname(tree$Get("name")))
  
  expect_equal(res, 1:9)
  
  res <- tree$Get("splitname")
  
  exp <- c(`1` = "Temp",
           `2` = "Wind",
           `3` = NA,
           `4` = "Temp",
           `5` = NA,
           `6` = NA,
           `7` = "Wind",
           `8` = NA,
           `9` = NA )
  
  expect_equal(res, exp)
  
  res <- tree$Get("splitLevel")
  
  exp <- c(`1` = NA,
           `2` = "<= 82",
           `3` = "<= 6.9",
           `4` = "> 6.9",
           `5` = "<= 77",
           `6` = "> 77",
           `7` = "> 82",
           `8` = "<= 10.3",
           `9` = "> 10.3")
  
  expect_equal(res, exp)
  
})
