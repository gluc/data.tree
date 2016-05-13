context("tree conversion party")


test_that("party on", {
  
  airq <- subset(airquality, !is.na(Ozone))
  airct <- party::ctree(Ozone ~ ., data = airq, 
                        controls = party::ctree_control(maxsurrogate = 3))
  
  tree <- CreateNodeFromParty(airct@tree)
  
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
