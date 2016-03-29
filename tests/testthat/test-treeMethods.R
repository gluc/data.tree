context("tree methods")



test_that("Climb NULL", {
  data(acme)
  expect_equal(acme$Climb('X'), NULL)
  expect_equal(acme$Climb('X', 'Y', 'Z'), NULL)
  expect_equal(acme$Climb('IT', 'X'), NULL)
 
})


test_that("Climb Equivalent", {
  data(acme)
  expect_equal(acme$Climb('IT', 'Go agile'), acme$Climb('IT')$Climb('Go agile'))
  
})



test_that("Climb 3rd Level", {
  data(acme)
  acme$Climb('IT', 'Go agile')$AddChild('MyTest')$AddChild('MyTest2')
  expect_equal("MyTest2", acme$Climb('IT', 'Go agile', 'MyTest', 'MyTest2')$name )
  expect_equal("MyTest2", acme$Climb(c('IT', 'Go agile', 'MyTest', 'MyTest2'))$name )
  expect_equal("MyTest2", acme$Climb(name = c('IT', 'Go agile', 'MyTest', 'MyTest2'))$name )
  
})



test_that("Climb non-name", {
  tree <- CreateRegularTree(5, 2)
  p <- tree$Climb(c("1.1", "1.1.1"), position = c(2, 2))$path
  expect_equal(c("1", "1.1", "1.1.1", "1.1.1.2", "1.1.1.2.2"), p)
  
})




test_that("Find", {
  
  data(acme)
  os <- acme$FindNode("Outsource")
  expect_equal(os$name, "Outsource")
  
  os <- acme$FindNode("XYZ")
  expect_null(os)
  
  acme$Accounting$AddChild("Outsource")
  os <- acme$FindNode("Outsource")
  expect_equal(class(os), c("Node", "R6"))
  expect_equal(os$name, "Outsource")

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
  get <- acme$Climb('Research', 'New Labs')$Get("name", traversal = "ancestor")
  
  exp <- c('New Labs', 
           'Research', 
           'Acme Inc.')
  
  names(exp) <- exp
  
  expect_equal(get, exp)
  
})


test_that("GetAttribute matrix", {
  data(acme)
  acme$IT$matrix <- diag(2)
  
  res <- GetAttribute(acme$IT, "matrix")
  
  expect_equal(acme$IT$matrix, res)
  
})


test_that("Get format", {
  
  data(acme)
  
  calculateAggregateChildCost <- function(node, fun) {
    if (node$isLeaf) node$averageCost <- node$cost
    else node$averageCost <- fun(sapply(node$children, function(x) x$averageCost))
  }
  
  myFormat <- function(x) {
    format(x, nsmall=2, scientific = FALSE)
  }
  
  acme$Do(calculateAggregateChildCost, mean, traversal = "post-order")
  get <- acme$Get("averageCost", format = myFormat)["New Product Line"]
  
  expect_equal(as.character(get), "2000000.00")
  

})


test_that("Traverse pre-order", {
  data(acme)
  tr <- Traverse(acme, traversal = "pre-order")
  nms <- sapply(tr, function(x) x$name)
  exp <- c("Acme Inc.",
           "Accounting",
           "New Software",
           "New Accounting Standards",
           "Research",
           "New Product Line",
           "New Labs",
           "IT",
           "Outsource",
           "Go agile",
           "Switch to R")
  expect_equal(nms, exp)
  
})


test_that("Traverse post-order", {
  data(acme)
  tr <- Traverse(acme, traversal = "post-order")
  nms <- sapply(tr, function(x) x$name)
  exp <- c("New Software",             "New Accounting Standards", "Accounting",               "New Product Line",        
           "New Labs",                 "Research",                 "Outsource",                "Go agile",                
           "Switch to R",              "IT",                       "Acme Inc."        )
  expect_equal(nms, exp)
  
})


test_that("Traverse in-order", {
  data(acme)
  tr <- Traverse(acme, traversal = "level")
  nms <- sapply(tr, function(x) x$name)
  exp <- c("Acme Inc.",                "Accounting",               "Research",                 "IT",                      
           "New Software",             "New Accounting Standards", "New Product Line",         "New Labs",                
           "Outsource",                "Go agile",                 "Switch to R"        )
  expect_equal(nms, exp)
  
})


test_that("Traverse empty filter", {
  data(acme)
  tr <- Traverse(acme, filterFun = function(x) x$name == "Marketing")
  nms <- sapply(tr, function(x) x$name)
  exp <- vector(mode = "list")
  expect_equal(nms, exp)
  
})


test_that("Traverse empty filter level", {
  data(acme)
  tr <- Traverse(acme, traversal = "level", filterFun = function(x) x$name == "Marketing")
  nms <- sapply(tr, function(x) x$name)
  exp <- vector(mode = "list")
  expect_equal(nms, exp)
  
})


test_that("Do", {
  
  data(acme)
  
  calculateAggregateChildCost <- function(node, fun) {
    if (node$isLeaf) return(node$cost)
    fun(sapply(node$children, function(x) x$averageCost))
  }
  
  myFormat <- function(x) {
    format(x, nsmall=2, scientific = FALSE)
  }
  
  acme$Do(function(x) x$averageCost <- calculateAggregateChildCost(x, mean), traversal = "post-order")
  get <- acme$Get('averageCost', traversal = "post-order")["New Product Line"]
  
  
  expect_equal(as.numeric(get), 2000000)
  
  
})



test_that("post-order", {
  
  data(acme)
  
  acme$Set(myval = 1:acme$totalCount, traversal = "post-order")
  
  expect_equal(acme$myval, 11)
  expect_equal(acme$Climb("Research")$myval, 6)
  
})


test_that("level", {
  
  data(acme)
  
  acme$Set(myval = 1:acme$totalCount, traversal = "level")
  
  expect_equal(acme$myval, 1)
  expect_equal(acme$Climb("Research")$myval, 3)
  expect_equal(acme$Climb("IT", "Go agile")$myval, 10)
  
})



test_that("level subtree", {
  
  data(acme)
  it <- acme$Climb("IT")
  
  it$Set(myval = 1:it$totalCount, traversal = "level")
  
  expect_equal(it$myval, 1)
  expect_equal(it$Climb("Outsource")$myval, 2)
  expect_equal(it$Climb("Go agile")$myval, 3)
  expect_equal(it$Climb("Switch to R")$myval, 4)
  
})


test_that("prune", {
  
  data(acme)
  
  acme$Set(myval = 1:8, pruneFun = function(x) x$name != "Research")
  
  expect_equal(acme$myval, 1)
  expect_true(is.null(acme$Climb("Research")$myval))
  expect_true(is.null(acme$Climb("Research", "New Labs")$myval))
  expect_equal(acme$Climb("IT", "Go agile")$myval, 7)
  
})


test_that("filter", {
  
  data(acme)
  
  acme$Set(myval = 1:10, filterFun = function(x) x$name != "Research")
  
  expect_equal(acme$myval, 1)
  expect_true(is.null(acme$Climb("Research")$myval))
  expect_equal(acme$Climb("Research", "New Labs")$myval, 6)
  expect_equal(acme$Climb("IT", "Go agile")$myval, 9)
  
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
  addBinChildren(node$Climb("0.1", "0.1.2", "0.1.2.1"), 0)
  
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
    acme$Climb("Accounting", "New Accounting Standards")$AddChild("ICI 320")
    acme$Set(myval = 1:6)
    expect_equal(acme$myval, 1)
    expect_equal(acme$Climb("Research", "New Labs")$myval, 2)
    expect_equal(acme$Climb("IT", "Go agile")$myval, 5)
       
})





test_that("Aggregate", {
  data(acme)
  expect_equal(Aggregate(acme, "cost", sum), 4950000)
  
})




test_that("Clone", {
  data(acme)
  n <- Clone(acme)
  
  expect_equal(class(n), class(acme))
  expect_equal(n$name, acme$name)
  expect_equal(n$count, acme$count)
  expect_equal(n$totalCount, acme$totalCount)
  expect_equal(n$Climb("IT", "Go agile")$p, acme$Climb("IT", "Go agile")$p)
  
  expect_equal(as.list(n), as.list(acme))
  acme2 <- acme
  expect_identical(acme, acme2)
  
  #expect_false(n, is_identical_to(acme))
  n$name <- 'Acme2'
  expect_false(n$name == acme$name)
  
})

test_that("Clone formatter", {
  data(acme)
  SetFormat(acme, "count", FormatFixedDecimal)
  SetFormat(acme$Climb("IT", "Outsource"), "p", FormatPercent)
  
  n <- Clone(acme, attributes = TRUE)
  
  fo <- attr(n, "formatters")[["count"]]
  expect_equal(fo, FormatFixedDecimal)

  fo2 <- attr(n$Climb("IT", "Outsource"), "formatters")[["p"]]
  expect_equal(fo2, FormatPercent)
  
  
})


test_that("Clone subtree", {
  data(acme)
  it <- acme$Climb("IT")
  itcl <- Clone(it)
  
  expect_equal(class(itcl), class(it))
  expect_equal(itcl$name, it$name)
  expect_equal(itcl$count, it$count)
  expect_equal(itcl$totalCount, it$totalCount)
  expect_equal(itcl$Climb("Go agile")$p, it$Climb("Go agile")$p)
  expect_true(itcl$isRoot)
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
  
  expect_equal(g[[1]], sum(acme$Get(function(x) x$cost * x$p, filterFun = isLeaf)))

})





test_that("Formatter Get", {
  data(acme)
  SetFormat(acme, "p", FormatPercent)
  p <- acme$Get("p")
  expect_equal(p[["Go agile"]], "5.00 %")
})

test_that("Formatter Get Hierarchy", {
  data(acme)
  SetFormat(acme, "p", FormatPercent)
  acme$p <- 1
  n <- acme$Climb("IT")
  SetFormat(n, "p", FormatFixedDecimal)
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


test_that("Set matrix", {
  
  data(acme)
  acme$Set(id = 1:acme$totalCount)
  ms <- sapply(1:acme$totalCount, function(x) diag(x))
  acme$Set(matrix = ms)
  msget <- acme$Get("matrix")
  expect_equal(unname(acme$Get("name")), names(msget))
  expect_equal(ms, unname(msget))
})


test_that("Set pre-order", {
  data(acme)
  acme$Set(mycnt = 1:acme$totalCount)
  expect_equal( acme$Climb("IT")$mycnt, 8)
})


test_that("Set post-order", {
  data(acme)
  acme$Set(mycnt = 1:acme$totalCount, traversal = "post-order")
  expect_equal( acme$Climb("IT")$mycnt, 10)
  expect_equal( acme$mycnt, 11)
})


test_that("Set filter", {
  data(acme)
  acme$Set(mycnt = 1:3, filterFun = function(x) x$level == 2)
  expect_equal( acme$Climb("IT")$mycnt, 3)
  expect_equal( acme$mycnt, NULL)
})



test_that("Revert", {
  data(acme)
  acme$Set(id = 1:acme$totalCount)
  Revert(acme)
  ids <- unname(acme$Get("id"))
  expected = c(1, 8, 11, 10, 9, 5, 7, 6, 2, 4, 3)
  expect_equal(ids, expected)
})


test_that("fieldsAll", {
  data(acme)
  fa <- acme$fieldsAll
  expect_equal(fa, c("cost", "p"))
  acme$Set(tta = 1:acme$totalCount)
  expect_equal(acme$fieldsAll, c("tta", "cost", "p"))
})


test_that("height", {
  data(acme)
  expect_equal(acme$height, 3)
  expect_equal(acme$Climb("IT")$height, 2)
  acme$Climb("IT", "Outsource")$AddChild("New")
  
  expect_equal(acme$height, 4)
})


test_that("isRoot", {
  data(acme)
  expect_true(acme$isRoot)
  expect_false(acme$Climb("IT")$isRoot)
  expect_equal(acme$Climb("IT")$height, 2)
  isRoot <- acme$Get("isRoot")
  expect_equal(sum(isRoot), 1)
  
})


test_that("isLeaf", {
  data(acme)
  expect_false(acme$isLeaf)
  expect_true(acme$Climb("Research", "New Labs")$isLeaf)
  
  isLeaf <- acme$Get("isLeaf")
  leaves <- names(isLeaf)[isLeaf]
  exp <- c("New Software", "New Accounting Standards", "New Product Line", "New Labs", 
           "Outsource", "Go agile", "Switch to R")
  expect_equal(leaves, exp)
  
})



test_that("level (active)", {
  data(acme)
  expect_equal(acme$level, 1)  
  expect_equal(acme$Climb("Research")$level, 2)
  expect_equal(acme$Climb("Research", "New Labs")$level, 3)
  
})


test_that("set name Climb", {
  data(acme)
  rs <- acme$Climb("Research")
  rs$name <- "Research2"
  
  rs2 <- acme$Climb("Research")
  expect_true(is.null(rs2))
  rs2 <- acme$Climb("Research2")
  expect_true(rs2$name == "Research2")
  expect_equal(names(rs$parent$children), c("Accounting", "Research2", "IT"))
})


test_that("change name", {
  data(acme)
#  acme$Research$name <- "Research2"
  
#  expect_true(is.null(acme$Research))

  rs <- acme$Research
  rs$name <- "Research2"
  expect_true(is.null(acme$Research))
  expect_true(acme$Research2$name == "Research2")
})


test_that("attribute function with formatter", {
  data(acme)
  SetFormat(acme, "cost", FormatFixedDecimal)
  acme$IT$cost <- function(self) sum(sapply(self$children, function(x) x$cost))
  mycost <- acme$Get("cost")
  expect_equal(mycost[[8]], "700000.000")
  
})


test_that("Remove Child", {
  data(acme)
  sw <- acme$Accounting$RemoveChild("New Software")
  expect_equal(sw$name, "New Software")
  expect_true(sw$isRoot)
  expect_equal(acme$Accounting$count, 1)
  expect_equal(names(acme$Accounting$children), c("New Accounting Standards"))
})

test_that("Remove Attribute", {
  data(acme)
  acme$Research$floor <- 21
  expect_true("floor" %in% acme$Research$fields)
  acme$Research$RemoveAttribute("floor")
  expect_false("floor" %in% acme$Research$fields)
})


test_that("Remove Attribute stop", {
  data(acme)
  acme$Research$floor <- 21
  expect_true("floor" %in% acme$Research$fields)
  expect_true(acme$Research$RemoveAttribute("floor", FALSE))
  expect_false("floor" %in% acme$Research$fields)
  expect_false(acme$IT$RemoveAttribute("floor", FALSE))
  
})

test_that("Add Sibling", {
  data(acme)
  acme$Research$AddSibling("Marketing")$AddChild("Web")$AddSibling("Print")
  expect_equal(acme$Marketing$position, 3)
  expect_equal(acme$IT$position, 4)
  expect_equal(acme$Marketing$Web$siblings[[1]]$name, "Print")
})

test_that("print", {
  data(acme)
  acme2 <- print(acme, "cost")
  expect_equal(colnames(acme2), c("levelName", "cost"))
})




test_that("Cumulate", {
  data(acme)
  acme$Do(function(x) x$cost <- Aggregate(x, "cost", sum), traversal = "post-order")
  acme$Do(function(x) x$cumCost <- Cumulate(x, "cost", sum))
  expect_equal(unname(acme$Get("cumCost")),  c(4950000, 1500000, 1000000, 1500000, 4250000, 2000000, 2750000, 4950000, 400000, 650000, 700000))
})

test_that("averageBranchingFactor", {
  t <- CreateRegularTree(3, 3)
  expect_equal(t$averageBranchingFactor, 3)
})


test_that("siblings", {
  data(acme)
  s <- acme$IT$siblings
  expect_equal(2, length(s))
  nms <- unname(Get(s, "name"))
  expect_equal(c("Accounting", "Research"), nms)
  
})


test_that("leaves", {
  data(acme)
  l <- acme$leaves
  expect_equal(7, length(l))
  expect_equal(unname(sapply(l, function(x) x$name)), c("New Software",
                                                        "New Accounting Standards",
                                                        "New Product Line",
                                                        "New Labs",         
                                                        "Outsource",
                                                        "Go agile",
                                                        "Switch to R"))            

  l <- acme$IT$Outsource$leaves
  expect_equal(typeof(l), "list")
  expect_equal(length(l), 1)
  
})