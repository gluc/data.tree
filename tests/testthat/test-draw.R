context("plot")


test_that("grViz", {
  data(acme)  
  
  SetGraphStyle(acme, rankdir = "TB")
  SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
  #per default, Node style attributes will be inherited:
  SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
               fontname = "helvetica", tooltip = GetDefaultTooltip)
  SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
  #inheritance can be avoided:
  SetNodeStyle(acme$Accounting, inherit = FALSE, fillcolor = "Thistle", 
               fontcolor = "Firebrick", tooltip = "This is the accounting department")
  #use Do to set style on specific nodes:
  Do(acme$leaves, function(node) SetNodeStyle(node, shape = "egg"))
  gv <- ToGraphViz(acme, direction = "descend", pruneFun = function(x) x$level < 3)
  expect_equal(substr(gv, 1, 9), "digraph {")
})



test_that("grViz single attribute", {
  data(acme)
  SetNodeStyle(acme$Accounting, label = "Mimi")
  gv <- ToGraphViz(acme)
  
  exp <- "digraph {

graph []


  'Acme Inc.'
  'Accounting' [label = 'Mimi'] 
  'New Software'
  'New Accounting Standards'
  'Research'
  'New Product Line'
  'New Labs'
  'IT'
  'Outsource'
  'Go agile'
  'Switch to R'
  'Acme Inc.'->'Accounting' 
  'Acme Inc.'->'Research' 
  'Acme Inc.'->'IT' 
  'Accounting'->'New Software' 
  'Accounting'->'New Accounting Standards' 
  'Research'->'New Product Line' 
  'Research'->'New Labs' 
  'IT'->'Outsource' 
  'IT'->'Go agile' 
  'IT'->'Switch to R' 
}"
  
  expect_equal(gv, exp)
  
})


test_that("grViz single attribute names not uniuqe", {
  mytree <- CreateRegularTree(3, 3)
  mytree$Do(function(x) x$name <- x$position)
  SetNodeStyle(mytree, label = "Root")
  SetNodeStyle(mytree$`1`, tooltip = "L1")
  gv <- ToGraphViz(mytree)
  
  exp <- "digraph {

graph []


  '1' [label = 'Root'] 
  '1/1' [label = '1', tooltip = 'L1'] 
  '1/1/1' [label = '1'] 
  '1/1/2' [label = '2'] 
  '1/1/3' [label = '3'] 
  '1/2' [label = '2'] 
  '1/2/1' [label = '1'] 
  '1/2/2' [label = '2'] 
  '1/2/3' [label = '3'] 
  '1/3' [label = '3'] 
  '1/3/1' [label = '1'] 
  '1/3/2' [label = '2'] 
  '1/3/3' [label = '3'] 
  '1'->'1/1' 
  '1'->'1/2' 
  '1'->'1/3' 
  '1/1'->'1/1/1' 
  '1/1'->'1/1/2' 
  '1/1'->'1/1/3' 
  '1/2'->'1/2/1' 
  '1/2'->'1/2/2' 
  '1/2'->'1/2/3' 
  '1/3'->'1/3/1' 
  '1/3'->'1/3/2' 
  '1/3'->'1/3/3' 
}"
  
  expect_equal(gv, exp)
  
})

