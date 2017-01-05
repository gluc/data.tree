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
  graph <- ToDiagrammeRGraph(acme, direction = "descend", pruneFun = function(x) x$level < 3)
  gv <- generate_dot(graph)
  expect_equal(substr(gv, 1, 9), "digraph {")
})



test_that("grViz single attribute", {
  data(acme)
  SetNodeStyle(acme$Accounting, label = "Mimi")
  
  graph <- ToDiagrammeRGraph(acme)
  gv <- DiagrammeR::generate_dot(graph)
  
  exp <- "digraph {

  '1' [label = 'Acme Inc.'] 
  '2' [label = 'Mimi'] 
  '3' [label = 'New Software'] 
  '4' [label = 'New Accounting Standards'] 
  '5' [label = 'Research'] 
  '6' [label = 'New Product Line'] 
  '7' [label = 'New Labs'] 
  '8' [label = 'IT'] 
  '9' [label = 'Outsource'] 
  '10' [label = 'Go agile'] 
  '11' [label = 'Switch to R'] 
'1'->'2' [id = '1'] 
'1'->'5' [id = '2'] 
'1'->'8' [id = '3'] 
'2'->'3' [id = '4'] 
'2'->'4' [id = '5'] 
'5'->'6' [id = '6'] 
'5'->'7' [id = '7'] 
'8'->'9' [id = '8'] 
'8'->'10' [id = '9'] 
'8'->'11' [id = '10'] 
}"
  
  expect_equal(gv, exp)
  
})


test_that("grViz single attribute names not uniuqe", {
  mytree <- CreateRegularTree(3, 3)
  mytree$Do(function(x) x$name <- x$position)
  SetNodeStyle(mytree, label = "Root")
  SetNodeStyle(mytree$`1`, tooltip = "L1")
  graph <- ToDiagrammeRGraph(mytree)
  gv <- DiagrammeR::generate_dot(graph)
  
  exp <- "digraph {

  '1' [label = 'Root', tooltip = ''] 
  '2' [label = 'Root', tooltip = 'L1'] 
  '3' [label = 'Root', tooltip = ''] 
  '4' [label = 'Root', tooltip = ''] 
  '5' [label = 'Root', tooltip = ''] 
  '6' [label = 'Root', tooltip = ''] 
  '7' [label = 'Root', tooltip = ''] 
  '8' [label = 'Root', tooltip = ''] 
  '9' [label = 'Root', tooltip = ''] 
  '10' [label = 'Root', tooltip = ''] 
  '11' [label = 'Root', tooltip = ''] 
  '12' [label = 'Root', tooltip = ''] 
  '13' [label = 'Root', tooltip = ''] 
'1'->'2' [id = '1'] 
'1'->'6' [id = '2'] 
'1'->'10' [id = '3'] 
'2'->'3' [id = '4'] 
'2'->'4' [id = '5'] 
'2'->'5' [id = '6'] 
'6'->'7' [id = '7'] 
'6'->'8' [id = '8'] 
'6'->'9' [id = '9'] 
'10'->'11' [id = '10'] 
'10'->'12' [id = '11'] 
'10'->'13' [id = '12'] 
}"
  
  expect_equal(gv, exp)
  
})

