context("plot")


test_that("plot only works if DiagrammeR is installed", {

  # Given
  # - an object of class "Node"--"R6"
  # - in an R session where DiagrammeR is not installed
  # When
  # - the user tries to construct a DiagrammeR-based graph or plot
  # Then
  # - an error is thrown

  data(acme)
  mockery::stub(ToDiagrammeRGraph, "requireNamespace", FALSE, 1)
  mockery::stub(plot.Node, "requireNamespace", FALSE, 1)

  expect_error(
    ToDiagrammeRGraph(acme),
    info = "ToDiagrammeRGraph should fail if DiagrammeR is not installed")

  expect_error(
    plot(acme),
    info = "plot() should fail if DiagrammeR is not installed")
})


test_that("grViz", {
  testthat::skip_if_not_installed("DiagrammeR", minimum_version = "1.0.0")

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
  gv <- DiagrammeR::generate_dot(graph)
  expect_equal(substr(gv, 1, 9), "digraph {")
})



test_that("grViz single attribute", {
  testthat::skip_if_not_installed("DiagrammeR", minimum_version = "1.0.0")

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
  '1'->'2' 
  '1'->'5' 
  '1'->'8' 
  '2'->'3' 
  '2'->'4' 
  '5'->'6' 
  '5'->'7' 
  '8'->'9' 
  '8'->'10' 
  '8'->'11' 
}"
  
  expect_equal(gv, exp)
  
})


test_that("grViz single attribute names not uniuqe", {
  testthat::skip_if_not_installed("DiagrammeR", minimum_version = "1.0.0")

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
  '1'->'2' 
  '1'->'6' 
  '1'->'10' 
  '2'->'3' 
  '2'->'4' 
  '2'->'5' 
  '6'->'7' 
  '6'->'8' 
  '6'->'9' 
  '10'->'11' 
  '10'->'12' 
  '10'->'13' 
}"
  
  expect_equal(gv, exp)
  
})

test_that("grViz names with quotes", {
  mytree <- Node$new("my_root")
  mytree$AddChild("A")$AddChild("\"B\"")$AddChild("\"C\"")$AddChild("D")
  exp_lab <- c("my_root", "A", "\\\"B\\\"", "\\\"C\\\"", "D")
  expect_equal(ToDiagrammeRGraph(mytree)$nodes_df$label,
               exp_lab)
})

