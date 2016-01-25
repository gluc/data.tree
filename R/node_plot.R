
#'@rdname ToGraphViz
#'@import DiagrammeR
#'@export
Draw <- function(root, direction = c("climb", "descend"), pruneFun = NULL, engine = "dot") {
  dotLng <- ToGraphViz(root, direction, pruneFun)
  grViz(dotLng, engine)
}


#' Get a graphviz dot representation of the tree
#' 
#' @param root The root \code{\link{Node}} of the data.tree structure to visualize.
#' @param node The \code{\link{Node}} of the data.tree structure on which you would like to set style attributes.
#' 
#' @inheritParams Prune
#' 
#' @examples
#' data(acme)
#' SetGraphStyle(acme, rankdir = "TB")
#' SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
#' #per default, Node style attributes will be inherited:
#' SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", fontname = "helvetica", tooltip = GetDefaultTooltip)
#' SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
#' #inheritance can be avoided:
#' SetNodeStyle(acme$Accounting, inherit = FALSE, fillcolor = "Thistle", fontcolor = "Firebrick", tooltip = "This is the accounting department")
#' #use Do to set style on specific nodes:
#' Do(acme$leaves, function(node) SetNodeStyle(node, shape = "egg"))
#' Draw(acme)
#' 
#' @export
ToGraphViz <- function(root, direction = c("climb", "descend"), pruneFun = NULL) {
  # get all node styles

  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "nodeStyle")), names)))
    
  GetNodeStyle <- function(node, styleName, origNode = node) {
    inh <- attr(node, "nodeStyleInherit")
    res <- attr(node, "nodeStyle")[[styleName]]
    if (!is.null(res) && (identical(node, origNode) || inh)) {
      if (is.function(res)) res <- res(origNode)
      return (res)
    }
    if (node$isRoot) return ("")
    return (GetNodeStyle(node$parent, styleName, origNode = origNode))
  }
  
  tr <- Traverse(root, pruneFun = pruneFun)
  
  myargs <- list(nodes = Get(tr, "name"))
  
  for (style in ns) {
    myargs[[style]] <- Get(tr, function(x) GetNodeStyle(x, style))
  }
  #names(myargs) <- c("nodes", ns)

  nodes <- do.call(create_nodes, myargs)
  
  #nodes <- nodes[!names(nodes)=="tooltip"]
  
  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "edgeStyle")), names)))
  
  
  GetEdgeStyleFactory <- function(style) {
    styleName <- style
    function(node = node, origNode = node) {
      inh <- attr(node, "edgeStyleInherit")
      res <- attr(node, "edgeStyle")[[styleName]]
      if (!is.null(res) && (identical(node, origNode) || inh)) {
        if (is.function(res)) res <- res(origNode)
        return (res)
      }
      if (node$isRoot) return ("")
      return (Recall(node = node$parent, origNode = origNode))
    }
  }
  
  
  myargs <- list()
  #see http://stackoverflow.com/questions/19749923/function-factory-in-r
  for (style in ns) {
    myargs[[style]] <- GetEdgeStyleFactory(style)
  }

  #nodes <- do.call(create_nodes, myargs)
  edges <- do.call("ToDataFrameNetwork", c(root, direction = direction, pruneFun = pruneFun, myargs)) 
  
  graphStyle <- attr(root, "graphStyle")
  if (!is.null(graphStyle)) graphAttributes <- paste(names(graphStyle), graphStyle, sep = " = ", collapse = ", ")
  else graphAttributes <- ""
  
  graph <- create_graph(nodes, graph_attrs = graphAttributes)
  graph <- add_edges(graph, edges)
  #return (graph)
  #render_graph(graph)
  return (graph$dot_code)
  
}