
#'@rdname ToGraphViz
#'@import DiagrammeR
#'
#'@param x The root node of the data.tree structure to plot
#'@param engine string for the Graphviz layout engine; can be dot (default), 
#'neato, circo, or twopi. For more information see https://github.com/mdaines/viz.js#usage.
#'@inheritParams ToDataFrameNetwork
#'
#'@export
plot.Node <- function(x, direction = c("climb", "descend"), pruneFun = NULL, engine = "dot") {
  dotLng <- ToGraphViz(x, direction, pruneFun)
  grViz(dotLng, engine)
}


#' Get a graphviz dot representation of the tree
#' 
#' @param root The root \code{\link{Node}} of the data.tree structure to visualize.
#' @param node The \code{\link{Node}} of the data.tree structure on which you would like to set style attributes.
#' @param ... Any stlye / value pair. See http://graphviz.org/Documentation.php for details.
#' 
#' @inheritParams Prune
#' 
#' @examples
#' data(acme)
#' SetGraphStyle(acme, rankdir = "TB")
#' SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
#' #per default, Node style attributes will be inherited:
#' SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
#'              fontname = "helvetica", tooltip = GetDefaultTooltip)
#' SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
#' #inheritance can be avoided:
#' SetNodeStyle(acme$Accounting, inherit = FALSE, fillcolor = "Thistle", 
#'              fontcolor = "Firebrick", tooltip = "This is the accounting department")
#' #use Do to set style on specific nodes:
#' Do(acme$leaves, function(node) SetNodeStyle(node, shape = "egg"))
#' plot(acme)
#' 
#' @export
ToGraphViz <- function(root, direction = c("climb", "descend"), pruneFun = NULL) {
  # get all node styles

  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "nodeStyle"), simplify = FALSE), names)))
    
  tr <- Traverse(root, pruneFun = pruneFun)
  anu <- AreNamesUnique(root)
  myargs <- list(nodes = Get(tr, ifelse(anu, "name", "pathString")))
  if (!anu && !"label" %in% ns ) ns <- c(ns, "label")
  for (style in ns) {
    myargs[[style]] <- Get(tr, function(x) {
      myns <- GetNodeStyle(x, style)
      if (style == "label" && !anu && length(myns == 0)) myns <- x$name
      myns
    })
  }
  #names(myargs) <- c("nodes", ns)
  
  nodes <- do.call(create_nodes, myargs)
  if (!anu && !"label" %in% ns) nodes$label <- Get(tr, "name")
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
  
  graph <- create_graph(nodes, edges, graph_attrs = graphAttributes)
  
  #return (graph)
  #render_graph(graph)
  return (graph$dot_code)
  
}