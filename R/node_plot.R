
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
#' SetEdgeStyle(acme$Research$`New Labs`, color = "red", label = "Focus!", penwidth = 3, fontcolor = "red")
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
  #need to add label if not all names are unique
  if (!anu && !"label" %in% ns ) ns <- c(ns, "label")
  for (style in ns) {
    myargs[[style]] <- Get(tr, function(x) {
      myns <- GetStyle(x, style, "node")
      if (style == "label" && !anu && length(myns) == 0) myns <- x$name
      if (is.null(myns)) myns <- ""
      myns
    })
  }
  #names(myargs) <- c("nodes", ns)
  
  nodes <- do.call(create_nodes, myargs)
  if (!anu && !"label" %in% ns) nodes$label <- Get(tr, "name")
  #nodes <- nodes[!names(nodes)=="tooltip"]
  
  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "edgeStyle"), simplify = FALSE), names)))
  
  
  myargs <- list()
  #see http://stackoverflow.com/questions/19749923/function-factory-in-r
  for (style in ns) {
    myargs[[style]] <- GetEdgeStyleFactory(style)
  }

  #nodes <- do.call(create_nodes, myargs)

  edges <- do.call("ToDataFrameNetwork", c(root, direction = direction, pruneFun = pruneFun, myargs)) 
  
  graphStyle <- attr(root, "graphStyle")
  if (!is.null(graphStyle)) graphAttributes <- paste(names(graphStyle), paste0("'", graphStyle, "'"), sep = " = ", collapse = ", ")
  else graphAttributes <- ""
  nodeAttributes <- GetDefaultStyles(root, type = "node")
  edgeAttributes <- GetDefaultStyles(root, type = "edge")
  graph <- create_graph(nodes, edges, graph_attrs = graphAttributes, node_attrs = nodeAttributes, edge_attrs = edgeAttributes)
  
  #return (graph)
  #render_graph(graph)
  #cat(graph$dot_code)
  return (graph$dot_code)
  
}



GetEdgeStyleFactory <- function(style) {
  style <- force(style)
  function(node = node, origNode = node) {
    myns <- GetStyle(node, style, "edge")
    if (is.null(myns)) myns <- ""
    myns
  }
}


#' @param inherit If TRUE, then children will inherit this node's style. Otherwise they inherit from this 
#' node's parent.
#' 
#' @rdname ToGraphViz
#' 
#' @export
SetNodeStyle <- function(node, 
                         inherit = TRUE,
                         ...) {
  ll <- list(...)
  attr(node, "nodeStyle") <- ll
  attr(node, "nodeStyleInherit") <- inherit
}


#' @rdname ToGraphViz
#' @export
SetEdgeStyle <- function(node,
                         inherit = TRUE,
                         ...) {
  ll <- list(...)
  attr(node, "edgeStyle") <- ll
  attr(node, "edgeStyleInherit") <- inherit
}


#' @rdname ToGraphViz 
#' @export
SetGraphStyle <- function(root,
                          ...) {
  ll <- list(...)
  attr(root, "graphStyle") <- ll
}


GetStyle <- function(node, styleName, type = c("node", "edge"), origNode = node) {
  type <- type[1]
  inh <- attr(node, paste0(type, "StyleInherit"))
  res <- attr(node, paste0(type, "Style"))[[styleName]]
  if (!is.null(res)) {
    if (!node$isRoot) {
      if (identical(node, origNode) || (inh && !styleName %in% c("label", "tooltip"))) {# either on myself or inheritable
        if (is.function(res)) res <- res(origNode)
        return (res)
      }
    } else {
      #root
      if (identical(node, origNode) && styleName %in% c("label", "tooltip")) {
        if (is.function(res)) res <- res(origNode)
        return (res)
      } else {
        #inherited are only label and tt, and only if function
        if (styleName %in% c("label", "tooltip") && is.function(res)) {
          return (res(origNode))
        }
      }
      
    }
  }
  #recursion exit criteria
  # inherit from root only if label or tt are function
  if (node$level == 2 && styleName %in% c("label", "tooltip")) {
    res <- GetStyle(node$parent, styleName, type, origNode = origNode)
    if (is.function(res)) res <- res(origNode)
    return (res)
  }
  if (node$level <= 2) return (NULL) 
  #recursion
  GetStyle(node$parent, styleName, type, origNode = origNode)
  
}

GetDefaultStyles <- function(node, type = c("node", "edge")) {
  type <- type[1]
  node <- node$root
  inh <- attr(node, paste0(type, "StyleInherit"))
  res <- attr(node, paste0(type, "Style"))
  if (!is.null(res) && inh) {
    res <- res[!names(res) %in% c("label", "tooltip")]
    if (length(res) == 0) return (NULL)
    res <- paste(names(res), paste0("'", res, "'"), sep = " = ", collapse = ", ")
    return (res) 
  } else return (NULL)
}

