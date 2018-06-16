
#'@rdname ToDiagrammeRGraph
#'@import DiagrammeR
#'
#'@param x The root node of the data.tree structure to plot
#'@inheritParams ToDataFrameNetwork
#'@inheritParams DiagrammeR::render_graph
#'
#'@export
plot.Node <- function(x, ..., direction = c("climb", "descend"), pruneFun = NULL, output = "graph") {
  graph <- ToDiagrammeRGraph(x, direction, pruneFun)
  render_graph(graph, output = output, ...)
}


#' Plot a graph, or get a graphviz dot representation of the tree
#' 
#' Use these methods to style your graph, and to plot it. The functionality is built around the
#' DiagrammeR package, so for anything that goes beyond simple plotting, it is recommended to read its 
#' documentation at http://rich-iannone.github.io/DiagrammeR/docs.html
#' 
#' Use \code{SetNodeStyle} and \code{SetEdgeStyle} to define the style of your plot. Use \code{plot} to display a 
#' graphical representation of your tree.
#' 
#' The most common styles that can be set on the nodes are:
#' \itemize{
#'   \item{\code{color}}
#'   \item{\code{fillcolor}}
#'   \item{\code{fixedsize} true or false}
#'   \item{\code{fontcolor}}
#'   \item{\code{fontname}}
#'   \item{\code{fontsize}}
#'   \item{\code{height}}
#'   \item{\code{penwidth}}
#'   \item{\code{shape} box, ellipse, polygon, circle, box, etc.}
#'   \item{\code{style}}
#'   \item{\code{tooltip}}
#'   \item{\code{width}}
#'  }
#' The most common styles that can be set on the edges are:
#' \itemize{
#'   \item{\code{arrowhead} e.g. normal, dot, vee}
#'   \item{\code{arrowsize}}
#'   \item{\code{arrowtail}}
#'   \item{\code{color}}
#'   \item{\code{dir} forward, back, both, none}
#'   \item{\code{fontcolor}}
#'   \item{\code{fontname}}
#'   \item{\code{fontsize}}
#'   \item{\code{headport}}
#'   \item{\code{label}}
#'   \item{\code{minlen}}
#'   \item{\code{penwidth}}
#'   \item{\code{tailport}}
#'   \item{\code{tooltip}}
#'  }
#' A good source to understand the attributes is http://graphviz.org/Documentation.php. Another good source
#' is the DiagrammeR package documentation, or more specifically: http://rich-iannone.github.io/DiagrammeR/docs.html
#'
#' In addition to the standard GraphViz functionality, the \code{data.tree} plotting infrastructure takes advantage
#' of the fact that data.tree structure are always hierarchic. Thus, style attributes are inherited from parents
#' to children on an individual basis. For example, you can set the fontcolor to red on a parent, and then all children
#' will also have red font, except if you specifically disallow inheritance. Labels and tooltips are never inherited.
#' 
#' Another feature concerns functions: Instead of setting a fixed value (e.g. \code{SetNodeStyle(acme, label = "Acme. Inc"}), 
#' you can set a function (e.g. \code{SetNodeStyle(acme, label = function(x) x$name)}). The function must take a \code{\link{Node}}
#' as its single argument. Together with inheritance, this becomes a very powerful tool.
#'   
#' The \code{GetDefaultTooltip} method is a utility method that can be used to print all fields of a \code{\link{Node}}.
#' 
#' There are some more examples in the 'applications' vignette, see \code{vignette('applications', package = "data.tree")}
#' 
#' @param root The root \code{\link{Node}} of the data.tree structure to visualize.
#' @param node The \code{\link{Node}} of the data.tree structure on which you would like to set style attributes.
#' @param ... For the SetStyle methods, this can be any stlyeName / value pair. See 
#' http://graphviz.org/Documentation.php for details. For the plot.Node generic method, this is not used.
#' 
#' @inheritParams Prune
#' 
#' @examples
#' data(acme)
#' SetGraphStyle(acme, rankdir = "TB")
#' SetEdgeStyle(acme, arrowhead = "vee", color = "blue", penwidth = 2)
#' #per default, Node style attributes will be inherited:
#' SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
#'              fontname = "helvetica", tooltip = GetDefaultTooltip)
#' SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
#' #inheritance can be avoided:
#' SetNodeStyle(acme$Accounting, inherit = FALSE, fillcolor = "Thistle", 
#'              fontcolor = "Firebrick", tooltip = "This is the accounting department")
#' SetEdgeStyle(acme$Research$`New Labs`, 
#'              color = "red", 
#'              label = "Focus!", 
#'              penwidth = 3, 
#'              fontcolor = "red")
#' #use Do to set style on specific nodes:
#' Do(acme$leaves, function(node) SetNodeStyle(node, shape = "egg"))
#' plot(acme)
#' 
#' #print p as label, where available:
#' SetNodeStyle(acme, label = function(node) node$p)
#' plot(acme)
#' 
#' @export
ToDiagrammeRGraph <- function(root, direction = c("climb", "descend"), pruneFun = NULL) {
  #get unique node styles defined on tree
  
  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "nodeStyle"), simplify = FALSE), names)))
  
  
  # set tmp .id
  tr <- Traverse(root, pruneFun = pruneFun)
  Set(tr, `.id` = 1:length(tr))
  
  #create nodes df
  myargs <- list()
  if(!"label" %in% ns) ns <- c(ns, "label")
  for (style in ns) {
    myargs[[style]] <- Get(tr, function(x) {
      myns <- GetStyle(x, style, "node")
      if (style == "label" && length(myns) == 0) myns <- x$name
      #if (is.null(myns)) myns <- ""
      myns
    })
  }
  
  nodes <- do.call(create_node_df, c(n = length(tr), myargs))
  
  # get unique edge styles
  
  es <- unique(unlist(sapply(root$Get(function(x) attr(x, "edgeStyle"), simplify = FALSE), names)))
  
  
  myargs <- list()
  #see http://stackoverflow.com/questions/19749923/function-factory-in-r
  for (style in es) {
    myargs[[style]] <- GetEdgeStyleFactory(style)
  }
  
  
  
  edges <- do.call("ToDataFrameNetwork", c(root, from = function(node) node$parent$`.id`, to = ".id", myargs, direction = list(direction), pruneFun = pruneFun))[,-(1:2)]
  if (nrow(edges) > 0) {
    edges <- do.call(create_edge_df, as.list(edges))
  }
  
  graph <- create_graph(nodes, edges, attr_theme = NULL)
  
  # global attributes
  # (we'd prefer to set the default on the root as graphAttributes, but
  # due to a DiagrammeR bug/feature this is not possible). So instead
  # repeating styles redundantly
  
  graphAttributes <- attr(root, "graphStyle")
  #if (is.null(graphAttributes)) graphAttributes <- ""
  #nodeAttributes <- GetDefaultStyles(root, type = "node")
  #edgeAttributes <- GetDefaultStyles(root, type = "edge")
  nodeAttributes <- NULL
  edgeAttributes <- NULL
  
  #graph <- set_global_graph_attrs(graph, "layout", "dot", "graph")
  
  graph <- add_global_graph_attrs(graph, 
                                  c(names(graphAttributes), names(nodeAttributes), names(edgeAttributes)), 
                                  c(graphAttributes, nodeAttributes, edgeAttributes), 
                                  c(rep('graph', length(graphAttributes)), rep('node', length(nodeAttributes)), rep('edge', length(edgeAttributes))))
  
  return (graph)
  
}



GetEdgeStyleFactory <- function(style) {
  style <- force(style)
  function(node = node, origNode = node) {
    myes <- GetStyle(node, style, "edge")
    #if (is.null(myes)) myes <- ""
    myes
  }
}


#' @param inherit If TRUE, then children will inherit this node's style. 
#' Otherwise they inherit from this node's parent. Note that the inherit 
#' always applies to the node, i.e. all style attributes of a node and not 
#' to a single style attribute.
#' 
#' @param keepExisting If TRUE, then style attributes are added to possibly
#' existing style attributes on the node. 
#' 
#' @rdname ToDiagrammeRGraph
#' 
#' @export
SetNodeStyle <- function(node, 
                         inherit = TRUE,
                         keepExisting = FALSE,
                         ...) {
  SetStyle(node, "node", inherit, keepExisting, ...)
}


#' @rdname ToDiagrammeRGraph
#' @export
SetEdgeStyle <- function(node,
                         inherit = TRUE,
                         keepExisting = FALSE,
                         ...) {
  SetStyle(node, "edge", inherit, keepExisting, ...)
}

SetStyle <- function(node,
                     type = c("node", "edge"),
                     inherit = TRUE,
                     keepExisting = FALSE,
                     ...) {
  type <- type[1]
  an <- paste0(type, "Style")
  ain <- paste0(type, "StyleInherit")
  if (keepExisting) {
    ll <- attr(node, an)
    ll <- c(ll, list(...))
  } else ll <- list(...)
  attr(node, an) <- ll
  attr(node, ain) <- inherit
}



#' @rdname ToDiagrammeRGraph 
#' @export
SetGraphStyle <- function(root,
                          keepExisting = FALSE,
                          ...) {
  if (keepExisting) {
    ll <- attr(root, "graphStyle")
    ll <- c(ll, list(...))
  } else ll <- list(...)
  attr(root, "graphStyle") <- ll
}


GetStyle <- function(node, styleName, type = c("node", "edge"), origNode = node) {
  type <- type[1]
  inh <- attr(node, paste0(type, "StyleInherit"))
  res <- attr(node, paste0(type, "Style"))[[styleName]]
  if (!is.null(res)) {
    if (!isRoot(node)) {
      if (identical(node, origNode) || (inh && !styleName %in% c("label", "tooltip"))) {# either on myself or inheritable
        if (is.function(res)) res <- res(origNode)
        return (res)
      }
    } else {
      #root
      if (is.function(res)) res <- res(origNode)
      return (res)
    }
  }
  #recursion exit criteria
  if (isRoot(node)) return (NULL)

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
    isFun <- sapply(res, is.function)
    res <- res[!isFun]
    if (length(res) == 0) return (NULL)
    #res <- paste(names(res), paste0("'", res, "'"), sep = " = ", collapse = ", ")
    return (res) 
  } else return (NULL)
}

