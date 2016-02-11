
#'@rdname ToGraphViz
#'@import DiagrammeR
#'
#'@param x The root node of the data.tree structure to plot
#'@param engine string for the Graphviz layout engine; can be dot (default), 
#'neato, circo, or twopi. For more information see https://github.com/mdaines/viz.js#usage.
#'@inheritParams ToDataFrameNetwork
#'
#'@export
plot.Node <- function(x, ..., direction = c("climb", "descend"), pruneFun = NULL, engine = "dot") {
  dotLng <- ToGraphViz(x, direction, pruneFun)
  grViz(dotLng, engine)
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
#' SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
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


#' @param inherit If TRUE, then children will inherit this node's style. 
#' Otherwise they inherit from this node's parent. Note that the inherit 
#' always applies to the node, i.e. all style attributes of a node and not 
#' to a single style attribute.
#' 
#' @param keepExisting If TRUE, then style attributes are added to possibly
#' existing style attributes on the node. 
#' 
#' @rdname ToGraphViz
#' 
#' @export
SetNodeStyle <- function(node, 
                         inherit = TRUE,
                         keepExisting = FALSE,
                         ...) {
  SetStyle(node, "node", inherit, keepExisting, ...)
}


#' @rdname ToGraphViz
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



#' @rdname ToGraphViz 
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
    if (!node$isRoot) {
      if (identical(node, origNode) || (inh && !styleName %in% c("label", "tooltip"))) {# either on myself or inheritable
        if (is.function(res)) res <- res(origNode)
        return (res)
      }
    } else {
      #root
      if (identical(node, origNode)) {
        #directly asked on root
        if (styleName %in% c("label", "tooltip") || is.function(res)) {
          if (is.function(res)) res <- res(origNode)
          return (res)
        }
      } else {
        #inherited are only functions
        if (is.function(res)) {
          return (res(origNode))
        }
      }
      
    }
  }
  #recursion exit criteria
  if (node$isRoot) return (NULL)

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
    res <- paste(names(res), paste0("'", res, "'"), sep = " = ", collapse = ", ")
    return (res) 
  } else return (NULL)
}

