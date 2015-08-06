

#' Format a Number as a Percentage
#' 
#' This utility method can be used as a format function when converting trees to a \code{data.frame}
#' 
#' @param x A number
#' @param digits The number of digits to print
#' @param format The format to use
#' @param ... Any other argument passed to formatC
#' @return A string corresponding to x, suitable for printing
#' 
#' @examples
#' data(acme)
#' print(acme, prob = acme$Get("p", format = FormatPercent))
#' 
#' @seealso formatC
#' @export
FormatPercent <- function(x, digits = 2, format = "f", ...) {
  ifelse(is.na(x), "", paste(formatC(100 * x, format = format, digits = digits, ...), "%"))
}

#' Format a Number as a Decimal
#' 
#' Simple function that can be used as a format function when converting trees to a \code{data.frame}
#' 
#' @param x a numeric scalar or vector
#' @param digits the number of digits to print after the decimal point
#' @return A string corresponding to x, suitable for printing
#' 
#' @examples
#' data(acme)
#' print(acme, prob = acme$Get("p", format = function(x) FormatFixedDecimal(x, 4)))
#' 
#' @export
FormatFixedDecimal <- function(x, digits = 3) {
  ifelse(is.na(x), "", sprintf(paste0("%.",digits, "f"),x))
}





#'   Calculates the height of a \code{Node} given the hight of the root.
#'   
#'   This function puts leafs at the bottom (not hanging), and makes edges equally long.
#'   Useful for easy plotting with third-party packages, e.g. if you have no specific height 
#'   attribute, e.g. with #'   \code{\link{as.dendrogram.Node}}, \code{\link{ToNewick}}, 
#'   and \code{\link{as.phylo.Node}}
#'   
#'   @param node The node
#'   @param rootHeight The height of the root
#'   
#'   @examples
#'   data(acme)
#'   dacme <- as.dendrogram(acme, heightAttribute = function(x) Height(x, 200))
#'   plot(dacme, center = TRUE)
#'   
#'   @export
Height <- function(node, rootHeight = 100) {
  if (node$isRoot) return ( rootHeight )
  if (node$isLeaf) return ( 0 )
  h <- Height(node$parent, rootHeight) * (1 - 1 / node$depth)
  return (h)
}


#' Create a tree for demo and testing
#'
#' @param levels the number of levels
#' @param children the number of children per node
#' @param parent the parent node (for recursion)
#'
#' @export
CreateDummyTree <- function(levels = 5, children = 3, parent = Node$new("1")) {
  if (levels == 0) return()
  for (i in 1:children) {
    child <- parent$AddChild(as.character(i))
    CreateDummyTree(levels - 1, children, child)
  }
  return (parent)
}





#' Create a tree for demo and testing
#'
#' @param nodes The number of nodes to create
#' @param previous the previous node (for recursion)
#' @param id The id (for recursion)
#'
#' @export
CreateRandomTree <- function(nodes = 100, root = Node$new("1"), id = 1) {
  if (nodes == 0) return()
  dpth <- root$depth
  lvl <- sample(1:dpth, 1, rep(1/dpth))
  t <- Traverse(root, filterFun = function(x) x$level == lvl)
  parent <- sample(t, 1)[[1]]
  parent$AddChild(as.character(id + 1))
  CreateRandomTree(nodes - 1, root = root, id = id + 1)
  return (root)
}



PruneNaive <- function(x, limit) {
  xc <- Clone(x)
  k <- 2
  tc <- xc$totalCount
  xc$Set(.id = 1:tc)
  toBeCropped <- tc - limit
  
  while(xc$totalCount > limit && xc$depth > 1) { 
    
    #prune leaves
    xc$Set(.co = NULL)
    t <- Traverse(xc, filterFun = function(x) x$isLeaf && x$position > k)
    df <- data.frame(id = Get(t, ".id"),
                     parentCount = Get(t, function(x) x$parent$count), 
                     level = Get(t, "level"),
                     parentId = Get(t, function(x) x$parent$id)
    )
    
    df <- df[order(df$parentCount, 
                   df$level, 
                   df$parentId,
                   decreasing = TRUE),]
    df$co <- 1:dim(df)[1]
    df <- df[order(df$id),]
    Set(t, .co = df$co)
    cnt <- xc$totalCount
    xc$Prune(function(x) length(x$.co) == 0 || x$.co > toBeCropped)
    toBeCropped <- toBeCropped - (cnt - xc$totalCount)
    
    xc$Set(.co = NULL)
    t <- Traverse(xc, filterFun = function(x) x$depth == 2 && x$count == k)
    df <- data.frame(id = Get(t, ".id"),
                     level = Get(t, "level")
                     )
    
    df <- df[order(df$level,
                   df$id,
                   decreasing = TRUE),]
    df$co <- (1 + k) * (1:dim(df)[1])
    df <- df[order(df$id),]
    Set(t, .co = df$co)
    cnt <- xc$totalCount
    xc$Prune(function(x) length(x$.co) == 0 || x$.co > (toBeCropped + k))
    toBeCropped <- toBeCropped - (cnt - xc$totalCount)
    
  }
  t <- Traverse(x)
  df <- data.frame(id = 1:x$totalCount,
                   count = Get(t, "count"),
                   totalCount = Get(t, "totalCount"))
  
  t <- Traverse(xc)
  ids <- Get(t, ".id")
  df <- df[df$id %in% ids, ]
  Set(t, 
      .originalTotalCount = df$totalCount,
      .originalCount = df$count)
  
  Do(t, function(x) {
    if(x$count < x$.originalCount) {
      nds <- x$.originalCount - x$count
      sub <- x$.originalTotalCount - x$totalCount - nds
      x$AddChild(paste0("... ", nds, " nodes w/ ", sub, " sub"))
    }
  })
  
  
  x <- xc
  
}

