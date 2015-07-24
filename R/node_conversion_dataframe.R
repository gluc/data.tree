#' Convert a \code{Node} to a \code{data.frame}
#' 
#' @param x The root node of a tree or sub-tree to convert to a data.frame
#' @param ... the attributes to be added as columns of the data.frame. See \code{\link{Get}} for details.
#' If a specific Node does not contain the attribute, \code{NA} is added to the data.frame.
#' @param traversal any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'. See \code{\link{Traverse}} for details.
#' @param pruneFun a function taking a \code{Node} as an argument. See \code{\link{Traverse}} for details.
#' @param filterFun a function taking a \code{Node} as an argument. See \code{\link{Traverse}} for details.
#' @param inheritFromAncestors if FALSE, and if the attribute is a field or a method, then only a \code{Node} itself is
#' searched for the field/method. If TRUE, and if the \code{Node} does not contain the attribute, then ancestors are also searched.
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. 
#' Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names 
#' (to syntactic names: see make.names) is optional.
#'
#'   
#' @examples
#' data(acme)
#' acme$fieldsAll
#' as.data.frame(acme, row.names = NULL, optional = FALSE, "cost", "p") 
#' 
#' ToDataFrameTree(acme, "cost", "p")
#' ToDataFrameTaxonomy(acme, "cost", "p")
#' ToDataFrameTable(acme, "cost", "p")
#' 
#' #use the pruneFun:
#' acme$Do(function(x) x$totalCost <- Aggregate(x, "cost", sum), traversal = "post-order")
#' ToDataFrameTree(acme, "totalCost", pruneFun = function(x) x$totalCost > 300000)
#' 
#' #inherit
#' acme$Set(floor = c(1, 2, 3), filterFun = function(x) x$level == 2)
#' as.data.frame(acme, row.names = NULL, optional = FALSE, "floor", inheritFromAncestors = FALSE)  
#' as.data.frame(acme, row.names = NULL, optional = FALSE, "floor", inheritFromAncestors = TRUE)  
#' 
#' #using a function as an attribute:
#' acc <- acme$Climb("Accounting")
#' acc$Head <- "Mrs. Numright"
#' rs <- acme$Climb("Research")
#' rs$Head <- "Mr. Stein"
#' it <- acme$Climb("IT")
#' it$Head <- "Mr. Squarehead"
#' ToDataFrameTable(acme, department = function(x) x$parent$name, "name", "Head", "cost")
#'   
#'       
#' @export
as.data.frame.Node <- function(x, 
                               row.names = NULL, 
                               optional = FALSE, 
                               ..., 
                               traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),
                               pruneFun = NULL,
                               filterFun = NULL,
                               inheritFromAncestors = FALSE
) {
  
  traversal <- traversal[1]
  
  if(!x$isRoot) {
    #clone s.t. x is root (for pretty level names)
    x <- Clone(x)
  }
  
  t <- Traverse(x, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
  
  df <- data.frame( levelName = format(Get(t, 'levelName')),
                    row.names = row.names,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (length(names(cols)) > 0 && nchar(names(cols)[i]) > 0) colName <- names(cols)[i]
    else if (is.character(col)) colName <- col
    else stop(paste0("Cannot infer column name for ... arg nr ", i))
    if (length(col) > 1) {
      it <- col
    } else {
      it <- Get(t, col, inheritFromAncestors = inheritFromAncestors)
    }
    df[colName] <- it
    
  }
  
  return (df)
  
}



#' @rdname as.data.frame.Node
#' @return ToDataFrameTree: a \code{data.frame}, where each row represents a \code{Node} in the tree or sub-tree 
#' spanned by \code{x}, possibly pruned according to \code{pruneFun}.
#' 
#' @export
ToDataFrameTree <- function(X, ..., pruneFun = NULL) {
  as.data.frame(X, row.names = NULL, optional = FALSE, ..., pruneFun = pruneFun)
}

#' @rdname as.data.frame.Node
#'
#'
#' @return ToDataFrameTable: a \code{data.frame}, where each row represents a leaf \code{Node} in the tree or sub-tree
#' spanned by \code{x}, possibly pruned according to \code{pruneFun}.
#' 
#' 
#' @export
ToDataFrameTable <- function(X, ..., pruneFun = NULL) {
  df <- as.data.frame(X, row.names = NULL, optional = FALSE, ..., filterFun = isLeaf, pruneFun = pruneFun, inheritFromAncestors = TRUE)
  df[,-1]
}



#' @rdname as.data.frame.Node
#' 
#' @return ToDataFrameTaxonomy: a \code{data.frame}, where each row represents a \code{Node} in the tree or sub-tree 
#' spanned by \code{x}, possibly pruned according to \code{pruneFun}. The first column is called 'children', while the
#' second is called 'parents', describing the parent to child edge. The third column is caled 'level'. x itself is not returned.
#' 
#' @export
ToDataFrameTaxonomy <- function(x, 
                                ..., 
                                pruneFun = NULL, 
                                inheritFromAncestors = FALSE) {
  
  t <- Traverse(x, traversal = "level", pruneFun = pruneFun)
  children <- Get(t, "name")
  parents <- Get(t, function(x) x$parent$name)
  level <- Get(t, "level")
  df <- data.frame(children = children, parents = parents, level = level, stringsAsFactors = FALSE)

  df2 <- ToDataFrameTree(x, ..., traversal = "level", pruneFun = pruneFun, inheritFromAncestors = inheritFromAncestors)[,-1, drop = FALSE]
  
  df <- cbind(df, df2)
  df <- df[-1,]
  rownames(df) <- 1:dim(df)[1]
  return (df)
}






#' Convert a data.frame to a data.tree
#' 
#' @param x The data.frame in the required format.
#' @param ... Any other argument
#' @param na.rm If \code{TRUE}, then NA's are treated as NULL and values will not be set on nodes
#'
#' @return The root \code{Node} of the \code{data.tree}
#'  
#' @examples
#' data(acme)
#' 
#' #Tree
#' x <- ToDataFrameTree(acme, "pathString", "p", "cost")
#' x
#' xN <- as.Node(x)
#' print(xN, "p", "cost")
#' 
#' #Table
#' x <- ToDataFrameTable(acme, "pathString", "p", "cost")
#' x
#' xN <- FromDataFrameTable(x)
#' print(xN, "p", "cost")
#' 
#' #More complex Table structure, using colLevels
#' acme$Set(floor = c(1, 2, 3),  filterFun = function(x) x$level == 2)
#' x <- ToDataFrameTable(acme, "pathString", "floor", "p", "cost") 
#' x
#' xN <- FromDataFrameTable(x, colLevels = list(NULL, "floor", c("p", "cost")), na.rm = TRUE)
#' print(xN, "floor", "p", "cost")
#'  
#' #Taxonomy
#' x <- ToDataFrameTaxonomy(acme, "p", "cost")
#' x
#' xN <- FromDataFrameTaxonomy(x)
#' print(xN, "p", "cost")
#' 
#' @seealso \code{\link{as.data.frame.Node}}
#' 
#' @export
as.Node.data.frame <- function(x, 
                               ..., 
                               mode = c("table", "taxonomy"),
                               pathName = 'pathString', 
                               pathDelimiter = '/', 
                               colLevels = NULL,
                               na.rm = TRUE) {
  
  mode <- mode[1]
  if (mode == 'table') return (FromDataFrameTable(x, pathName, pathDelimiter, colLevels, na.rm))
  else if (mode == 'taxonomy') return (FromDataFrameTaxonomy(x))
  else stop(paste0("Mode ", mode, " unknown."))
  
}


#' @rdname as.Node.data.frame
#' 
#' @param table a \code{data.frame} in *table* or *tree* format, i.e. having a row for each leaf (and optionally
#' for additional nodes). There should be a column called \code{pathName}, separated by \code{pathDelimiter}, 
#' describing the path of each row. 
#' @param pathName The name of the column in x containing the path of the row
#' @param pathDelimiter The delimiter used
#' @param colLevels Nested list of column names, determining on what node levels the attributes are written to.
#' 
#' @export
FromDataFrameTable <- function(table, 
                               pathName = 'pathString', 
                               pathDelimiter = '/', 
                               colLevels = NULL,
                               na.rm = TRUE) {
  root <- NULL
  mycols <- names(table)[ !(names(table) %in% c(NODE_RESERVED_NAMES_CONST, pathName)) ]
  for (i in 1:nrow(table)) {
    myrow <- table[ i, ]
    mypath <- myrow[[pathName]]
    myvalues <- myrow[!colnames(myrow) == pathName]
    
    #create node and ancestors if necessary (might already have been created)
    paths <- strsplit(mypath, pathDelimiter, fixed = TRUE)[[1]]
    paths <- paths[paths!=""]
    if (is.null(root)) root <- Node$new(paths[1])
    mynode <- root
    colsToSet <- mycols
    colsToSetForLeaf <- mycols
    for (path in paths[-1]) {
      child <- Climb(mynode, path)
      
      if( is.null(child)) {
        mynode <- mynode$AddChild(path)
      } else {
        mynode <- child
      }
      
      if( length(colLevels) >= mynode$level ) {
        colsToSet <- intersect(colLevels[[mynode$level]], mycols) 
        
        #fill values on appropriate level
        for (mycol in colsToSet) {
          if ( !( na.rm && is.na(myrow[[mycol]]) )) {
            mynode[[mycol]] <- myrow[[mycol]]
          }
        }
        colsToSetForLeaf <- colsToSetForLeaf[!(colsToSetForLeaf %in% colsToSet)]
      }
      
    }
    
    #put the rest in the leaf
    for (mycol in colsToSetForLeaf) {
      if ( !( na.rm && is.na(myrow[[mycol]]) )) {
        mynode[[mycol]] <- myrow[[mycol]]
      }
      #remove
    }    
    
    
  }
  return (root)
  
}

#' @rdname as.Node.data.frame
#' 
#' @param taxonomy A \code{data.frame} in taxonomy format, i.e.
#' it must adhere to the following requirements:
#' \itemize{
#'  \item{It must contain as many rows as there are nodes, excluding the root}
#'  \item{Its first column must be called *children*, and contain the name of each node, whereby each name must be unique within a node's level}
#'  \item{Its second column must be called *parents*, and contain the name of the parent of each node}
#'  \item{Its third column must be called *level*, and contain the level of the node}
#'  \item{The rows must be ordered by level, increasing}
#' }
#' 
#' @export
FromDataFrameTaxonomy <- function(taxonomy) {
  if (any(names(taxonomy)[1:3] != c("children", "parents", "level"))) stop("taxonomy is not a taxonomy. First three columns must be children, parents, and level, respectively.")
  rootName <- unique(taxonomy$parents[!(taxonomy$parents %in% taxonomy$children)])
  if (length(rootName) != 1) stop("Cannot find root name. taxonomy is not a taxonomy.")
  root <- Node$new(rootName)
  GetNodeByName <- function(name, level) {
    t <- Traverse(root, filterFun = function(x) x$name == name && x$level == level)
    if (length(t) == 0) stop(paste0("Cannot find node ", name, ". taxonomy is not a taxonomy."))
    if (length(t) > 1) stop(paste0("More than one node named ", name, ". taxonomy is not a taxonomy."))
    t[[1]]
  }
  for (i in 1:dim(taxonomy)[1]) {
    parent <- GetNodeByName(taxonomy[i, "parents"], taxonomy[i, "level"] - 1)
    child <- parent$AddChild(taxonomy[i, "children"])
    if (dim(taxonomy)[2] > 3) {
      for (j in 4:dim(taxonomy)[2]) {
        child[[names(taxonomy)[j]]] <- taxonomy[i, j]
      }
    }
  }
  return (root)
}

