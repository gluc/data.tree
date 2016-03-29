#' Convert a \code{data.tree} structure to a \code{data.frame}
#' 
#' 
#' @param x The root \code{Node} of the tree or sub-tree to be convert to a data.frame
#' @param ... the attributes to be added as columns of the data.frame. See \code{\link{Get}} for details.
#' If a specific Node does not contain the attribute, \code{NA} is added to the data.frame.
#' @param traversal any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'. See \code{\link{Traverse}} for details.
#' @param direction when converting to a network, should the edges point from root to children ("climb") or from child to parent ("descend")?
#' @param type when converting type columns, the \code{type} is the discriminator, i.e. an attribute (e.g. field name) of each node
#' @param prefix when converting type columns, the prefix used for the column names. Can be NULL to omit prefixes.
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
#' ToDataFrameNetwork(acme, "cost", "p", direction = "climb")
#' ToDataFrameTable(acme, "cost", "p")
#' ToDataFrameTypeCol(acme)
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
#' acme$Accounting$Head <- "Mrs. Numright"
#' acme$Research$Head <- "Mr. Stein"
#' acme$IT$Head <- "Mr. Squarehead"
#' ToDataFrameTable(acme, department = function(x) x$parent$name, "name", "Head", "cost")
#'   
#' #complex TypeCol
#' acme$IT$Outsource$AddChild("India")
#' acme$IT$Outsource$AddChild("Poland")
#' acme$Set(type = c('company', 'department', 'project', 'project', 'department', 
#'                   'project', 'project', 'department', 'program', 'project', 
#'                   'project', 'project', 'project'
#'                   )
#'         )
#' print(acme, 'type')
#' ToDataFrameTypeCol(acme, type = 'type')
#'       
#' @inheritParams Prune
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
  
  if(!x$isRoot || length(pruneFun) > 0) {
    #clone s.t. x is root (for pretty level names)
    x <- Clone(x, attributes = TRUE)
    if (length(pruneFun) > 0) x$Prune(pruneFun)
    x$parent <- NULL
  }
  
  t <- Traverse(x, traversal = traversal, filterFun = filterFun)
  
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
ToDataFrameTree <- function(x, ..., pruneFun = NULL) {
  as.data.frame(x, row.names = NULL, optional = FALSE, ..., pruneFun = pruneFun)
}

#' @rdname as.data.frame.Node
#'
#'
#' @return ToDataFrameTable: a \code{data.frame}, where each row represents a leaf \code{Node} in the tree or sub-tree
#' spanned by \code{x}, possibly pruned according to \code{pruneFun}.
#' 
#' 
#' @export
ToDataFrameTable <- function(x, ..., pruneFun = NULL) {
  df <- as.data.frame(x, row.names = NULL, optional = FALSE, ..., filterFun = isLeaf, pruneFun = pruneFun, inheritFromAncestors = TRUE)
  df[,-1]
}



#' @rdname as.data.frame.Node
#' 
#' @return ToDataFrameNetwork: a \code{data.frame}, where each row represents a \code{Node} in the tree or sub-tree 
#' spanned by \code{x}, possibly pruned according to \code{pruneFun}. The first column is called 'from', while the
#' second is called 'to', describing the parent to child edge (for direction "climb") or the child to parent edge (for direction "descend").
#' If \code{\link{AreNamesUnique}} is TRUE, then the Network is
#' based on the \code{Node$name}, otherwise on the \code{Node$pathString}
#' 
#' 
#' @export
ToDataFrameNetwork <- function(x, 
                               ..., 
                               direction = c("climb", "descend"),
                               pruneFun = NULL, 
                               inheritFromAncestors = FALSE) {
  direction <- direction[1]
  if(!AreNamesUnique(x)) GetName <- function(x) x$pathString
  else GetName <- function(x) x$name
  t <- Traverse(x, traversal = "level", pruneFun = pruneFun)
  children <- Get(t, function(x) GetName(x))
  parents <- Get(t, function(x) GetName(x$parent))
  
  if (direction == "descend") df <- data.frame(from = children, 
                                             to = parents, 
                                             stringsAsFactors = FALSE)

  else if(direction == "climb") df <- data.frame(from = parents, 
                                                  to = children, 
                                                  stringsAsFactors = FALSE)
    
  else stop(paste0("direction ", direction, " unknown. Must be either climb or descen."))
  
  df2 <- ToDataFrameTree(x, ..., traversal = "level", pruneFun = pruneFun, inheritFromAncestors = inheritFromAncestors)[,-1, drop = FALSE]
  
  df <- cbind(df, df2)
  df <- df[-1,]
  rownames(df) <- 1:dim(df)[1]
  return (df)
}



#' @rdname as.data.frame.Node
#' 
#' @return ToDataFrameTypeCol: a \code{data.frame} in table format (i.e. where each row represents a leaf in the tree or sub-tree 
#' spanned by \code{x}), possibly pruned according to \code{pruneFun}. In addition to \code{...}, each distinct
#' \code{type} is output to a column.
#' 
#' 
#' @export
ToDataFrameTypeCol <- function(x, 
                               ...,
                               type = 'level',
                               prefix = type,
                               pruneFun = NULL) {
  cols <- unique(c(x$Get(type, filterFun = isNotLeaf), x$Get(type, filterFun = isLeaf)))

  
  pathArgs <- GetPathArgV(cols, type)
  if (is.null(prefix)) names(pathArgs) <- as.character(cols)
  else names(pathArgs) <- paste0(prefix, '_', cols)
  do.call(ToDataFrameTable, c(x, pathArgs, ...))
}


GetPathArg <- function(n, type) {
  lvl <- force(n)
  f <- function(leaf) {
    path <- leaf$Get(type, traversal = 'ancestor')
    name <- names(path[path == lvl])
    if (length(name) == 0) name <- NA
    return (name)
  }
  return (f)
}

GetPathArgV <- Vectorize(GetPathArg, vectorize.args = 'n')



#' Convert a \code{data.frame} to a \code{data.tree} structure
#' 
#' @param x The data.frame in the required format.
#' @param ... Any other argument implementations of this might need
#' @param mode Either "table" (if x is a data.frame in tree or table format) or "network"
#' @param na.rm If \code{TRUE}, then NA's are treated as NULL and values will not be set on nodes
#'
#' @return The root \code{Node} of the \code{data.tree} structure
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
#' #Network
#' x <- ToDataFrameNetwork(acme, "p", "cost", direction = "climb")
#' x
#' xN <- FromDataFrameNetwork(x)
#' print(xN, "p", "cost")
#' 
#' @seealso \code{\link{as.data.frame.Node}}
#' @family as.Node
#' 
#' @export
as.Node.data.frame <- function(x, 
                               ..., 
                               mode = c("table", "network"),
                               pathName = 'pathString', 
                               pathDelimiter = '/', 
                               colLevels = NULL,
                               na.rm = TRUE) {
  
  mode <- mode[1]
  if (mode == 'table') return (FromDataFrameTable(x, pathName, pathDelimiter, colLevels, na.rm))
  else if (mode == 'network') return (FromDataFrameNetwork(x))
  else stop(paste0("Mode ", mode, " unknown."))
  
}


#' @rdname as.Node.data.frame
#' 
#' @param table a \code{data.frame} in table or tree format, i.e. having a row for each leaf (and optionally
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
#' @param network A \code{data.frame} in network format, i.e.
#' it must adhere to the following requirements:
#' \itemize{
#'  \item{It must contain as many rows as there are nodes, excluding the root}
#'  \item{Its first and second columns contain the network relationships. This can be either climbing (from parent to children) or descending (from child to parent)}
#'  \item{Its subsequent columns contain the attributes to be set as fields on the nodes}
#'  \item{It must contain a single root}
#'  \item{There are no cycles in the network}
#' }
#' 
#' @import methods
#' 
#' @export
FromDataFrameNetwork <- function(network) {
  
  if (!is(network, "data.frame")) stop("network must be a data.frame")
  if (dim(network)[2] < 2) stop("network must hold the relationships in the first two columns")
  
  if (length(unique(network[ , 1])) > length(unique(network[ , 2]))) {
    children <- network[ , 1]
    parents <- network[ , 2]
  } else {
    children <- network[ , 2]
    parents <- network[ , 1]
  }
  
  rootName <- unique(parents[!(parents %in% children)])
  if (length(rootName) != 1) stop("Cannot find root name. network is not a tree!")
  
  root <- Node$new(rootName)
  AddChildren <- function(node) {
    childrenIdxs <- which(parents == node$name)
    for (idx in childrenIdxs) {
      nodeName <- children[idx]
      child <- node$AddChild(nodeName)
      if (dim(network)[2] > 2) {
        for (j in 3:dim(network)[2]) {
          vlu <- network[idx, j]
          if (!is.na(vlu)) {
            nm <- names(network)[j]
            if (!nm %in% NODE_RESERVED_NAMES_CONST) child[[nm]] <- network[idx, j]
          }
        }
      }
      AddChildren(child)
    }
  }
  AddChildren(root)
 
  return (root)
}

