#' Convert a nested \code{list} structure to a \code{data.tree} structure
#' 
#' @param x The \code{list} to be converted.
#' @param mode How the list is structured. "simple" (the default) will interpret any list to be a child. "explicit" 
#' assumes that children are in a nested list called \code{childrenName}
#' @param nameName The name of the element in the list that should be used as the name, can be NULL if the children lists are named
#' @param childrenName The name of the element that contains the child list (applies to mode 'explicit' only).
#' @param nodeName The name x will get (only applies if no nameName is available, this is useful for some conversions where th
#' name is not given explicitly)
#' @param ... Any other argument to be passed to generic sub implementations
#' 
#' @examples
#' kingJosephs <- list(name = "Joseph I",
#'                     spouse = "Mary",
#'                     born = "1818-02-23",
#'                     died = "1839-08-29",
#'                     children = list(
#'                                     list(name = "Joseph II",
#'                                          spouse = "Kathryn",
#'                                          born = "1839-03-28",
#'                                          died = "1865-12-19"),
#'                                     list(name = "Helen",
#'                                          born = "1840-17-08",
#'                                          died = "1845-01-01")
#'                                     )
#'                    )
#' FromListExplicit(kingJosephs)
#' 
#' kingJosephs <- list(head = "Joseph I",
#'                     spouse = "Mary",
#'                     born = "1818-02-23",
#'                     died = "1839-08-29",
#'                     list(head = "Joseph II",
#'                          spouse = "Kathryn",
#'                          born = "1839-03-28",
#'                          died = "1865-12-19"),
#'                     list(head = "Helen",
#'                          born = "1840-17-08",
#'                          died = "1845-01-01")       
#'                    )
#' FromListSimple(kingJosephs, nameName = "head")
#' 
#' kingJosephs <- list(spouse = "Mary",
#'                     born = "1818-02-23",
#'                     died = "1839-08-29",
#'                     `Joseph II` = list(spouse = "Kathryn",
#'                                        born = "1839-03-28",
#'                                        died = "1865-12-19"),
#'                     Helen = list(born = "1840-17-08",
#'                                  died = "1845-01-01")
#'                                  
#'                    )
#' FromListSimple(kingJosephs, nodeName = "Joseph I")
#'   
#' 
#' @family as.Node
#' 
#' @export
as.Node.list <- function(x, mode = c("simple", "explicit"), nameName = "name", childrenName = "children", nodeName = NULL, ...) {
  mode <- mode[1]
  if (is.null(nameName) || !(nameName %in% names(x))) {
    if (length(nodeName)==0) myName <- tempfile(pattern = '', tmpdir = '')
    else myName <- nodeName
  } else {
    myName <- x[[nameName]]
  }
  if (myName %in% NODE_RESERVED_NAMES_CONST) myName <- paste0(myName, "2")
  n <- Node$new(as.character(myName))
  
  fields <- names(x)[!(names(x) %in% NODE_RESERVED_NAMES_CONST)]
  if (length(nameName) > 0) fields <- fields[fields != nameName]
  fields <- fields[nchar(fields) > 0]
  for (field in fields) {
    v <- x[[field]]
    if(mode == 'simple' && class(v) == "list") {
    } else n[[field]] <- v
  }
  
  #children
  if(mode == 'simple') children <- x[sapply(x, is.list)]
  else if(mode == 'explicit') {
    children <- x[[childrenName]]
  }
  if (length(children) == 0) return (n)
  for (i in 1:length(children)) {
    if (!is.null(names(children))) {
      childName <- names(children)[i]
    } else {
      childName <- ""
    }
    child <- children[[i]]
    childNode <- as.Node.list(child, mode, nameName, childrenName, nodeName = childName, ...)
    n$AddChildNode(childNode)
    
  }
  
  
  return (n)
  
}

#' @rdname as.Node.list
#' 
#' @param explicitList A \code{list} in which children are in a separate nested list called \code{childrenName}.
#' 
#' @export
FromListExplicit <- function(explicitList, nameName = "name", childrenName = "children", nodeName = NULL) {
  as.Node.list(explicitList, mode = "explicit", nameName = nameName, childrenName = childrenName, nodeName = nodeName)
}


#' @rdname as.Node.list
#' 
#' @param simpleList A \code{list} in which children are stored as nested list alongside other fields. Any list is
#' interpreted as a child \code{Node}
#' 
#' @export
FromListSimple <- function(simpleList, nameName = "name", nodeName = NULL) {
  as.Node.list(simpleList, mode = "simple", nameName = nameName, nodeName = nodeName)
}






#' Convert a \code{data.tree} structure to a list-of-list structure
#' 
#' @param x The Node to convert
#' @param mode How the list is structured. "simple" (the default) will add children directly as nested lists.
#' "explicit" puts children in a separate nested list called \code{childrenName}
#' @param unname If TRUE, then the nested children list will not have named arguments. This
#' can be useful e.g. in the context of conversion to JSON, if you prefer the children to be
#' an array rather than named objects.
#' @param nameName The name that should be given to the name element
#' @param childrenName The name that should be given to the children nested list
#' @param rootName The name of the node. If provided, this overrides \code{Node$name}
#' @param ... Additional parameters (ignored)
#' 
#' @examples
#' data(acme)
#' 
#' str(ToListSimple(acme))
#' 
#' str(ToListExplicit(acme))
#' str(ToListExplicit(acme, unname = TRUE))
#' str(ToListExplicit(acme, unname = TRUE, nameName = "id", childrenName = "descendants"))
#'
#' 
#' @export
as.list.Node <- function(x, 
                         mode = c("simple", "explicit"),
                         unname = FALSE, 
                         nameName = ifelse(unname, "name", ""), 
                         childrenName = 'children',
                         rootName = '',
                         ...) {
  mode <- mode[1]
  self <- x
  res <- list()
  
  if (nchar(rootName) != 0) myname <- rootName
  else myname <- x$name
  
  if (nchar(nameName) != 0 || nchar(rootName) != 0 || x$isRoot) {
    l_nameName <- nameName
    if(nchar(nameName) == 0) l_nameName <- "name"
    res[l_nameName] <- myname
  }
  
  fields <- self$fields
  fields <- fields[!is.function(fields) && !is.environment(fields)]
  
  for (fieldName in fields) res[[fieldName]] <- self[[fieldName]]
    
  if(!self$isLeaf) {
    kids <- lapply(self$children, FUN = function(x) as.list.Node(x, mode, unname, nameName, childrenName, ...))
    if(mode == "explicit") {
      res[[childrenName]] <- kids
      if (unname) res[[childrenName]] <- unname(res[[childrenName]])
    } else if(mode == "simple") {
      res <- c(res, kids)
    } else {
      stop(paste0("Mode ", mode, " unknown"))
    }
  }
  return (res)
  
}


#' @rdname as.list.Node
#' 
#' @export
ToListSimple <- function(x, nameName = "name") {
  as.list.Node(x, mode = "simple", nameName = nameName)
}


#' @rdname as.list.Node
#'  
#'
#' @export 
ToListExplicit <- function(x, unname = FALSE, nameName = ifelse(unname, "name", ""), childrenName = 'children') {
  as.list.Node(x, mode = "explicit", unname = unname, nameName = nameName, childrenName = childrenName) 
}
