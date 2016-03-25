#' Convert a nested \code{list} structure to a \code{data.tree} structure
#' 
#' @param x The \code{list} to be converted.
#' @param mode How the list is structured. "simple" (the default) will interpret any list to be a child. "explicit" 
#' assumes that children are in a nested list called \code{childrenName}
#' @param nameName The name of the element in the list that should be used as the name, can be NULL if mode = explicit and
#' the children lists are named, or if an automatic name (running number) should be assigned
#' @param childrenName The name of the element that contains the child list (applies to mode 'explicit' only).
#' @param nodeName A name suggestion for x, if the name cannot be deferred otherwise. This is for example the case for
#' the root with mode explicit and named lists.
#' @param warn If TRUE, then warnings are printed
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
as.Node.list <- function(x, mode = c("simple", "explicit"), nameName = "name", childrenName = "children", nodeName = NULL, warn = TRUE, ...) {
  mode <- mode[1]
  
  #find my name
  if (is.null(nameName) || !(nameName %in% names(x))) {
    if (length(nodeName)==0) myName <- "Root"
    else myName <- nodeName
  } else {
    myName <- x[[nameName]]
  }
  
  if (myName %in% NODE_RESERVED_NAMES_CONST) {
    if (warn) warning(paste0("'", myName, "' is a reserved word and cannot be used as name for a node. Using '", myName, "2' instead!"))
    myName <- paste0(myName, "2")
  }
  
  n <- Node$new(as.character(myName))
  
  #set fields
  
  #find fields that need importing
  fields <- names(x)
  #exclude nameName
  fields <- fields[nchar(fields) > 0]
  if(!is.null(nameName)) fields <- fields[fields != nameName]
  #exclude childrenName if explicit
  if (mode == "explicit") fields <- fields[fields != childrenName]
  #
  
  if (warn) {
    fieldNameIsReserved <- (fields %in% NODE_RESERVED_NAMES_CONST) & !(fields %in% c(nameName, childrenName))
    if (any(fieldNameIsReserved)) warning(paste0("The following names are data.tree reserved words and will be appended with 2: ", paste(fields[fieldNameIsReserved], sep = ", "), "." ))
  }
  
  for (field in fields) {
    v <- x[[field]]
    
    if(mode == 'simple' && class(v) == "list") {
      #any list is interpreted as child, so don't store
    } else {
      fieldNm <- field
      if (field %in% NODE_RESERVED_NAMES_CONST) fieldNm <- paste0(field, "2")
      n[[fieldNm]] <- v
    }
  }
  
  #children
  if (is.character(x)) return (n)
  if (mode == 'simple') children <- x[sapply(x, is.list)]
  else if (mode == 'explicit') children <- x[[childrenName]]
  
  if (length(children) == 0) return (n)
  
  for (i in 1:length(children)) {
    if (any(duplicated(names(children)))) {
      childName <- ""
    } else if (is.character(children)) {
      childName <- children[i]
    } else if (!is.null(names(children))) {
      childName <- names(children)[i]
    } else {
      childName <- ""
    }
    if (nchar(childName) == 0) childName <- i
    child <- children[[i]]
    childNode <- as.Node.list(child, mode, nameName, childrenName, nodeName = childName, warn = warn, ...)
    n$AddChildNode(childNode)
    
  }
  
  
  return (n)
  
}

#' @rdname as.Node.list
#' 
#' @param explicitList A \code{list} in which children are in a separate nested list called \code{childrenName}.
#' 
#' @export
FromListExplicit <- function(explicitList, nameName = "name", childrenName = "children", nodeName = NULL, warn = TRUE) {
  as.Node.list(explicitList, mode = "explicit", nameName = nameName, childrenName = childrenName, nodeName = nodeName, warn = warn)
}


#' @rdname as.Node.list
#' 
#' @param simpleList A \code{list} in which children are stored as nested list alongside other fields. Any list is
#' interpreted as a child \code{Node}
#' 
#' @export
FromListSimple <- function(simpleList, nameName = "name", nodeName = NULL, warn = TRUE) {
  as.Node.list(simpleList, mode = "simple", nameName = nameName, nodeName = nodeName, warn = warn)
}






#' Convert a \code{data.tree} structure to a list-of-list structure
#' 
#' @param x The Node to convert
#' @param mode How the list is structured. "simple" (the default) will add children directly as nested lists.
#' "explicit" puts children in a separate nested list called \code{childrenName}
#' @param unname If TRUE, and if \code{mode} is "explicit", then the nested children list will not have named arguments. This
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
