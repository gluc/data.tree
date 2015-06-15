

#' Find a \code{Node} by its path
#' 
#' 
#' Find returns the \code{Node} at path \code{...}. The path is relative to the \code{Node} on which this method is called. Each argument provided corresponds to an 
#' element in the path, specified by the \code{Node}'s name.
#' 
#' 
#' @param ... the names of the nodes in the path
#' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
#' 
#' @examples
#' data(acme)
#' acme$Find('IT', 'Outsource')$name
#' #This is equivalent to:
#' acme$Find('IT')$Find('Outsource')$name
#' acme$Find('X', 'Y', 'Z')
#'
#'
#' @seealso \code{\link{Node}}
#'
#' @keywords internal
Find = function(...) {
  stop("This method can only be called on a Node!")
}



#' Traverse a Tree and Collect Values
#' 
#' The \code{Get} function is one of the most important ones of the \code{data.tree} package. It lets you traverse a tree
#' and collect values along the way. Alternatively, you can call a method or a function on each \code{Node}.
#' 
#' 
#'   @param attribute determines what is collected during traversal. The attribute can be
#'       \itemize{
#'         \item a.) the name of a field of each \code{Node} in the tree 
#'         \item b.) the name of a Method of each \code{Node}.
#'         \item c.) a function, whose first argument must be a node. In that case, the \code{Get} method calls the function by 
#'         passing \code{...} to the function.
#'        }
#'  @param traversal determines the traversal order. It can be either "pre-order", "post-order", or "ancestor"
#'  @param filterFun allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
#'  Note that if filter returns \code{FALSE}, then the node and its entire subtree are ignored and neither traversed nor returned.
#'  @param assign can be the name of a variable to which we assign the collected values before \code{format} is called.
#'  @param format can be a function that transforms the collected values, e.g. for printing
#'  
#'  @return a vector containing the \code{atrributes} collected during traversal, in traversal order. \code{NULL} is converted
#'  to NA, such that \code{length(Node$Get) == Node$totalCount}
#'  
#'  @examples
#'data(acme)
#'acme$Get("level")
#'acme$Get("totalCount")
#'  
#'calculateAggregateChildCost <- function(node, fun) {
#'  if (node$isLeaf) return(node$cost)
#'  fun(sapply(node$children, function(x) x$averageCost))
#'}
#'
#'myFormat <- function(x) {
#'  format(x, nsmall=2, scientific = FALSE)
#'}
#'
#'acme$Get(calculateAggregateChildCost, 
#'         mean, 
#'         traversal = "post-order", 
#'         assign = "averageCost", 
#'         format = myFormat)
#'  
#' @seealso \code{\link{Node}}
#'  
#' @keywords internal
Get = function(attribute, ..., traversal = "pre-order", filterFun = function(x) TRUE, assign = NULL, format = NULL) {
  stop("This method can only be called on a Node!")
}

#' Traverse a Tree and Assign Values
#' 
#' The method takes a vector as an argument. It traverses the tree, and assigns values to variables, whereby the values are picked
#' from the vector.
#' 
#' 
#' @param ... each argument can be a vector of values to be assigned.
#' @param traversal any of 'pre-order', 'post-order', 'ancestor'
#' @param returnValues if \code{TRUE}, then the non-processed arg passed in ... are returned. 
#' Otherwise the \code{Node} itself is returned for chaining. Mainly for internal use.
#'  
#'  
#' @examples
#' data(acme)
#' acme$Set(departmentId = 1:11, 
#'          head = c("Jack Brown", 
#'                   "Dr. Frank N. Stein", 
#'                   "", 
#'                   "", 
#'                   "Mona Moneyhead", 
#'                   "", 
#'                   "", 
#'                   "Eric Nerdahl", 
#'                   "", 
#'                   "", 
#'                   ""))
#' print(acme, "departmentId", "head")
#'  
#' @seealso \code{\link{Node}}
#'  
#' @keywords internal
Set = function(..., traversal = "pre-order", returnValues = FALSE) {
  stop("This method can only be called on a Node!")
}


#' Traverse a Tree and Perform Aggregation Operations
#' 
#' The \code{Aggregate} method lets you set e.g. a value on the leafs, and then sum them up along the tree.
#' 
#' 
#' @param attribute the attribute that is being called on every node. The attribute can be 
#' field, a property or a method. If the node contains #' the attribute, its value is return. 
#' Otherwise, \code{fun(children$Aggregate(...))} is called. To use the Attribute method, 
#' the attribute must be set on the leaf.
#' @param fun a function to be applied
#' @param ... any arguments to be passed on to fun
#' 
#' @examples
#' data(acme)
#' acme$Aggregate("cost", sum)
#' acme$Get("Aggregate", "cost", sum)
#' print(acme, totalCost = acme$Get("Aggregate", "cost", sum))
#' 
#' @seealso \code{\link{Node}}
#'
#' @keywords internal
Aggregate = function(attribute, fun, ...) {
  stop("This method can only be called on a Node!")
}



#' Sort Children of a Node or an Entire Tree
#' 
#' You can sort with respect to any argument of the tree.
#' 
#' @param attribute a field, method or function. The result of the attribute determines the 
#' sorting. If it is a function, #' the attribute must take a \code{Node} as a first argument.
#' @param ... any parameters to be passed on the the attribute (in case it's a method or a 
#' function)
#' @param decreasing sort order
#' @param recursive if \code{TRUE}, Sort will be called recursively on the \code{Node}'s children. 
#' This allows sorting an entire tree.
#' 
#' @examples
#' data(acme)
#' acme$Get("Aggregate", "cost", sum, assign = "totalCost")
#' acme$Sort("totalCost", decreasing = TRUE)
#' print(acme, "totalCost")
#' 
#' @seealso \code{\link{Node}}
#' @keywords internal
Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
  stop("This method can only be called on a Node!")
}



#' Convert a \code{\link{Node}} to a \code{data.frame}
#' 
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. 
#' Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names 
#' (to syntactic names: see make.names) is optional.
#' @param ... the attributes to be added as columns of the data.frame. There are various
#' options:
#' \itemize{
#'  \item a string corresponding to the name of a node attribute
#'  \item the result of the \code{Node$Get} method
#' }
#' If a specific Node does not contain the attribute, the data.frame will contain NA.
#'
#' @seealso \code{\link{Node}}, \code{\link{as.data.frame.Node}} 
#' @keywords internal
ToDataFrame <- function(row.names = NULL, optional = FALSE, ..., filterFun = function(x) TRUE) {
  stop("This method can only be called on a Node!")
}


#' Convert a \code{\link{Node}} object to a nested \code{list}
#' 
#' @param x The Node to convert
#' @param unname If TRUE, then the nested children list will not have named arguments. This
#' can be useful e.g. in the context of conversion to JSON, if you prefer the children to be
#' an array rather than named objects.
#' @param nameName The name that should be given to the name element
#' @param childrenName The name that should be given to the children nested list
#' @param ... Additional parameters
#' 
#' @seealso \code{\link{Node}}, \code{\link{as.list.Node}} 
#' @keywords internal
#' 
ToList <- function(unname = FALSE, 
                   nameName = 'name', 
                   childrenName = 'children', ...) {
  stop("This method can only be called on a Node!")
}




#' Prunes a tree by applying the filterFun to each node. 
#' 
#' You can apply this to any \code{\link{Node}}
#' in a tree; the filterFun is applied to the children of the \code{\link{Node}} only. Note that as
#' soon as the filter criteria is not matched anymore, the algorithm stops, and neither the \code{\link{Node}}
#' nor its children are kept, regardless of whether the child would satisfy the filter.
#' 
#' @param traversal one of the traversal modes (pre-order, post-order)
#' @param filterFun A function taking a \code{\link{Node}} as an argument, an returning \code{TRUE}
#' if the \code{\link{Node}} should be kept, false otherwise.
#' @keywords internal
Prune <- function(traversal = "pre-order", filterFun) {
  stop("This method can only be called on a Node!")
}

#' Converts the tree to tabular form, keeping only the leafs.
#' 
#' This method is especially useful when you need to apply a specific function that is only available 
#' for \code{\link{data.frame}}. 
#' 
#' @param ... character strings representing the attributes to be returned. Note that these may not be 
#' available on a leaf, in which case the algorithm inherits the values from its ancestors.
#' 
#' @keywords internal
ToDataFrameTable <- function(...) {
  stop("This method can only be called on a Node!")
}