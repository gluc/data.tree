#' Create Trees With \code{Node}s
#' 
#' @description \code{Node} is at the very heart of the \code{data.tree} package. All trees are constructed
#' by tying toghether \code{Node} objects.
#' @docType class
#' @examples
#' library(data.tree)
#' acme <- Node$new("Acme Inc.")
#' accounting <- acme$AddChild("Accounting")
#' print(acme)
#' @seealso For more details see the \code{data.tree} vignette: \code{vignette("data.tree")}
#' @importFrom R6 R6Class
#' @field children A list of children \code{Node}s
#' @field parent The node's parent \code{Node}
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Node$new(name)}}{Creates a new \code{Node} called \code{name}. Often used to construct the root.}
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node}.}
#'   \item{\code{\link{Find}(...)}}{Find a node with path \code{...}, where the \code{...} arguments are the \code{name}s of the \code{Node}s }
#'   \item{\code{\link{Get}(attribute, ..., traversal = "pre-order", filterFun = function(x) TRUE, assign = NULL, format = NULL)}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Set}(..., traversal = "pre-order", returnValues = FALSE)}}{Traverses the tree and assigns attributes along the way.}
#'   \item{\code{\link{Aggregate}(attribute, fun, ...)}}{Traverses the tree and calls \code{fun(children$Aggregate(...))} on each node. }
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE)}}{Sorts the children of a node according to \code{attribute}}
#'   \item{\code{\link{ToDataFrame}(row.names = NULL, optional = FALSE, ...)}}{Converts the tree below this \code{Node} to a \code{data.frame}}
#' }
#' 
#' @section Properties:
#'   
#' \describe{
#'  \item{\code{children}}{Returns a list containing all the children of this \code{Node}}
#'  \item{\code{parent}}{Returns the parent \code{Node} of this \code{Node}}
#'  \item{\code{name}}{Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}}
#'  \item{\code{isLeaf}}{Returns \code{TRUE} if the \code{Node} is a leaf, \code{FALSE} otherwise}
#'  \item{\code{isRoot}}{Returns \code{TRUE} if the \code{Node} is the root, \code{FALSE} otherwise}
#'  \item{\code{count}}{Returns the number of children of a \code{Node}}
#'  \item{\code{totalCount}}{Returns the total number of \code{Node}s in the tree}
#'  \item{\code{path}}{Returns a vector of mode \code{character} containing the names of the \code{Node}s in the path from the root to this \code{Node}}
#'  \item{\code{pathString}}{Returns a string representing the path to this \code{Node}, separated by backslash}
#'  \item{\code{levelName}}{Returns the name of the \code{Node}, preceded by level times '*'. Useful for printing.}
#'  \item{\code{leaves}}{Returns a list containing all the leaf \code{Node}s }
#'  \item{\code{level}}{Returns an integer representing the level of a \code{Node}. For example, the root has level 0.}
#'  \item{\code{root}}{Returns the root \code{Node} of a \code{Node}'s tree}
#'  
#' }
#' 
#' @export
#' @format An \code{\link{R6Class}} generator object
Node <- R6Class("Node",
                lock = FALSE,
                    public = list(
                      children = list(),
                      parent = NULL,
                      
                      initialize=function(name, ...) {
                        if (!missing(name)) self$name <- name
                        invisible (self)
                      },
                      
                      
                      AddChild = function(name) {
                        child <- Node$new(as.character(name))
                        invisible (self$AddChildNode(child))
                      },
                      
                      AddChildNode = function(child) {
                        self$children[[child$name]] <- child
                        child$parent <- self
                        invisible (child)
                      },
                      
                      
                      AddSibling = function(name) {
                        sibling <- Node$new(name)
                        invisible (self$AddSiblingNode(sibling))
                      },
                      
                      AddSiblingNode = function(sibling) {
                        if (self$isRoot) stop("Cannot add sibling to root!")
                        self$parent$AddChildNode(sibling)
                        invisible (sibling)
                      },
                      
                      
                      Find = function(...) {
                        path <- as.character(list(...))
                        if (length(path) == 0) {
                          return (self)
                        } else {
                          child <- self$children[[path[1]]]
                          if (is.null(child)) {
                            return (NULL)
                          } else if (length(path) == 1) {
                            return (child)
                          } else {
                            return (do.call(child$Find, list(...)[-1]))
                            #return (child$Find( path[ length(path) - ( ( length(path) - 2 ) : 0 ) ] ) )
                          }
                        }
                      },
                      
                      
                      Get = function(attribute, 
                                     ..., 
                                     traversal = "pre-order", 
                                     filterFun = function(x) TRUE, 
                                     assign = NULL, 
                                     format = NULL) {
                        #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
                        
                        if (!filterFun(self)) return (NULL)
                        
                        if(traversal == "pre-order") {
                          # nice for printing. e.g. L1, L1.1 , L1.1.1, L1.1.2, L1.2, etc.
                          v <- self$GetAttribute(attribute, ..., assign = assign, format = format)
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              if (filterFun(child)) {
                                v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format))
                              }
                            }
                          }
                          
                        
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              if (filterFun(child)) {
                                childValues <- c(childValues, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format))
                              }
                            }
                          }
                          v <- self$GetAttribute(attribute, ..., assign = assign, format = format)
                          v <- c(childValues, v)
                          
                        } else if (traversal == "ancestor") {
                          v <- self$GetAttribute(attribute, ..., format = format)
                          if (!self$isRoot) {
                            if (filterFun(self$parent)) {
                              parentV <- self$parent$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format)
                              v <- c(v, parentV)
                            }
                          }
                        }
                        return (v)
                      },
                      
                      Set = function(..., traversal = "pre-order", returnValues = FALSE) {
                        args <- list(...)
                        argsnames <- sapply(substitute(list(...))[-1], deparse)
                        gargsnames <- names(args)
                        if (is.null(gargsnames)) gargsnames <- vector(mode = "character", length = length(args))
                        gargsnames[nchar(gargsnames) == 0] <- argsnames[nchar(gargsnames) == 0]
                        names(args) <- gargsnames
                        
                        if(traversal == "pre-order") {
                          
                          #for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          args <- Map(function(name, arg) self$SetAttribute(name, arg), names(args), args)
                          
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              
                              args <- do.call(child$Set, c(args, traversal = traversal, returnValues = TRUE))
                            }
                          }
                          
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                          childValues <- vector()
                          if(!self$isLeaf) {
                            for(child in self$children) {
                              args <- do.call(child$Set, c(args, traversal = traversal, returnValues = TRUE))
                            }
                          }
                          for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          
                          
                        } else if (traversal == "ancestor") {
                          for (i in 1:length(args)) args[[i]] <- self$SetAttribute(names(args)[[i]], args[[i]])
                          if (!self$isRoot) {
                            args <- do.call(self$parent$Set, c(args, traversal = traversal, returnValues = TRUE))
                          }
                        }
                        if (returnValues) invisible (args)
                        else invisible (self)
                      },
                      
                      Aggregate = function(attribute, fun, ...) {
                        v <- self[[attribute]]
                        if (!is.null(v)) {
                          if (is.function(v)) v <- v(...)
                          return (v)
                        }
                        if (self$isLeaf) stop(paste0("Cannot find attribute ", attribute, "!"))
                        values <- sapply(self$children, function(x) x$Aggregate(attribute, fun, ...))
                        result <- fun(values)
                        return (result)
                      },
                                            
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        if (self$isLeaf) return()
                        ChildL <- sapply(self$children, function(x) x$GetAttribute(attribute, ...))
                        names(ChildL) <- names(self$children)
                        self$children <- self$children[names(sort(ChildL, decreasing = decreasing, na.last = TRUE))]
                        if (recursive) for(child in self$children) child$Sort(attribute, ..., decreasing = decreasing, recursive = recursive)
                      },
                      
                      
                      SetAttribute = function(attribute, values) {
                        
                        if (length(values) == 1) {
                          self[[attribute]] <- values
                          return (values)
                        } else if (length(values) > 1) {
                          self[[attribute]] <- values[1]
                          return (values[-1])
                        } else if(is.null(values)) {
                          self[[attribute]] <- NULL
                          return (NULL)
                        } else {
                          stop("length of values must be 1 or equal to the number of nodes")
                        }
                      },
                      
                      
                                          
                      GetAttribute = function(attribute, ..., assign = NULL, format = NULL) {
                        if(is.function(attribute)) {
                          #function
                          v <- attribute(self, ...)
                        } else if(is.character(attribute) && length(attribute) == 1) {
                          #property
                          v <- self[[attribute]]
                          if (is.function(v)) v <- v(...)
                        } else {
                          stop("attribute must be a function, the name of a public property, or the name of method")
                        }
                        
                        if (is.null(v)) v <- NA
                        if (length(v) == 0) v <- NA
                        if(!is.null(assign)) self[[assign]] <- v
                        names(v) <- self$name
                        
                        if(!is.null(format)) {
                          if (!is.function(format)) stop("form must be a function!")
                          v <- format(v)
                        }
                        return (v)
                      },
                      
                      #Copy = function() {
                        #return a deep copy of the Node and all its children (but not the parent)
                        #selfCopy <- as.environment(as.list(self, all.names = TRUE))
                        #privateCopy <- as.environment(as.list(private, all.names = TRUE))
                      #},
                      
                      #Filter = function(){}
                      
                      ToDataFrame = function(..., filterFun = function(x) TRUE) {
                        as.data.frame(self, row.names = NULL, optional = FALSE, ..., filterFun = filterFun)
                      }
                      
                                            
                    ),
                
                
                    active = list(
                      
                      name = function(value) {
                        if (missing(value)) return (self$p_name)
                        else self$p_name <- value
                      },
                      
                      isLeaf = function() {
                        return (length(self$children) == 0) 
                      },
                      
                      isRoot = function() {
                        return (is.null(self$parent))
                      },
                      
                      count = function() {
                        return (length(self$children))
                      },
                      
                      totalCount = function() {
                        return (1 + sum(as.numeric(sapply(self$children, function(x) x$totalCount))))
                      }, 
                      
                      path = function() {
                        c(self$parent$path, self$name)
                      }, 
                      
                      pathString = function() {
                        paste(self$path, collapse="/")
                      },
                      
                      position = function() {
                        if (self$isRoot) return (0)
                        match(self$name, names(self$parent$children))
                      },
                      
                      levelName = function() {
                        paste0(self$.separator, self$name)
                      },
                      
                      
                      .separator = function() {
                        if (self$isRoot) return("")
                        if (self$position == self$parent$count) mySeparator <- paste0(" ", "\u00B0", "--") 
                        else mySeparator <- paste0(" ", "\u00A6", "--")
                        return (paste0(self$parent$.parentSeparator, mySeparator))
                      },
                      
                      .parentSeparator = function() {
                        if (self$isRoot) return("")
                        if (self$position == self$parent$count) mySeparator <- "    "
                        else mySeparator <- paste0(" ", "\u00A6", "  ")
                        paste0(self$parent$.parentSeparator, mySeparator)
                        
                      },
                      
                      leaves = function() {
                        if (self$isLeaf) {
                          return (self)
                        } else {
                          l <- unlist(sapply(self$children, function(x) x$leaves))
                        }
                      },
                      
                      level = function() {
                        if (self$isRoot) {
                          return (0)
                        } else {
                          return (1 + self$parent$level)
                        }
                      },
                      
                      root = function() {
                        if (self$isRoot) {
                          invisible (self)
                        } else {
                          invisible (self$parent$root)
                        }
                      }
                      
                      
                      
                      
                    ),
                
                    private = list(
                      
                      p_name = ""
                      
                      
                    )
                  )



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


#' @export
print.Node <- function(x, ...) {
  print(as.data.frame(x, row.names = NULL, optional = FALSE, ...))
}

#' Convert a \code{\link{Node}} to a \code{data.frame}
#' 
#' @param x The root node to convert to a data.frame
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
#' If a specific Node does not contain the attribute, \code{NA} is added to the data.frame.
#' @param filterFun a function which filters the Nodes added to the \code{data.frame}. The function must
#' take a \code{Node} as an input, and it must return \code{TRUE} or \code{FALSE}, depending on whether the
#' \code{Node} and its subtree should be displayed.
#' 
#' @export
as.data.frame.Node <- function(x, row.names = NULL, optional = FALSE, ..., filterFun = function(x) TRUE) {
  if(is.null(row.names)) {
    if(optional) {
      row.names <- rep("", x$totalCount)
    } else {
      row.name <- 1:x$totalCount
    }
  }
  
  df <- data.frame( levelName = format(x$Get('levelName', filterFun = filterFun)),
                    row.names = row.names,
                    stringsAsFactors = FALSE)
  
  cols <- list(...)
  
  if(length(cols) == 0) return (df)
  for (i in 1:length(cols)) {
    col <- cols[[i]]
    if (is.character(col) && length(col) == 1) {
      it <- x$Get(col, filterFun = filterFun)
      colName <- col
    } else {
      it <- col
      colName <- names(cols)[i]
    }
    
    df[colName] <- it
    
  }
  return (df)
  
}


