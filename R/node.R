#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' Attribute names.
#' 
NODE_RESERVED_NAMES_CONST <- c( 'AddChild',
                                'AddChildNode',
                                'AddSibling',
                                'AddSiblingNode',
                                'Aggregate',
                                'children',
                                'Clone',
                                'count',
                                'depth',
                                'fields',
                                'fieldsAll',
                                'Find',
                                'Get',
                                'GetAttribute',
                                'Height',
                                'initialize',
                                'isLeaf',
                                'isRoot',
                                'leafCount',
                                'leaves',
                                'level',
                                'levelName',
                                'name',
                                'parent',
                                'path',
                                'pathString',
                                'position', 
                                'Prune',
                                'root',
                                'Set',
                                'SetAttribute',
                                'Sort',
                                'ToDataFrame',
                                'ToDataFrameTable',
                                'ToList',
                                'ToNewick',
                                'totalCount')


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
#'   \item{\code{Prune(filterFun, traversal = "pre-order")}}{ Remove \code{Node}s in the tree based on the return value of \code{filterFun} }
#'   \item{\code{\link{Get}(attribute, ..., traversal = "pre-order", filterFun = NULL, assign = NULL, format = NULL)}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Set}(..., traversal = "pre-order", returnValues = FALSE)}}{Traverses the tree and assigns attributes along the way.}
#'   \item{\code{\link{Aggregate}(attribute, fun, ...)}}{Traverses the tree and calls \code{fun(children$Aggregate(...))} on each node. }
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE)}}{Sorts the children of a node according to \code{attribute}}
#'   \item{\code{Clone()}}{Creates a deep copy of a \code{Node} and all its sub-nodes}
#'   \item{\code{\link{ToDataFrame}(..., filterFun = function(x) TRUE, inheritFromAncestors)}}{Converts the tree below this \code{Node} to a \code{data.frame}}
#'   \item{\code{Height(rootHeight = 100)}}{Calculates the height of a \code{Node} given the hight of the root, assuming that nodes are equally distributed. Useful for easy printing.}
#'   \item{\code{\link{ToList}(unname = FALSE, nameName = ifelse(unname, 'name', ''), childrenName = 'children', nodeName = NULL, ...)}}{Converts the tree below this \code{Node} to a \code{list}}
#'   \item{\code{\link{ToNewick}(heightAttributeName = "Height", ...)}}{Converts the tree to Newick notation. }

#'
#' }
#' 
#' @section Properties:
#'   
#' \describe{
#'  \item{\code{children}}{Returns a list containing all the children of this \code{Node}}
#'  \item{\code{parent}}{Returns the parent \code{Node} of this \code{Node}}
#'  \item{\code{name}}{Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}}
#'  \item{\code{fields}}{Gets the names of the set properties of a \code{Node}}
#'  \item{\code{fieldsAll}}{Gets the names of the set properties of a \code{Node} or any of its sub-Nodes}
#'  \item{\code{isLeaf}}{Returns \code{TRUE} if the \code{Node} is a leaf, \code{FALSE} otherwise}
#'  \item{\code{isRoot}}{Returns \code{TRUE} if the \code{Node} is the root, \code{FALSE} otherwise}
#'  \item{\code{count}}{Returns the number of children of a \code{Node}}
#'  \item{\code{totalCount}}{Returns the total number of \code{Node}s in the tree}
#'  \item{\code{path}}{Returns a vector of mode \code{character} containing the names of the \code{Node}s in the path from the root to this \code{Node}}
#'  \item{\code{pathString}}{Returns a string representing the path to this \code{Node}, separated by backslash}
#'  \item{\code{levelName}}{Returns the name of the \code{Node}, preceded by level times '*'. Useful for printing.}
#'  \item{\code{leafCount}}{Returns the number of leaves are below a \code{Node} }
#'  \item{\code{leaves}}{Returns a list containing all the leaf \code{Node}s }
#'  \item{\code{level}}{Returns an integer representing the level of a \code{Node}. For example, the root has level 0.}
#'  \item{\code{depth}}{Returns 1 plus the maximum number of edges between a \code{Node} and any of its descendants}
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
                                     filterFun = NULL, 
                                     assign = NULL, 
                                     format = NULL,
                                     inheritFromAncestors = FALSE) {
                        #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
                        v <- vector()
                        if(traversal == "pre-order") {
                          
                          if(is.null(filterFun) || filterFun(self)) {
                            
                            for(child in self$children) {
                              v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
                            }
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(me, v)
                          }
                          
                        } else if (traversal == "post-order") {
                          # useful if leafs need to be calculated first
                         
                          if(is.null(filterFun) || filterFun(self)) {
                            for(child in self$children) {
                              v <- c(v, child$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors))
                            }
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(v, me)
                          }
                          
                        } else if (traversal == "ancestor") {
                          
                          
                          if (!self$isRoot) {
                            v <- self$parent$Get(attribute, ..., traversal = traversal, filterFun = filterFun, assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                          }
                          if(is.null(filterFun) || filterFun(self)) {
                            me <- self$GetAttribute(attribute, ..., assign = assign, format = format, inheritFromAncestors = inheritFromAncestors)
                            v <- c(me, v)
                            
                          }
                        }
                        if (is.null(assign)) return (v)
                        invisible (v)
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
                        #if(is.function(attribute)) browser()
                        v <- self$GetAttribute(attribute, ..., nullAsNa = FALSE)
                        if (!is.null(v)) {
                          return (v)
                        }
                        if (self$isLeaf) stop(paste0("Cannot find attribute ", attribute, "!"))
                        values <- sapply(self$children, function(x) x$Aggregate(attribute, fun, ...))
                        result <- fun(values)
                        return (result)
                      },
                      
                      
                      Prune = function(filterFun, traversal = "pre-order") {
                        if ( self$isLeaf) return()
                        if ( traversal == "pre-order") {
                          for( i in length(self$children):1 ) {
                            if ( !filterFun(self$children[[i]]) ) {
                              self$children <- self$children[-i]
                            }
                          }
                          for( child in self$children) {
                            child$Prune(filterFun)
                          }
                        } else if( traversal == "post-order") {
                          for( child in self$children) {
                            child$Prune(filterFun)
                          }
                          for( i in length(self$children):1 ) {
                            if ( !filterFun(self$children[[i]]) ) {
                              self$children <- self$children[-i]
                            }
                          }
                        }
                      },
                                            
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        if (self$isLeaf) return()
                        ChildL <- sapply(self$children, function(x) x$GetAttribute(attribute, ...))
                        names(ChildL) <- names(self$children)
                        self$children <- self$children[names(sort(ChildL, decreasing = decreasing, na.last = TRUE))]
                        if (recursive) for(child in self$children) child$Sort(attribute, ..., decreasing = decreasing, recursive = recursive)
                        invisible(self)
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
                      
                      
                                          
                      GetAttribute = function(attribute, ..., assign = NULL, format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
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
                        
                        if(is.null(v) && inheritFromAncestors && !self$isRoot) {
                          v <- self$parent$GetAttribute(attribute, 
                                                        ..., 
                                                        assign = assign, 
                                                        format = format, 
                                                        inheritFromAncestors = inheritFromAncestors)
                        }
                        
                        if (!nullAsNa && is.null(v)) return (NULL)
                        if (is.null(v)) v <- NA
                        if (length(v) == 0) v <- NA
                        
                        if(!is.null(assign)) self[[assign]] <- v
                        names(v) <- self$name
                        
                        if(!is.null(format)) {
                          if (!is.function(format)) stop("format must be a function!")
                          v <- format(v)
                        }
                        return (v)
                      },
                      
                      Clone = function() {
                        return (as.Node(self$ToList()))
                      },
                      
                      #Filter = function(){}
                      
                      ToDataFrame = function(..., filterFun = NULL, inheritFromAncestors = FALSE) {
                        as.data.frame(self, row.names = NULL, optional = FALSE, ..., filterFun = filterFun, inheritFromAncestors = inheritFromAncestors)
                      }, 
                      
                      
                      ToDataFrameTable = function(...) {
                        df <- as.data.frame(self, row.names = NULL, optional = FALSE, ..., filterFun = NULL, inheritFromAncestors = TRUE)
                        df <- df[self$Get("isLeaf"),-1]
                        row.names(df) <- 1:nrow(df)
                        return (df)
                      },
                      
                      ToList = function(unname = FALSE, 
                                        nameName = ifelse(unname, "name", ""),
                                        childrenName = 'children',
                                        nodeName = NULL,
                                        ...) {
                        as.list(self, unname, nameName, childrenName, ...)
                      },
                      
                      Height = function(rootHeight = 100) {
                        if (self$isRoot) return ( rootHeight )
                        if (self$isLeaf) return ( 0 )
                        
                        h <- self$parent$Height(rootHeight) * (1 - 1 / self$depth)
                        return (h)
                      }, 
                      
                      ToNewick = function(heightAttributeName = "Height", ...) {
                        ToNewick(self, heightAttributeName, ...)
                      }
                      
                                            
                    ),
                
                
                    active = list(
                      
                      name = function(value) {
                        if (missing(value)) return (private$p_name)
                        else private$p_name <- value
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
                        
                        result <- which(unname(sapply(self$parent$children, function(x) identical(self, x))))
                        # match(self$name, names(self$parent$children))
                        return (result)
                      },
                      
                      levelName = function() {
                        paste0(self$.separator, self$name)
                      },
                      
                      
                      fields = function() {
                        ls(self)[!(ls(self) %in% NODE_RESERVED_NAMES_CONST)]
                      },
                      
                      fieldsAll = function() {
                        as.vector(na.omit(unique(self$Get("fields"))))
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
                          unlist(sapply(self$children, function(x) x$leaves))
                        }
                      },
                      
                      leafCount = function() {
                        sum(self$Get("isLeaf"))
                      },
                      
                      level = function() {
                        if (self$isRoot) {
                          return (0)
                        } else {
                          return (1 + self$parent$level)
                        }
                      },
                      
                      depth = function() {
                        max(self$Get("level")) - self$level + 1
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

