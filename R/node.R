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
                                'Do',
                                'fields',
                                'fieldsAll',
                                'Find',
                                'formatters',
                                'Get',
                                'GetAttribute',
                                'initialize',
                                'isBinary',
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
                                'Revert',
                                'root',
                                'Set',
                                'SetAttribute',
                                'Sort',
                                'tmp',
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
#'   \item{\code{Prune(pruneFun, traversal = "pre-order")}}{ Remove \code{Node}s in the tree based on the return value of \code{pruneFun} }
#'   \item{\code{\link{Get}(attribute, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL, format = NULL, inheritFromAncestors = FALSE)}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Set}(..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL)}}{Traverses the tree and assigns the args along the way, recycling the args.}
#'   \item{\code{\link{Aggregate}(attribute, fun, ...)}}{Traverses the tree and calls \code{fun(children$Aggregate(...))} on each node. }
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE)}}{Sorts the children of a node according to \code{attribute}}
#'   \item{\code{\link{Revert}(recursive = TRUE)}}{Reverts the order of the children of a node}
#'   \item{\code{Clone()}}{Creates a deep copy of a \code{Node} and all its sub-nodes}
#'   \item{\code{\link{ToDataFrame}(..., pruneFun = NULL, filterFun = NULL, inheritFromAncestors)}}{Converts the tree below this \code{Node} to a \code{data.frame}}
#'   \item{\code{ToDataFrameTable}(..., pruneFun = NULL, filterFun = NULL)}{Converts the tree below this \code{Node} to standard tabular format, i.e. a \code{data.frame}, inheriting from ancestors and only putting one line per leaf.}
#'   \item{\code{\link{ToList}(mode = c("simple", "explicit"), unname = FALSE, nameName = ifelse(unname, 'name', ''), childrenName = 'children', nodeName = NULL, ...)}}{Converts the tree below this \code{Node} to a \code{list}}
#'   \item{\code{\link{ToNewick}(heightAttributeName = "height", ...)}}{Converts the tree to Newick notation. }
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
                      #formatters = list(),
                      formatters = NULL,
                      parent = NULL,
                      
                      initialize=function(name, ...) {
                        if (!missing(name)) self$name <- as.character(name)
                        self$formatters = new.env(hash = FALSE, parent = self, size = 10)
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
                                     traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),  
                                     pruneFun = NULL,
                                     filterFun = NULL, 
                                     format = NULL,
                                     inheritFromAncestors = FALSE) {
                        
                        t <- Traverse(self, 
                                      traversal = traversal, 
                                      pruneFun = pruneFun,
                                      filterFun = filterFun)
                        Get(t, 
                            attribute, 
                            ...,  
                            format = format, 
                            inheritFromAncestors = inheritFromAncestors)

                      },
                      
                      
                      Do = function( fun, 
                                     ..., 
                                     traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),  
                                     pruneFun = NULL,
                                     filterFun = NULL 
                                     ) {
                        
                        t <- Traverse(self, 
                                      traversal = traversal, 
                                      pruneFun = pruneFun,
                                      filterFun = filterFun)
                        Do( t, fun, ...)
                        
                      },
                      
                      Set = function(..., 
                                     traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),  
                                     pruneFun = NULL,
                                     filterFun = NULL) {
                        t <- Traverse(self, 
                                      traversal = traversal, 
                                      pruneFun = pruneFun,
                                      filterFun = filterFun)
                        Set(t, ...)
                        invisible (self)
                      },
                      
                      Aggregate = function(attribute, fun, ...) {
                        Aggregate(self, attribute, fun, ...)
                      },
                      
                      
                      Prune = function(pruneFun, traversal = "pre-order") {
                        if ( self$isLeaf) return()
                        if ( traversal == "pre-order") {
                          for( i in length(self$children):1 ) {
                            if ( !pruneFun(self$children[[i]]) ) {
                              self$children <- self$children[-i]
                            }
                          }
                          for( child in self$children) {
                            child$Prune(pruneFun)
                          }
                        } else if( traversal == "post-order") {
                          for( child in self$children) {
                            child$Prune(pruneFun)
                          }
                          for( i in length(self$children):1 ) {
                            if ( !pruneFun(self$children[[i]]) ) {
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
                      
                      Revert = function(recursive = TRUE) {
                        self$Set(tmp = 1:self$totalCount)
                        self$Sort("tmp", decreasing = TRUE, recursive = recursive)
                      },
                      
                                          
                      GetAttribute = function(attribute, ..., format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
                        GetAttribute(self, 
                                     attribute, 
                                     ..., 
                                     format = format, 
                                     inheritFromAncestors = inheritFromAncestors, 
                                     nullAsNa = nullAsNa)
                      },
                      
                      Clone = function() {
                        l <- as.list(self, mode = "explicit", rootName = self$name)
                        res <- as.Node(l, mode = "explicit")
                        #formatters need to be set manually
                        for(name in names(self$formatters)) {
                          res$formatters[[name]] <- self$formatters[[name]]
                        }
                        return (res)
                      },
                      
                      ToDataFrame = function(..., pruneFun = NULL, filterFun = NULL, inheritFromAncestors = FALSE) {
                        as.data.frame(self, row.names = NULL, optional = FALSE, ..., pruneFun = pruneFun, filterFun = filterFun, inheritFromAncestors = inheritFromAncestors)
                      }, 
                      
                      
                      ToDataFrameTable = function(..., pruneFun = NULL, filterFun = NULL) {
                        ifilterFun <- function(x) {
                          x$isLeaf && (length(filterFun) == 0 || filterFun(x))  
                        }
                        df <- as.data.frame(self, row.names = NULL, optional = FALSE, ..., pruneFun = pruneFun, filterFun = ifilterFun, inheritFromAncestors = TRUE)
                        df <- df[,-1]
                        return (df)
                      },
                      
                      ToList = function(mode = c("simple", "explicit"),
                                        unname = FALSE,
                                        nameName = ifelse(unname, "name", ""),
                                        childrenName = 'children',
                                        ...) {
                        as.list(self, 
                                     mode = mode,
                                     unname = unname, 
                                     nameName = nameName, 
                                     childrenName = childrenName,
                                     ...)
                      },
                      
                      
                      ToNewick = function(heightAttribute = Height, ...) {
                        ToNewick(self, heightAttribute, ...)
                      }
                      
                                            
                    ),
                
                
                    active = list(
                      
                      name = function(value) {
                        if (missing(value)) return (private$p_name)
                        else {
                          private$p_name <- value
                          if(!self$isRoot) {
                            chldrn <- self$parent$children
                            nms <- names(chldrn)
                            i <- which(sapply(chldrn, function(x) identical(x, self)))
                            nms[i] <- value
                            names(chldrn) <- nms
                            self$parent$children <- chldrn                    
                          }
                        }
                      },
                      
                      isLeaf = function() {
                        isLeaf(self) 
                      },
                      
                      isRoot = function() {
                        isRoot(self)
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
                        as.vector(na.omit(unique(unlist(self$Get("fields")))))
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
                          return (1)
                        } else {
                          return (1 + self$parent$level)
                        }
                      },
                      
                      depth = function() {
                        max(self$Get("level")) - self$level + 1
                      },
                      
                      isBinary = function() {
                        all(2 == self$Get("count", filterFun = function(x) !x$isLeaf))
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

