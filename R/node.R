#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' Attribute names.
#' 
NODE_RESERVED_NAMES_CONST <- c( 'AddChild',
                                'AddChildNode',
                                'AddSibling',
                                'AddSiblingNode',
                                'children',
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
#'   \item{\code{\link{Get}(attribute, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL, format = NULL, inheritFromAncestors = FALSE)}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Do}(fun, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NUL)}}{Traverses the tree and call fun on each node.}
#'   \item{\code{\link{Set}(..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL)}}{Traverses the tree and assigns the args along the way, recycling the args.}
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE}}{Sort children of a node with respect to an attribute (field, method, active, function)}
#'   \item{\code{\link{Revert}(recursive = TRUE)}}{Revert the sort order of a node}
#'   \item{\code{\link{Prune}(pruneFun)}}{Prune a tree. The pruneFun takes a node as its first argument, and returns TRUE if the node should be kept, FALSE otherwise}
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
#'  \item{\code{depth}}{Returns 1 + max(nr of edges between a \code{Node} and any of its descendants)}
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
                      
                      
                      ########################
                      ## Side Effects
                      
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        Sort(self, attribute, ..., decreasing = decreasing, recursive = recursive)  
                      },
                      
                      Revert = function(recursive = TRUE) {
                        Revert(self, recursive)
                      },
                      
                      Prune = function(pruneFun) {
                        Prune(self, pruneFun = pruneFun)
                      },
                      
                      
                      # End Side Effects
                      ###########################
                      
                      
                      
                      Find = function(...) {
                        Find(self, ...)
                      },
                      
                      
                      ##########################
                      # Traversal
                      
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
                                
                      # End Traversal
                      #######################
                                          
                      GetAttribute = function(attribute, ..., format = NULL, inheritFromAncestors = FALSE, nullAsNa = TRUE) {
                        GetAttribute(self, 
                                     attribute, 
                                     ..., 
                                     format = format, 
                                     inheritFromAncestors = inheritFromAncestors, 
                                     nullAsNa = nullAsNa)
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

