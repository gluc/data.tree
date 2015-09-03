#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' attribute names.
#' Note also that all fields starting with a . are reserved.
#' 
#' @export
NODE_RESERVED_NAMES_CONST <- c( 'AddChild',
                                'AddChildNode',
                                'AddSibling',
                                'AddSiblingNode',
                                'averageBranchingFactor',
                                'children',
                                'Climb',
                                'clone',
                                'count',
                                'Do',
                                'fields',
                                'fieldsAll',
                                'Get',
                                'GetAttribute',
                                'height',
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
                                'RemoveAttribute',
                                'RemoveChild',
                                'root',
                                'Set',
                                'Sort',
                                'totalCount',
                                '.*')


#' Create a \code{data.tree} Structure With \code{Nodes}
#' 
#' @description \code{Node} is at the very heart of the \code{data.tree} package. All trees are constructed
#' by tying toghether \code{Node} objects.
#' @docType class
#' @importFrom R6 R6Class
#' @field children A list of child \code{Nodes}
#' @field parent The node's parent \code{Node}
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Node$new(name)}}{Creates a new \code{Node} called \code{name}. Often used to construct the root when creating trees programmatically.}
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node} as a child.}
#'   \item{\code{RemoveChild(name)}}{Remove the child \code{Node} called \code{name} from a \code{Node} and returns it.}
#'   \item{\code{RemoveAttribute(name)}}{Removes attribute called \code{name} from this \code{Node}.}
#'   \item{\code{\link{Climb}(...)}}{Find a node with path \code{...}, where the \code{...} arguments are the \code{name}s of the \code{Node}s, or other field values.}
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
#'  \item{\code{height}}{Returns max(level) of any of the \code{Nodes} of the tree}
#'  \item{\code{averageBranchingFactor}}{Returns the average number of crotches below this \code{Node}}
#'  \item{\code{root}}{Returns the root \code{Node} of a \code{Node}'s tree}
#'  
#' }
#'
#' @examples
#' library(data.tree)
#' acme <- Node$new("Acme Inc.")
#' accounting <- acme$AddChild("Accounting")
#' print(acme)
#' 
#' @seealso For more details see the \code{\link{data.tree}} documentations, or the \code{data.tree} vignette: \code{vignette("data.tree")}
#'
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
Node <- R6Class("Node",
                lock_objects = FALSE,
                    public = list(
                      parent = NULL,
                      children = NULL,
                      
                      initialize=function(name, ...) {
                        if (!missing(name)) private$p_name <- as.character(name)
                        args <- list(...)
                        mapply(FUN = function(arg, nme) self[[nme]] <- arg, args, names(args))
                        invisible (self)
                      },
                      
                      
                      
                      ####################
                      # Tree creation
                      
                      AddChild = function(name, ...) {
                        child <- Node$new(as.character(name), ...)
                        invisible (self$AddChildNode(child))
                      },
                      
                      AddChildNode = function(child) {
                        self$children[[child$name]] <- child
                        self[[child$name]] <- child
                        child$parent <- self
                        invisible (child)
                      },
                      
                      
                      RemoveChild = function(name) {
                        if (!name %in% names(self$children)) stop(paste0("Node ", self$name, " does not contain child ", name))
                        child <- self$children[[name]]
                        self$RemoveAttribute(name)
                        self$children <- self$children[-child$position]
                        child$parent <- NULL
                        return (child)
                      },
                      
                      RemoveAttribute = function(name) {
                        if (!name %in% ls(self)) stop(paste0("Node ", self$name, " does not contain field ", name))
                        rm(list = name, envir = self)
                      },
                      
                      
                      # End Tree Creation
                      ########################
                      
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
                      
                      
                      
                      Climb = function(...) {
                        Climb(self, ...)
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
                        Do(t, fun, ...)
                        
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
                      }
                                
                      # End Traversal
                      #######################
                                          
                      
                    ),
                
                
                    active = list(
                      
                      name = function(value) {
                        if (missing(value)) return (private$p_name)
                        else private$p_name <- changeName(self, private$p_name, value)
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
                        return (1 + sum(as.numeric(sapply(self$children, function(x) x$totalCount, simplify = TRUE, USE.NAMES = FALSE))))
                      }, 
                      
                      path = function() {
                        c(self$parent$path, self$name)
                      }, 
                      
                      pathString = function() {
                        paste(self$path, collapse="/")
                      },
                      
                      position = function() {
                        if (self$isRoot) return (1)
                        
                        result <- which(names(self$parent$children) == self$name)
                        # match(self$name, names(self$parent$children))
                        return (result)
                      },
                                            
                      fields = function() {
                        nms <- ls(self)
                        nms <- nms[!(nms %in% NODE_RESERVED_NAMES_CONST)]
                        nms <- nms[!(nms %in% names(self$children))]
                        nms <- nms[!(str_sub(nms, 1, 1) == '.')]
                        return (nms)
                      },
                      
                      fieldsAll = function() {
                        as.vector(na.omit(unique(unlist(self$Get("fields")))))
                      },
                      
                      levelName = function() {
                        paste0(.separator(self), self$name)
                      },
                      
                      
                      
                      leaves = function() {
                        if (self$isLeaf) {
                          return (self)
                        } else {
                          unlist(sapply(self$children, function(x) x$leaves))
                        }
                      },
                      
                      leafCount = function() {
                        length(Traverse(self, filterFun = isLeaf))
                      },
                      
                      level = function() {
                        if (self$isRoot) {
                          return (1)
                        } else {
                          return (1 + self$parent$level)
                        }
                      },
                      
                      height = function() {
                        if (isLeaf(self)) return (1)
                        max(self$Get("level", filterFun = function(x) isLeaf(x) && x$position == 1)) - self$level + 1
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
                      },
                      
                      averageBranchingFactor = function() {
                        averageBranchingFactor(self)
                      }
                      
                      
                      
                      
                    ),
                
                    private = list(
                      
                      p_name = ""
                      
                    )
                  )

