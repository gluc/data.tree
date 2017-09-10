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
                                'Navigate',
                                'FindNode',
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
                                'siblings',
                                'Sort',
                                'totalCount',
                                '.*')


#' Create a \code{data.tree} Structure With \code{Nodes}
#' 
#' @description \code{Node} is at the very heart of the \code{data.tree} package. All trees are constructed
#' by tying together \code{Node} objects.
#' 
#' @details Assemble \code{Node} objects into a \code{data.tree}
#' structure and use the traversal methods to set, get, and perform operations on it. Typically, you construct larger tree 
#' structures by converting from \code{data.frame}, \code{list}, or other formats.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @field children A list of child \code{Nodes}
#' @field parent The node's parent \code{Node}
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Node$new(name)}}{Creates a new \code{Node} called \code{name}. Often used to construct the root when creating trees programmatically.}
#'   \item{\code{AddChild(name)}}{Creates a new \code{Node} called \code{name} and adds it to this \code{Node} as a child.}
#'   \item{\code{AddChildNode(node)}}{Adds a \code{Node} as a child.}
#'   \item{\code{AddSibling(name)}}{Creates a new \code{Node} called \code{name} and adds it after this \code{Node} as a sibling.}
#'   \item{\code{AddSiblingNode(sibling)}}{Adds a new \code{Node} after this \code{Node}, as a sibling.}      
#'   \item{\code{RemoveChild(name)}}{Remove the child \code{Node} called \code{name} from a \code{Node} and returns it.}
#'   \item{\code{RemoveAttribute(name, stopIfNotAvailable)}}{Removes attribute called \code{name} from this \code{Node}. Gives an error if \code{stopIfNotAvailable} and the attribute does not exist.}
#'   \item{\code{\link{Climb}(...)}}{Find a node with path \code{...}, where the \code{...} arguments are the \code{name}s of the \code{Node}s, or other field values.}
#'   \item{\code{\link{Navigate}(path)}}{Find a node by relative \code{path}}
#'   \item{\code{\link{FindNode}(name)}}{Find a node with name \code{name}. Especially useful if \code{\link{AreNamesUnique}} is \code{TRUE}}
#'   \item{\code{\link{Get}(attribute, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL, format = NULL, inheritFromAncestors = FALSE, simplify = c(TRUE, FALSE, "array", "regular"))}}{Traverses the tree and collects values along the way.}
#'   \item{\code{\link{Do}(fun, ..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NUL)}}{Traverses the tree and call fun on each node.}
#'   \item{\code{\link{Set}(..., traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), pruneFun = NULL, filterFun = NULL)}}{Traverses the tree and assigns the args along the way, recycling the args.}
#'   \item{\code{\link{Sort}(attribute, ..., decreasing = FALSE, recursive = TRUE}}{Sort children of a node with respect to an attribute (field, method, active, function)}
#'   \item{\code{\link{Revert}(recursive = TRUE)}}{Revert the sort order of a node}
#'   \item{\code{\link{Prune}(pruneFun)}}{Prune a tree. The pruneFun takes a node as its first argument, and returns TRUE if the node should be kept, FALSE otherwise}
#'
#' }
#' 
#' @section Actives (aka Properties):
#'   
#' \describe{
#'  \item{\code{name}}{Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}}
#'  \item{\code{parent}}{Gets or sets the parent \code{Node} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!}
#'  \item{\code{children}}{Gets or sets the children \code{list} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!}
#'  \item{\code{siblings}}{Returns a list of the siblings of this \code{Node}}
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
#'  \item{\code{level}}{Returns an integer representing the level of a \code{Node}. For example, the root has level 1.}
#'  \item{\code{height}}{Returns max(level) of any of the \code{Nodes} of the tree}
#'  \item{\code{averageBranchingFactor}}{Returns the average number of crotches below this \code{Node}}
#'  \item{\code{root}}{Returns the root \code{Node} of a \code{Node}'s tree}
#'  
#' }
#' 
#' @usage # n1 <- Node$new("Node 1")
#'
#' @examples
#' library(data.tree)
#' acme <- Node$new("Acme Inc.")
#' accounting <- acme$AddChild("Accounting")$
#'               AddSibling("Research")$
#'               AddChild("New Labs")$
#'               parent$
#'               AddSibling("IT")$
#'               AddChild("Outsource")
#' print(acme)
#' 
#' @seealso For more details see the \code{\link{data.tree}} documentations, or the \code{data.tree} vignette: \code{vignette("data.tree")}
#'
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
Node <- R6Class("Node",
                lock_object = FALSE,
                lock_class = TRUE,
                portable = TRUE,
                class = TRUE,
                cloneable = TRUE,
                    public = list(
                      
                      
                      initialize=function(name, check = c("check", "no-warn", "no-check"), ...) {
                        if (!missing(name)) {
                          name <- as.character(name)
                          name <- CheckNameReservedWord(name, check)
                          private$p_name <- name
                        }
                        if (!missing(...)) {
                          args <- list(...)
                          mapply(FUN = function(arg, nme) self[[nme]] <- arg, args, names(args))
                        }
                        invisible (self)
                      },
                      
                      
                      
                      ####################
                      # Tree creation
                      
                      AddChild = function(name, check = c("check", "no-warn", "no-check"), ...) {
                        child <- Node$new(as.character(name), check, ...)
                        invisible (self$AddChildNode(child))
                      },
                      
                      AddChildNode = function(child) {
                        private$p_children[[child$name]] <- child
                        self[[child$name]] <- child
                        child$parent <- self
                        invisible (child)
                      },
                      
                      
                      AddSibling = function(name, check = c("check", "no-warn", "no-check"), ...) {
                        sibling <- Node$new(as.character(name), check, ...)
                        invisible (self$AddSiblingNode(sibling))
                      },
                      
                      AddSiblingNode = function(sibling) {
                        if(isRoot(self)) stop("Cannot insert sibling to root!")
                        private$p_parent[[sibling$name]] <- sibling
                        private$p_parent$children <- append(private$p_parent$children, sibling, after = self$position)
                        names(private$p_parent$children)[self$position + 1] <- sibling$name
                        sibling$parent <- private$p_parent
                        invisible (sibling)
                      },
                      
                      
                      RemoveChild = function(name) {
                        if (!name %in% names(private$p_children)) stop(paste0("Node ", self$name, " does not contain child ", name))
                        child <- private$p_children[[name]]
                        self$RemoveAttribute(name)
                        private$p_children <- private$p_children[-child$position]
                        child$parent <- NULL
                        return (child)
                      },
                      
                      RemoveAttribute = function(name, stopIfNotAvailable = TRUE) {
                        attAvailable <- name %in% ls(self)
                        if (stopIfNotAvailable && !attAvailable) stop(paste0("Node ", self$name, " does not contain field ", name))
                        else if (attAvailable) {
                          rm(list = name, envir = self)
                          return (TRUE)
                        }
                        return (FALSE)
                      },
                      
                      
                      # End Tree Creation
                      ########################
                      
                      ########################
                      ## Side Effects
                      
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        .Deprecated("Sort(node, ...)")
                        Sort(self, attribute, ..., decreasing = decreasing, recursive = recursive)  
                      },
                      
                      Revert = function(recursive = TRUE) {
                        .Deprecated("Revert(node, ...)")
                        Revert(self, recursive)
                      },
                      
                      Prune = function(pruneFun) {
                        .Deprecated("Prune(node, ...)")
                        Prune(self, pruneFun = pruneFun)
                      },
                      
                      
                      # End Side Effects
                      ###########################
                      
                      
                      
                      Climb = function(...) {
                        Climb(self, ...)
                      },
                      
                      Navigate = function(path) {
                        .Deprecated("Navigate(node, ...)")
                        Navigate(self, path)
                      },
                      
                      FindNode = function(name) {
                        .Deprecated("FindNode(node, ...)")
                        FindNode(self, name)
                      },
                      
                      
                      ##########################
                      # Traversal
                      
                      Get = function(attribute, 
                                     ..., 
                                     traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),  
                                     pruneFun = NULL,
                                     filterFun = NULL, 
                                     format = FALSE,
                                     inheritFromAncestors = FALSE,
                                     simplify = c(TRUE, FALSE, "array", "regular")) {
                        t <- Traverse(self, 
                                      traversal = traversal, 
                                      pruneFun = pruneFun,
                                      filterFun = filterFun)
                        Get(t, 
                            attribute, 
                            ...,  
                            format = format, 
                            inheritFromAncestors = inheritFromAncestors,
                            simplify = simplify)

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
                      
                      
                      parent = function(value) {
                        if (missing(value)) return (private$p_parent)
                        if (!is.null(value) && !is(value, "Node")) stop("Cannot set the parent to a non-Node!")
                        private$p_parent <- value
                      },
                      
                      children = function(value) {
                        if (missing(value)) return (private$p_children)
                        if (!is.null(value) && !is.list(value)) stop("Cannot set children to non-list!")
                        private$p_children <- value
                      },
                      
                      isLeaf = function() {
                        isLeaf(self) 
                      },
                      
                      isRoot = function() {
                        isRoot(self)
                      },
                      
                      count = function() {
                        return (length(private$p_children))
                      },
                      
                      totalCount = function() {
                        return (1 + sum(as.numeric(sapply(private$p_children, function(x) x$totalCount, simplify = TRUE, USE.NAMES = FALSE))))
                      }, 
                      
                      path = function() {
                        c(private$p_parent$path, self$name)
                      }, 
                      
                      pathString = function() {
                        paste(self$path, collapse="/")
                      },
                      
                      position = function() {
                        if (isRoot(self)) return (1)
                        
                        result <- which(names(private$p_parent$children) == self$name)
                        # match(self$name, names(private$p_parent$children))
                        return (result)
                      },
                                            
                      fields = function() {
                        nms <- ls(self)
                        nms <- nms[!(nms %in% NODE_RESERVED_NAMES_CONST)]
                        nms <- nms[!(nms %in% names(private$p_children))]
                        nms <- nms[!(str_sub(nms, 1, 1) == '.')]
                        return (nms)
                      },
                      
                      fieldsAll = function() {
                        as.vector(na.omit(unique(unlist(Get(Traverse(self), "fields")))))
                      },
                      
                      levelName = function() {
                        paste0(.separator(self), self$name)
                      },
                      
                      
                      
                      leaves = function() {
                        if (self$isLeaf) {
                          return (list(self))
                        } else {
                          unlist(sapply(private$p_children, function(x) x$leaves))
                        }
                      },
                      
                      leafCount = function() {
                        length(Traverse(self, filterFun = isLeaf))
                      },
                      
                      level = function() {
                        if (isRoot(self)) {
                          return (1)
                        } else {
                          return (1 + private$p_parent$level)
                        }
                      },
                      
                      height = function() {
                        if (isLeaf(self)) return (1)
                        max(Get(Traverse(self, filterFun = function(x) isLeaf(x) && x$position == 1), "level")) - self$level + 1
                      },
                      
                      isBinary = function() {
                        all(2 == Get(Traverse(self, filterFun = function(x) !x$isLeaf), "count"))
                      },
                      
                      root = function() {
                        if (isRoot(self)) {
                          invisible (self)
                        } else {
                          invisible (private$p_parent$root)
                        }
                      },
                      
                      siblings = function() {
                        if (isRoot(self)) {
                          return (list())
                        } else {
                          private$p_parent$children[names(private$p_parent$children) != self$name]
                        }
                      },
                      
                      averageBranchingFactor = function() {
                        averageBranchingFactor(self)
                      }
                      
                      
                      
                      
                    ),
                
                    private = list(
                      
                      p_name = "",
                      p_children = NULL,
                      p_parent = NULL
                      
                    )
                  )

