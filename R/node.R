#' Names that are reserved by the Node class.
#'
#' These are reserved by the Node class, you cannot use these as 
#' attribute names.
#' Note also that all attributes starting with a . are reserved.
#' 
#' @export
NODE_RESERVED_NAMES_CONST <- c( 
                                'AddChild',
                                'AddChildNode',
                                'AddSibling',
                                'AddSiblingNode',
                                'attributes',
                                'attributesAll',
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
#' Most methods (e.g. \code{node$Sort()}) also have a functional form (e.g. \code{Sort(node)})
#' 
#' @docType class
#' @importFrom R6 R6Class
#'   
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
#'
#' @param name the name of the node to be created
#' @param check Either
#' \itemize{
#'  \item{\code{"check"}: if the name conformance should be checked and warnings should be printed in case of non-conformance (the default)}
#'  \item{\code{"no-warn"}: if the name conformance should be checked, but no warnings should be printed in case of non-conformance (if you expect non-conformance)}
#'  \item{\code{"no-check" or FALSE}: if the name conformance should not be checked; use this if performance is critical. However, in case of non-conformance, expect cryptic follow-up errors}
#' }
#' @param ... A name-value mapping of node attributes
#' @param attribute determines what is collected. The \code{attribute} can be
#'       \itemize{
#'         \item a.) the name of a \bold{field} or a \bold{property/active} of each \code{Node} in the tree, e.g. \code{acme$Get("p")} or \code{acme$Get("position")}
#'         \item b.) the name of a \bold{method} of each \code{Node} in the tree, e.g. \code{acme$Get("levelZeroBased")}, where e.g. \code{acme$levelZeroBased <- function() acme$level - 1}
#'         \item c.) a \bold{function}, whose first argument must be a \code{Node} e.g. \code{acme$Get(function(node) node$cost * node$p)}
#'        }
#'
#' @param recursive if \code{TRUE}, the method will be called recursively on the \code{Node}'s children. This allows sorting an entire tree.
#' @param traversal defines the traversal order to be used. This can be
#'  \describe{
#'    \item{pre-order}{Go to first child, then to its first child, etc.}
#'    \item{post-order}{Go to the first branch's leaf, then to its siblings, and work your way back to the root}
#'    \item{in-order}{Go to the first branch's leaf, then to its parent, and only then to the leaf's sibling}
#'    \item{level}{Collect root, then level 2, then level 3, etc.}
#'    \item{ancestor}{Take a node, then the node's parent, then that node's parent in turn, etc. This ignores the \code{pruneFun} }
#'    \item{function}{You can also provide a function, whose sole parameter is a \code{\link{Node}} object. The function is expected to return the node's next node, a list of the node's next nodes, or NULL.}
#'  }
#'  Read the data.tree vignette for a detailed explanation of these traversal orders.
#'       
#'       
#' @param pruneFun allows providing a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
#' If the pruneFun returns FALSE for a Node, then the Node and its entire sub-tree will not be considered.
#'       
#' @param filterFun allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
#' Note that if filter returns \code{FALSE}, then the node will be excluded from the result (but not the entire subtree).
#'       
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
                      
                     
                      #' @description Create a new \code{Node} object. This is often used to create the root of a tree when creating a tree programmatically.
                      #' 
                      #' @examples 
                      #' node <- Node$new("mynode", x = 2, y = "value of y")
                      #' node$y
                      #' 
                      #' @return A new `Node` object
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
                      
                      #' @description Creates a \code{Node} and adds it as the last sibling as a child to the \code{Node} on which this is called.
                      #' 
                      #' @examples 
                      #' root <- Node$new("myroot", myname = "I'm the root")
                      #' root$AddChild("child1", myname = "I'm the favorite child")
                      #' child2 <- root$AddChild("child2", myname = "I'm just another child")
                      #' child3 <- child2$AddChild("child3", myname = "Grandson of a root!")
                      #' print(root, "myname")
                      #' 
                      #' @return The new \code{Node} (invisibly)
                      AddChild = function(name, check = c("check", "no-warn", "no-check"), ...) {
                        child <- Node$new(as.character(name), check, ...)
                        invisible (self$AddChildNode(child))
                      },
                      
                      
                      #' @description Adds a \code{Node} as a child to this node.
                      #' 
                      #' @param child The child \code{"Node"} to add.
                      #' 
                      #' @examples 
                      #' root <- Node$new("myroot")
                      #' child <- Node$new("mychild")
                      #' root$AddChildNode(child)
                      #' 
                      #' @return the child node added (this lets you chain calls)
                      AddChildNode = function(child) {
                        private$p_children[[child$name]] <- child
                        self[[child$name]] <- child
                        child$parent <- self
                        invisible (child)
                      },
                      
                      
                      
                      #' @description Creates a new \code{Node} called \code{name} and adds it after this \code{Node} as a sibling.
                      #' 
                      #' @examples 
                      #' #' root <- Node$new("myroot")
                      #' child <- root$AddChild("child1")
                      #' sibling <- child$AddSibling("sibling1")
                      #' 
                      #' @return the sibling node (this lets you chain calls)
                      #' 
                      AddSibling = function(name, check = c("check", "no-warn", "no-check"), ...) {
                        sibling <- Node$new(as.character(name), check, ...)
                        invisible (self$AddSiblingNode(sibling))
                      },
                      
                      
                      #' @description Adds a \code{Node} after this \code{Node}, as a sibling.
                      #'  
                      #' @param sibling The \code{"Node"} to add as a sibling.
                      #' 
                      #' @examples 
                      #' root <- Node$new("myroot")
                      #' child <- Node$new("mychild")
                      #' sibling <- Node$new("sibling")
                      #' root$AddChildNode(child)$AddSiblingNode(sibling)
                      #' 
                      #' @return the added sibling node (this lets you chain calls, as in the examples)
                      #' 
                      AddSiblingNode = function(sibling) {
                        if(isRoot(self)) stop("Cannot insert sibling to root!")
                        private$p_parent[[sibling$name]] <- sibling
                        private$p_parent$children <- append(private$p_parent$children, sibling, after = self$position)
                        names(private$p_parent$children)[self$position + 1] <- sibling$name
                        sibling$parent <- private$p_parent
                        invisible (sibling)
                      },
                      
                      
                      #' @description Remove the child \code{Node} called \code{name} from a \code{Node} and returns it.
                      #' 
                      #' @examples 
                      #' node <- Node$new("myroot")$AddChild("mychild")$root
                      #' node$RemoveChild("mychild")
                      #' 
                      #' @return the subtree spanned by the removed child.  
                      RemoveChild = function(name) {
                        if (!name %in% names(private$p_children)) stop(paste0("Node ", self$name, " does not contain child ", name))
                        child <- private$p_children[[name]]
                        self$RemoveAttribute(name)
                        private$p_children <- private$p_children[-child$position]
                        child$parent <- NULL
                        return (child)
                      },
                      
                      
                      #' @description  Removes attribute called \code{name} from this \code{Node}. 
                      #' 
                      #' @param stopIfNotAvailable Gives an error if \code{stopIfNotAvailable} and the attribute does not exist.
                      #' 
                      #' @examples 
                      #' node <- Node$new("mynode")
                      #' node$RemoveAttribute("age", stopIfNotAvailable = FALSE)
                      #' node$age <- 27
                      #' node$RemoveAttribute("age")
                      #' node
                      #' 
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
                      
                      #' @description Sort children of a \code{Node} or an entire \code{data.tree} structure
                      #' 
                      #' @details
                      #' You can sort with respect to any argument of the tree. But note that sorting has
                      #' side-effects, meaning that you modify the underlying, original data.tree object structure.
                      #' 
                      #' See also \code{\link{Sort}} for the equivalent function.
                      #' 
                      #' 
                      #' @param ... any parameters to be passed on the the attribute (in case it's a method or a 
                      #' function)
                      #' @param decreasing sort order
                      #' 
                      #' 
                      #' @return Returns the node on which Sort is called, invisibly. This can be useful to chain Node methods.
                      #' 
                      #' @examples
                      #' data(acme)
                      #' acme$Do(function(x) x$totalCost <- Aggregate(x, "cost", sum), traversal = "post-order")
                      #' Sort(acme, "totalCost", decreasing = FALSE)
                      #' print(acme, "totalCost")
                      #' 
                      Sort = function(attribute, ..., decreasing = FALSE, recursive = TRUE) {
                        .Deprecated("Sort(node, ...)")
                        Sort(self, attribute, ..., decreasing = decreasing, recursive = recursive)  
                      },
                      
                      
                      #' @description Reverts the sort order of a \code{Node}'s children.
                      #' 
                      #' See also \code{\link{Revert}} for the equivalent function.
                      #' 
                      #' 
                      #' @return returns the Node invisibly (for chaining)
                      #'
                      #' @seealso \code{\link{Node}}
                      #' @seealso \code{\link{Sort}}
                      #' @export
                      Revert = function(recursive = TRUE) {
                        .Deprecated("Revert(node, ...)")
                        Revert(self, recursive)
                      },
                      
                      
                      #' @description Prunes a tree. 
                      #' 
                      #' Pruning refers to removing entire subtrees. This function has side-effects, it modifies your data.tree structure!
                      #' 
                      #' See also \code{\link{Prune}} for the equivalent function.
                      #' 
                      #' @param pruneFun allows providing a a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
                      #' If the pruneFun returns FALSE for a Node, then the Node and its entire sub-tree will not be considered.
                      #' @return the number of nodes removed
                      #' 
                      #' @examples
                      #' data(acme)
                      #' acme$Do(function(x) x$cost <- Aggregate(x, "cost", sum))
                      #' Prune(acme, function(x) x$cost > 700000)
                      #' print(acme, "cost")
                      #' 
                      Prune = function(pruneFun) {
                        .Deprecated("Prune(node, ...)")
                        Prune(self, pruneFun = pruneFun)
                      },
                      
                      
                      # End Side Effects
                      ###########################
                      
                      
                      
                      #' @description Climb a tree from parent to children, by provided criteria.
                      #'
                      #' @details 
                      #' This method lets you climb the tree, from crutch to crutch. On each \code{Node}, the
                      #' \code{Climb} finds the first child having attribute value equal to the the provided argument.
                      #' 
                      #' See also \code{\link{Climb}} and \code{\link{Navigate}}
                      #'
                      #' Climb(node, ...)
                      #'
                      #'
                      #' @param node The root \code{\link{Node}} of the tree or subtree to climb
                      #' @param ... an attribute-value pairlist to be searched. For brevity, you can also provide a character vector to search for names.
                      #' @return the \code{Node} having path \code{...}, or \code{NULL} if such a path does not exist
                      #'
                      #' @examples
                      #' data(acme)
                      #'
                      #' #the following are all equivalent
                      #' Climb(acme, 'IT', 'Outsource')
                      #' Climb(acme, name = 'IT', name = 'Outsource')
                      #' Climb(acme, 'IT')$Climb('Outsource')
                      #' Navigate(acme, path = "IT/Outsource")
                      #'
                      #' Climb(acme, name = 'IT')
                      #'
                      #' Climb(acme, position = c(2, 1))
                      #' #or, equivalent:
                      #' Climb(acme, position = 2, position = 1)
                      #' Climb(acme, name = "IT", cost = 250000)
                      #'
                      #' tree <- CreateRegularTree(5, 2)
                      #' tree$Climb(c("1", "1"), position = c(2, 2))$path
                      #'
                      #'
                      Climb = function(...) {
                        Climb(self, ...)
                      },
  
                      
                      #' @description Navigate to another node by relative path.
                      #'
                      #'
                      #' @param node The starting \code{\link{Node}} to navigate
                      #' @param path A string or a character vector describing the path to navigate
                      #'
                      #' @details The \code{path} is always relative to the \code{Node}. Navigation
                      #' to the parent is defined by \code{..}, whereas navigation to a child
                      #' is defined via the child's name.
                      #' If path is provided as a string, then the navigation steps are separated
                      #' by '/'.
                      #' 
                      #' See also \code{\link{Navigate}} and \code{\link{Climb}}
                      #'
                      #' @examples
                      #' data(acme)
                      #' Navigate(acme$Research, "../IT/Outsource")
                      #' Navigate(acme$Research, c("..", "IT", "Outsource"))
                      #'
                      Navigate = function(path) {
                        .Deprecated("Navigate(node, ...)")
                        Navigate(self, path)
                      },
                      
                      
                      
                      
                      ##########################
                      # Traversal
                      
                      
                      #' @description Traverse a Tree and Collect Values
                      #' 
                      #' @details 
                      #' The \code{Get} method is one of the most important ones of the \code{data.tree} package. It lets you traverse a tree
                      #' and collect values along the way. Alternatively, you can call a method or a function on each \code{\link{Node}}.
                      #' 
                      #' See also \code{\link{Get}}, \code{\link{Node}}, \code{\link{Set}}, \code{\link{Do}}, \code{\link{Traverse}}
                      #' 
                      #' 
                      #' 
                      #' 
                      #' @param attribute determines what is collected. The \code{attribute} can be
                      #'       \itemize{
                      #'         \item a.) the name of a \bold{field} or a \bold{property/active} of each \code{Node} in the tree, e.g. \code{acme$Get("p")} or \code{acme$Get("position")}
                      #'         \item b.) the name of a \bold{method} of each \code{Node} in the tree, e.g. \code{acme$Get("levelZeroBased")}, where e.g. \code{acme$levelZeroBased <- function() acme$level - 1}
                      #'         \item c.) a \bold{function}, whose first argument must be a \code{Node} e.g. \code{acme$Get(function(node) node$cost * node$p)}
                      #'        }
                      #' @param ... in case the \code{attribute} is a function or a method, the ellipsis is passed to it as additional arguments.
                      #' @param format if \code{FALSE} (the default), no formatting is being used. If \code{TRUE}, then the first formatter (if any) found along the ancestor path is being used for formatting 
                      #' (see \code{\link{SetFormat}}). If \code{format} is a function, then the collected value is passed to that function, and the result is returned.
                      #' @param inheritFromAncestors if \code{TRUE}, then the path above a \code{Node} is searched to get the \code{attribute} in case it is NULL.
                      #' @param simplify same as \code{\link{sapply}}, i.e. TRUE, FALSE or "array". Additionally, you can specify "regular" if
                      #' each returned value is of length > 1, and equally named. See below for an example.
                      #'        
                      #' @return a vector containing the \code{atrributes} collected during traversal, in traversal order. \code{NULL} is converted
                      #' to NA, such that \code{length(Node$Get) == Node$totalCount}
                      #'  
                      #'  
                      #' @examples
                      #' data(acme)
                      #' acme$Get("level")
                      #' acme$Get("totalCount")
                      #'  
                      #'
                      #' acme$Get(function(node) node$cost * node$p,
                      #'          filterFun = isLeaf)
                      #' 
                      #' #This is equivalent:
                      #' nodes <- Traverse(acme, filterFun = isLeaf)
                      #' Get(nodes, function(node) node$cost * node$p)
                      #' 
                      #'    
                      #' #simplify = "regular" will preserve names
                      #' acme$Get(function(x) c(position = x$position, level = x$level), simplify = "regular")
                      #'  
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
                      
                      
                      
                      #' @description Executes a function on a set of nodes
                      #' 
                      #' @details 
                      #' See also \code{\link{Node}}, \code{\link{Get}}, \code{\link{Set}}, \code{\link{Traverse}}
                      #' 
                      #' @param fun the function to execute. The function is expected to be either a Method, or to take a 
                      #' Node as its first argument
                      #' 
                      #' @examples 
                      #' data(acme)
                      #' acme$Do(function(node) node$expectedCost <- node$p * node$cost)
                      #' print(acme, "expectedCost")
                      #' 
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
                      
                      
                      
                      #' @description Traverse a Tree and Assign Values
                      #' 
                      #' @details 
                      #' The method takes one or more vectors as an argument. It traverses the tree, whereby the values are picked
                      #' from the vector. Also available as OO-style method on \code{\link{Node}}.
                      #' 
                      #' See also \code{\link{Node}}, \code{\link{Get}}, \code{\link{Do}}, \code{\link{Traverse}}
                      #' 
                      #' 
                      #' 
                      #' @param ... each argument can be a vector of values to be assigned. Recycled.
                      #'
                      #' @return invisibly returns the nodes (useful for chaining)  
                      #'  
                      #' @examples
                      #' data(acme)
                      #' acme$Set(departmentId = 1:acme$totalCount, openingHours = NULL, traversal = "post-order")
                      #' acme$Set(head = c("Jack Brown", 
                      #'                   "Mona Moneyhead", 
                      #'                   "Dr. Frank N. Stein", 
                      #'                   "Eric Nerdahl"
                      #'                   ),
                      #'          filterFun = function(x) !x$isLeaf
                      #'         )
                      #' print(acme, "departmentId", "head")
                      #'  
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
                      
                      #' @field name Gets or sets the name of a \code{Node}. For example \code{Node$name <- "Acme"}.
                      name = function(value) {
                        if (missing(value)) return (private$p_name)
                        else private$p_name <- changeName(self, private$p_name, value)
                      },
                      
                      #' @field parent Gets or sets the parent \code{Node} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!
                      parent = function(value) {
                        if (missing(value)) return (private$p_parent)
                        if (!is.null(value) && !is(value, "Node")) stop("Cannot set the parent to a non-Node!")
                        private$p_parent <- value
                      },
                      
                      #' @field children Gets or sets the children \code{list} of a \code{Node}. Only set this if you know what you are doing, as you might mess up the tree structure!
                      children = function(value) {
                        if (missing(value)) return (private$p_children)
                        if (!is.null(value) && !is.list(value)) stop("Cannot set children to non-list!")
                        private$p_children <- value
                      },
                      
                      #' @field isLeaf Returns \code{TRUE} if the \code{Node} is a leaf, \code{FALSE} otherwise
                      isLeaf = function() {
                        isLeaf(self) 
                      },
                      
                      #' @field isRoot Returns \code{TRUE} if the \code{Node} is the root, \code{FALSE} otherwise
                      isRoot = function() {
                        isRoot(self)
                      },
                      
                      #' @field count Returns the number of children of a \code{Node}
                      count = function() {
                        return (length(private$p_children))
                      },
                      
                      #' @field totalCount Returns the total number of \code{Node}s in the tree
                      totalCount = function() {
                        return (1 + sum(as.numeric(sapply(private$p_children, function(x) x$totalCount, simplify = TRUE, USE.NAMES = FALSE))))
                      }, 
                      
                      #' @field path Returns a vector of mode \code{character} containing the names of the \code{Node}s in the path from the root to this \code{Node}
                      path = function() {
                        c(private$p_parent$path, self$name)
                      }, 
                      
                      #' @field pathString Returns a string representing the path to this \code{Node}, separated by backslash
                      pathString = function() {
                        paste(self$path, collapse="/")
                      },
                      
                      #' @field position The position of a \code{Node} within its siblings
                      position = function() {
                        if (isRoot(self)) return (1)
                        
                        result <- which(names(private$p_parent$children) == self$name)
                        # match(self$name, names(private$p_parent$children))
                        return (result)
                      },
                                    
                      #' @field fields Will be deprecated, use \code{attributes} instead
                      fields = function() {
                        print("Node$fields will be deprecated in the next release. Please use Node$attributes instead.")
                        # .Deprecated("Node$attributes", old = "Node$fields")
                        return(self$attributes)
                      },
                      
                      #' @field fieldsAll Will be deprecated, use \code{attributesAll} instead
                      fieldsAll = function() {
                        print("Node$fieldsAll will be deprecated in the next release. Please use Node$attributesAll instead.")
                        # .Deprecated("Node$attributesAll", old = "Node$fieldsAll")
                        return(self$attributesAll)
                      },
                      
                      #' @field attributes The attributes defined on this specific node        
                      attributes = function() {
                        nms <- ls(self)
                        nms <- nms[!(nms %in% NODE_RESERVED_NAMES_CONST)]
                        nms <- nms[!(nms %in% names(private$p_children))]
                        nms <- nms[!(stri_sub(nms, 1, 1) == '.')]
                        return (nms)
                      },
                      
                      #' @field attributesAll The distinct union of attributes defined on all the nodes in the tree spanned by this \code{Node}
                      attributesAll = function() {
                        as.vector(na.omit(unique(unlist(Get(Traverse(self), "attributes", simplify = FALSE)))))
                      },
                      
                      #' @field levelName Returns the name of the \code{Node}, preceded by level times '*'. Useful for printing and not typically called by package users.
                      levelName = function() {
                        paste0(.separator(self), self$name)
                      },
                      
                      
                      #' @field leaves Returns a list containing all the leaf \code{Node}s
                      leaves = function() {
                        if (self$isLeaf) {
                          return (list(self))
                        } else {
                          unlist(sapply(private$p_children, function(x) x$leaves))
                        }
                      },
                      
                      #' @field leafCount Returns the number of leaves are below a \code{Node}
                      leafCount = function() {
                        length(Traverse(self, filterFun = isLeaf))
                      },
                      
                      #' @field level Returns an integer representing the level of a \code{Node}. For example, the root has level 1.
                      level = function() {
                        if (isRoot(self)) {
                          return (1)
                        } else {
                          return (1 + private$p_parent$level)
                        }
                      },
                      
                      #' @field height Returns max(level) of any of the \code{Nodes} of the tree
                      height = function() {
                        if (isLeaf(self)) return (1)
                        max(Get(Traverse(self, filterFun = function(x) isLeaf(x) && x$position == 1), "level")) - self$level + 1
                      },
                      
                      #' @field isBinary Returns \code{TRUE} if all \code{Node}s in the tree (except the leaves) have \code{count = 2}
                      isBinary = function() {
                        all(2 == Get(Traverse(self, filterFun = function(x) !x$isLeaf), "count"))
                      },
                      
                      #' @field root Returns the root of a \code{Node} in a tree.
                      root = function() {
                        if (isRoot(self)) {
                          invisible (self)
                        } else {
                          invisible (private$p_parent$root)
                        }
                      },
                      
                      #' @field siblings Returns a \code{list} containing all the siblings of this \code{Node}
                      siblings = function() {
                        if (isRoot(self)) {
                          return (list())
                        } else {
                          private$p_parent$children[names(private$p_parent$children) != self$name]
                        }
                      },
                      
                      #' @field averageBranchingFactor Returns the average number of crotches below this \code{Node}
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

