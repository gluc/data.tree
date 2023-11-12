#' Register a method for a suggested dependency
#'
#' Code copied into data.tree from `vctrs` (authors Wickham H, Henry L,
#' Vaughan D; https://github.com/r-lib/vctrs)
#'
#'
#' For R 3.5.0 and later, `s3_register()` is useful when demonstrating
#' class creation in a vignette, since method lookup no longer always involves
#' the lexical scope. For R 3.6.0 and later, you can achieve a similar effect
#' by using "delayed method registration".
#'
#'
#' @param generic Name of the generic in the form `pkg::generic`.
#' @param class Name of the class
#' @param method Optionally, the implementation of the method. By default,
#'   this will be found by looking for a function called `generic.class`
#'   in the package environment.
#'
#'   Note that providing `method` can be dangerous if you use
#'   devtools. When the namespace of the method is reloaded by
#'   `devtools::load_all()`, the function will keep inheriting from
#'   the old namespace. This might cause crashes because of dangling
#'   `.Call()` pointers.
#' @examples
#' # A typical use case is to dynamically register tibble/pillar methods
#' # for your class. That way you avoid creating a hard depedency on packages
#' # that are not essential, while still providing finer control over
#' # printing when they are used.
#'
#' .onLoad <- function(...) {
#'   s3_register("pillar::pillar_shaft", "vctrs_vctr")
#'   s3_register("tibble::type_sum", "vctrs_vctr")
#' }
#' @keywords internal
# nocov start
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  
  caller <- parent.frame()
  
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }
  
  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)
      
      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)
      
      registerS3method(generic, class, method_fn, envir = ns)
    }
  )
  
  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }
  
  envir <- asNamespace(package)
  
  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }
  
  invisible()
}

# nocov end