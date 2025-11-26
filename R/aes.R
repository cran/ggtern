#' Modified Aesthetic Mappings
#' 
#' @details 
#' An extension to the base aes functin from ggplot2, this is modified to handle a default z mapping 
#' for application in ternary phase diagrams. Does not alter the standard behaviour. 
#' 
#' @param x x value
#' @param y y value
#' @param z z value
#' @param ... other arguments as per \code{\link[ggplot2]{aes}}
#' @seealso Parent \code{\link[ggplot2]{aes}} function.
#' @rdname aes
#' @export
aes <- function(x, y, z, ...) {
  
  xs <- arg_enquos("x")
  ys <- arg_enquos("y")
  zs <- arg_enquos("z")
  dots <- enquos(...)
  
  args <- c(xs, ys, zs, dots)
  args <- Filter(Negate(quo_is_missing), args)
  
  # Pass arguments to helper dummy to throw an error when duplicate
  # `x`, `y` and `z` arguments are passed through dots
  local({
    aes <- function(x, y, z, ...) NULL
    inject(aes(!!!args))
  })
  
  class_mapping(rename_aes(args), env = parent.frame())
}

#aes <- function(x,y,z,...) {
#  aes <- structure(as.list(match.call()[-1]), class = "uneval")
#  rename_aes(aes)
#}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  aa = c(getFromNamespace('.all_aesthetics','ggplot2'),"T","L","R","zend")
  # Convert prefixes to full names
  full <- match(names(x),aa)
  names(x)[!is.na(full)] <- aa[full[!is.na(full)]]
  plyr::rename(x, find_global_tern(".base_to_ggplot"), warn_missing = FALSE)
}

# Look up the scale that should be used for a given aesthetic -- ternary version
aes_to_scale_tern = function (var){
  var = ggint$aes_to_scale(var)
  var[var %in% c("z", "zmin", "zmax", "zend", "zintercept")] <- "z"
  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale_tern(vars) %in% c("x", "y", "z")
}

# Takes a quosure and returns a named list of quosures, expanding
# `!!!` expressions as needed
arg_enquos <- function(name, frame = caller_env()) {
  # First start with `enquo0()` which does not process injection
  # operators
  quo <- inject(enquo0(!!sym(name)), frame)
  expr <- quo_get_expr(quo)
  
  is_triple_bang <- !is_missing(expr) &&
    is_bang(expr) && is_bang(expr[[2]]) && is_bang(expr[[c(2, 2)]])
  if (is_triple_bang) {
    # Evaluate `!!!` operand and create a list of quosures
    env <- quo_get_env(quo)
    xs <- eval_bare(expr[[2]][[2]][[2]], env)
    xs <- lapply(xs, as_quosure, env = env)
  } else {
    # Redefuse `x` to process injection operators, then store in a
    # length-1 list of quosures
    quo <- inject(enquo(!!sym(name)), frame)
    xs <- set_names(list(quo), name)
  }
  
  new_quosures(xs)
}