#' Modified Aesthetic Mappings
#' 
#' An extension to the base aes functin from ggplot2, this is modified to handle a default z mapping for application in ternary phase diagrams.
#' Does not alter the standard behaviour. 
#' @param x x value
#' @param y y value
#' @param z z value
#' @param ... other arguments as per \code{\link[ggplot2]{aes}}
#' @seealso Parent \code{\link[ggplot2]{aes}} function.
#' @export
aes <- function(x,y,z,...) {
  X  <- structure(as.list(match.call()[-1]), class="uneval")
  do.call(.rename_aes,args=list(x=X))
}

# Rename American or old-style aesthetics name
.rename_aes <- function(x) {
  aa = c(getFromNamespace('.all_aesthetics','ggplot2'),"T","L","R","zend")
  # Convert prefixes to full names
  full <- match(names(x),aa)
  names(x)[!is.na(full)] <- aa[full[!is.na(full)]]
  plyr::rename(x, find_global_tern(".base_to_ggplot"), warn_missing = FALSE)
}

# Look up the scale that should be used for a given aesthetic -- ternary version
aes_to_scale_tern = function (var){
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  var[var %in% c("z", "zmin", "zmax", "zend", "zintercept")] <- "z"
  var
}

