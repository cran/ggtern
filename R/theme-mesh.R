#' Create Grid Mesh
#' 
#' Convenience function for creation of a grid mesh of an ideal number of 'n' major breaks. Note that 
#' the value of 'n' is the target number of breaks, and due to the use of the \code{\link{pretty}} function
#' within \code{\link{getBreaks}} convenience function, may not be strictly adhered or reflected.
#' @param n the 'target' number of major breaks
#' @export
theme_mesh = function(n = 5){
  if(!is.numeric(n)) 
    stop("'n' must be numeric")
  l = c(0,1)
  n = max(as.integer(n[1]),1)
  tern_limits(breaks       = getBreaks(limits = l, isMajor = T, n = n), 
              minor_breaks = getBreaks(limits = l, isMajor = F, n = n))
}