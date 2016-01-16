#' Render Grids on Top
#' 
#' Convenience function to render the major and minor grids on top of the other layers.
#' @author Nicholas Hamilton
#' @rdname theme_gridsontop
#' @export
theme_gridsontop = function(){
  theme(tern.panel.grid.ontop=TRUE)
}