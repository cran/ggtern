#' Hexbin (ggtern version).
#' 
#' Divides the plane into regular hexagons, counts the number of cases in
#' each hexagon, and then (by default) maps the number of cases to the hexagon
#' fill.  Hexagon bins avoid the visual artefacts sometimes generated by
#' the very regular alignment of [geom_bin2d()].
#' 
#' This geometry is loosely based on the base ggplot2 geom_hex, with a few subtle
#' (but advantageous differences). The user can control the border thickness of the
#' hexagonal polygons using the size aesthetic. The user can also control the
#' particular statistic to use, by defining the \code{fun} argument (sum by default), which by default
#' is applied over a value of 1 per point, however, this can also be mapped to a data variable
#' via the 'value' mapping.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "hex")}
#' @inheritParams ggplot2::geom_hex
#' @param geom,stat Override the default connection between `geom_hex_tern` and `stat_hex_tern`
#' @param fun the scalar function to use for the statistic
#' @rdname geom_hex_tern
#' @examples 
#' set.seed(1)
#' n  = 1000
#' df = data.frame(x  = runif(n),
#'                 y  = runif(n),
#'                 z  = runif(n),
#'                 wt = runif(n))
#'                 
#' #Equivalent of Hexbin
#' ggtern(df,aes(x,y,z)) + 
#'     geom_hex_tern(binwidth=0.1)
#'     
#' #Calculate Mean of variable wt
#' ggtern(df,aes(x,y,z)) + 
#'      geom_hex_tern(binwidth=0.05,
#'                    aes(value=wt),
#'                    fun=mean)
#'
#' #Custom functions, for ex. discrete output...
#' myfun = function(x) sample(LETTERS,1)
#' ggtern(df,aes(x,y,z)) + 
#'      geom_hex_tern(binwidth=0.05,
#'                    fun=myfun)
#' 
#' @export
geom_hex_tern <- function(mapping = NULL, data = NULL,
                     stat = "hex_tern", position = "identity",
                     ...,
                     fun = sum,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomHexTern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm = na.rm,
      fun   = fun,
      ...
    )
  )
}


#' @rdname geom_hex_tern
#' @format NULL
#' @usage NULL
#' @import hexbin
#' @export
GeomHexTern <- ggproto("GeomHexTern", Geom,
    setup_data = function(data,params){
      #in ggplot2, this is in a dedicated grob, however, we need to resolve
      #prior to potential rotation of the ternary canvas
      #therfore, we introduce hexagons as polygons with their unique id
      
      #Put into cartesian
      coord       = coord_tern()
      data        = tlr2xy(data,coord)
      
      #Hex Template  
      dx          = resolution(round(data$x,6), FALSE)
      dy          = resolution(round(data$y,6), FALSE) / sqrt(3) / 2 * 1.15
      hexC        = hexbin::hexcoords(dx, dy, n = 1)
      hexC        = hexcoords(dx, dy, n = 1)
      
      #Base Points
      n           = nrow(data)
      
      #Add hex template to current points
      data$group  = sprintf("%s%s",seq_len(n),data$group)
      data        = data[rep(seq_len(n), each=6),]  #1 -> 6 mapping
      data$x      = data$x + rep.int(hexC$x, n)     #Add template x
      data$y      = data$y + rep.int(hexC$y, n)     #Add template y
      
      #Put back to ternary
      tlr2xy(data,coord,inverse=TRUE)
    },   
    draw_group = function(data,panel_params,coord){
      if (!inherits(coord, "CoordTern"))
             stop("geom_hex_tern() only works with Ternary coordinates", call. = FALSE)
      
      #Transform
      data = coord$transform(data, panel_params)
      
      #Polygon Grob
      ggint$ggname("geom_hex_tern", 
          polygonGrob(data$x, 
                      data$y, 
                      default.units = "native",
                      gp = gpar(
                        col  = data$colour[1], 
                        fill = alpha(data$fill[1], 
                                     data$alpha[1]),
                        lty  = data$linetype[1], 
                        lwd  = data$size[1]
                      )
          )
      )
    },
    required_aes = c("x", "y", "z"),
    default_aes = aes(colour = NA, fill = "grey50", size = 0.5, alpha = 1, linetype=1),
    draw_key = draw_key_polygon
)
