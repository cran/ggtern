#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @export
stat_interpolate_tern <- function(mapping = NULL, data = NULL, geom = "InterpolateTern",
                         position = "identity", method='auto', na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,n=80, formula=value~poly(x,y,degree=1), ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatInterpolateTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      formula   = formula,
      method    = method,
      n         = n,
      ...
    )
  )
}

#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @export
StatInterpolateTern <- ggproto("StatInterpolateTern", 
  Stat,
  retransform   = FALSE,
  required_aes  = c("x", "y", "z", "value"),
  default_aes   = aes(order = ..level..),
  setup_params  = function(data, params) {
    if (identical(params$method, "auto")) {
      max_group <- max(table(data$group))
      if (max_group < 1000) {
        params$method <- "loess"
      } else {
        params$method <- "gam"
        params$formula <- y ~ s(x, bs = "cs")
      }
    }
    if (identical(params$method, "gam")) { params$method <- mgcv::gam }
    params
  },
  compute_group = function(self,data, scales, method='auto', bins = NULL, binwidth = NULL, breaks = NULL, 
                           complete = FALSE, na.rm = FALSE,formula=value~poly(x,y,degree=1), 
                           n = 80, expand=0.5, method.args=list()) {
    
    #Check required aesthetics
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Transform the data into the orthonormal space
    data[,self$required_aes[1:2]] = as.data.frame(ilr(data[,self$required_aes[1:3]]))
    data[,self$required_aes[3]]   = NULL
    
    #Build the model
    base.args <- list(quote(formula), data = quote(data))
    model     <- do.call(method, c(base.args, method.args))
    
    #New Data to Predict
    rng      = expand_range(c(-6,6),expand)
    newdata  = expand.grid(x = seq(rng[1],rng[2],length.out = n), 
                           y = seq(rng[1],rng[2],length.out = n))
    
    #Predict the data
    data = data.frame(newdata,z=as.numeric(predict(model,newdata=newdata)))
    data = remove_missing(data,vars=self$required_aes,na.rm=TRUE,name=class(self)[1],finite=TRUE)
    if( empty(data)) return(data.frame())
    
    #Determine the bins / breaks
    if ( is.null(bins) && is.null(binwidth) && is.null(breaks)) { breaks <- pretty(range(data$z), 10) }
    if (!is.null(bins))   { binwidth <- diff(range(data$z)) / bins }
    if ( is.null(breaks)) { breaks   <- fullseq(range(data$z), binwidth) }
    
    #Process the data
    result = ggint$contour_lines(data, breaks, complete = complete)
    
    #Do the prediction
    result[,self$required_aes[1:3]] = as.data.frame(ilrInv(result[,self$required_aes[1:2]]))
    
    #Done
    result
  }
)