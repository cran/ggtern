#' @inheritParams stat_density_tern
#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @export
stat_interpolate_tern <- function(mapping = NULL, data = NULL, geom = "interpolate_tern",position = "identity",
                                  ...,
                                  method='auto', na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE, n=80, formula=value~poly(x,y,degree=1),base='ilr') {
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
      base      = base,
      ...
    )
  )
}

#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @usage NULL
#' @export
StatInterpolateTern <- ggproto("StatInterpolateTern", 
  Stat,
  retransform   = FALSE,
  required_aes  = c("x", "y", "z", "value"),
  default_aes   = aes(order = ..level..),
  setup_params  = function(data, params) {
    if (identical(params$method, "auto")) {
      max_group <- max(table(data$group))
      params$method <- if(max_group < 1000){'loess'}else{'glm'}
    }
    #if (identical(params$method, "gam")) { params$method <- mgcv::gam } ##NH
    params
  },
  compute_group = function(self, data, scales, method='auto', bins = NULL, binwidth = NULL, breaks = NULL, 
                           complete  = FALSE, na.rm = FALSE, formula=value~poly(x,y,degree=1), 
                           fullrange = FALSE, n = 80, expand=0.5, method.args=list(),base='ilr') {
    
    if(!base %in% c('identity','ilr')) 
      stop('base must be either identity or ilr',call.=FALSE)
    
    #Check required aesthetics
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Ensure it is a composition
    data[self$required_aes[1:3]] = acomp(data[self$required_aes[1:3]])
    
    #Determine the Breaks
    values = data[self$required_aes[4]]
    if (is.null(bins) && is.null(binwidth) && is.null(breaks))
      breaks <- pretty(range(values), 10)
    if (!is.null(bins))
      binwidth <- diff(range(values))/bins
    if (is.null(breaks))
      breaks <- fullseq(range(values), binwidth)
    
    #Build the ternary grid
    #theGrid = .getGrid(data,n,fullrange,expand=0)
    
    #Transform the data into the orthonormal space
    din  = data[self$required_aes[1:3]]
    func = if(base == 'ilr') base else tlr2xy
    args = if(base == 'ilr') list(x=din) else list(data=din,coord=coord_tern(),inverse=FALSE)
    data[self$required_aes[1:2]] = do.call(func,args=args)
    
    #Build the model
    base.args = list(formula = quote(formula), 
                     data    = quote(data))
    model     = do.call(method, c(base.args, method.args))
    
    ##Check expand is vector of 2
    expand   = if(length(expand) != 2) rep(expand[1],2) else expand
    
    #New Data to Predict
    #xrng    = expand_range(range(theGrid$x),expand[1])#yrng    = expand_range(range(theGrid$y),expand[2])
    xrng    = expand_range(range(data[self$required_aes[1]]),expand[1])
    yrng    = expand_range(range(data[self$required_aes[2]]),expand[2])
    
    #Predict the data
    data = predictdf2d(model, 
                       xseq = seq(xrng[1],xrng[2],length.out=n), 
                       yseq = seq(yrng[1],yrng[2],length.out=n))
    data = data[which(complete.cases(data)),]
    
    #Draw the contours
    result    = StatContour$compute_group(data,scales,bins=bins,binwidth=binwidth,breaks=breaks,complete=complete,na.rm=na.rm)
    
    #Transform the data out of orthonormal space, into ternary space
    din  = result[self$required_aes[1:2]]
    func = if(base == 'ilr') sprintf("%sInv",base) else tlr2xy
    args = if(base == 'ilr') list(z=din) else list(data = din, coord  = coord_tern(), inverse=TRUE)
    result[self$required_aes[1:3]] = do.call(func,args)
    
    #Done
    result
  }
)

.getGrid <- function(data,n,fullrange,expand){
  ## REDUNDANT
  
  #Determine the limits
  seqmax = .getSeq(c(0,1),n,expand); 
  seqx   = ifthenelse(fullrange,seqmax,.getSeq(range(data$x),n,expand)) 
  seqy   = ifthenelse(fullrange,seqmax,.getSeq(range(data$y),n,expand))
  
  #Build the grid
  theGrid   = expand.grid(x=seqx,y=seqy)
  theGrid$z = 1 - theGrid$x - theGrid$y
  
  invalid   = apply(theGrid,1,function(x) max(x) > (1 - 1/n) | min(x) < 1/n)
  if(length(invalid) > 0) theGrid = theGrid[-which(invalid),]
  
  #Convert to ilr coordinates
  theGrid = remove_missing(as.data.frame(acomp(theGrid)),na.rm=TRUE)
  theGrid = data.frame(ilr(theGrid))
  colnames(theGrid) = c('x','y')
  
  #Done
  theGrid
}

.getSeq = function(x,n,expand){ 
  x = expand_range(x,expand)
  seq(x[1],x[2],length.out = n) 
}



