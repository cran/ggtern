#' Apply Manual Clipping Mask
#' 
#' This function creates a manual clipping mask, which in turn suppresses the standard clipping mask that would otherwise
#' be rendered in the foregound rendering procedure, giving the user control over the exact placement with respect to 
#' other layers. For example, the user may wish to have the clipping mask placed after 
#' the \code{geom_point(...)} layer, but before the \code{geom_label(...)} layer, this situation has been
#' demonstrated in the example below. In the event that the user wishes to suppress the mask altogether, then a convenience
#' function has been provided, \code{theme_nomask()}.
#' @author Nicholas Hamilton
#' @examples 
#' data(Feldspar)
#' x = ggtern(Feldspar,aes(Ab,An,Or,label=Experiment)) + geom_point()
#' 
#' #Default Behaviour
#' x + geom_label()
#' 
#' #Insert manual mask before the labels, to prevent them being truncated
#' x + geom_point(size=6) + geom_mask() + geom_label()
#' @export
#' @rdname geom_mask
geom_mask <- function() {
  layer(
    data        = data.frame(x=1,y=1,z=1),
    mapping     = NULL,
    stat        = "identity",
    geom        = GeomMask,
    position    = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = TRUE
    )
  )
}


#' @rdname geom_mask
#' @format NULL
#' @usage NULL
GeomMask <- ggproto("GeomMask", Geom,
  default_aes = aes("x","y","z"),
  draw_panel  = function(self, data, panel_scales, coord){
    
    items = list()
    
    #Only for coord tern
    if(!inherits(coord,'CoordTern'))
      return(items)
    
    tryCatch({
      theme         = coord$theme %||% theme_get()
      themeElements = c('tern.panel.background','tern.plot.background','tern.plot.background')
      for(ixEl in seq_along(themeElements)){
        
        e  = calc_element(themeElements[ixEl],theme,verbose=F)
        
        if(!identical(e,element_blank())){
          
          #1st pass is master triangle.
          if(ixEl == 1){
            ex  = data.frame(diag(1,3,3)); colnames(ex) = as.character(coord$mapping)
          }else{
            ex  = .get.tern.extremes(coord,panel_scales,transform=FALSE)
          }
          ex   = coord$transform(ex,scale_details = panel_scales)
          ex   = rbind(ex,ex[1,,drop=F])
          
          #Key Limits
          a = c(0.0,1.0); b = 0.5
          
          #2nd pass is the global white mask, 
          #which expands beyond the plot region
          #this is to mask any 'edge' effects when using limiting region
          if(ixEl == 2){ 
            a    = expand_range(a,1) #EXPAND THE TOP MASK
            fill = 'white'
            clip = 'off'
          }else{
            fill = e$fill
            clip = 'inherit'
          }
          
          #Build a specific viewport for this ixEl value
          vp <- viewport(x     = 0.5, 
                         y     = 0.5, 
                         width = 1, 
                         height= 1, 
                         just  = c("center","center"),
                         clip  = clip
          )
          
          #1st pass traces the all borders includeing the inside triangle,
          #2nd pass renders the convex hull (outer border)
          for(ix in c(1:2)){
            
            #Build the x values
            xvals = c(a[1],a[1],b[1],if(ix==1){ ex$x }else{NULL},b[1],a[2],a[2],a[1])
            
            #Build the yvalues
            yvals = c(a[1],a[2],a[2],if(ix==1){ ex$y }else{NULL},a[2],a[2],a[1],a[1])
            
            #Local Fill Variable
            fillLoc = if(ix == 2 | is.null(fill)) NA else fill
            sizeLoc = if(ix == 1) 0 else is.numericor(e$size,0)
            
            #Build the Grob with the custom viewport
            grob     <- polygonGrob(  x = xvals,
                                      y = yvals,
                                      default.units = "npc",
                                      id   = rep(1,length(xvals)),
                                      vp   = vp,
                                      name = sprintf("mask-%i-%i",ixEl,ix),
                                      gp   = gpar(  col  = if(ix != 1){ e$colour }else{ fillLoc },
                                                    fill = alpha(fillLoc, is.numericor(e$alpha,1) ),
                                                    lwd  = sizeLoc*find_global_tern(".pt"),
                                                    lty  = e$linetype)
                                      
            )
            
            #Add the grob to the items
            items[[length(items) + 1]] = grob
          }
        }
      }
      
      #Render Foreground on top of the mask
      if(!.theme.get.gridsontop(theme)){
        extrm = .get.tern.extremes(coord,panel_scales,transform=TRUE)
        items = .render.fgset(coord,extrm,scale_details,theme,items)
      }
      
    },error=function(e){
      writeLines(as.character(e))
    })
    
    do.call("gList",items)
  },
  draw_key = FALSE
)