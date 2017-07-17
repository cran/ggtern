#Expose some required functions from the parent ggplot2 namespace
.getFunctions <- function(){

  #' OLD FUNCTIONS  
  #'new_panel','train_layout','train_position','train_ranges','map_position','map_layout','reset_scales','facet_render',
  #'xlabel','ylabel'
  
  .functions.ggplot2   = c('create_layout','expand_default','plot_theme',
                           'element_render','message_wrap',
                           'set_last_plot','make_labels','build_guides','is.zero','add_ggplot','labelGrob',
                           'is.layer','is.facet','is.Coord','GeomSegment',
                           '.element_tree','el_def','combine_elements','aes_to_scale',
                           'is.Coord','is.facet','is.layer','make_labels','update_labels','update_guides','update_theme',
                           'aes_to_scale',
                           'scales_add_missing','scales_list','scales_transform_df','scales_map_df','scales_train_df',
                           'predictdf','contour_lines','check_required_aesthetics','snake_class',
                           'ggname','ggplot_gtable','camelize',
                           'element_grob.element_line','element_grob.element_rect','element_grob.element_text','element_grob.element_blank',
                           'plot_clone','compute_just','labelGrob')
  .functions.gridExtra  = c('latticeGrob')
    .functions          = rbind(data.frame(p='ggplot2',  f=unique(.functions.ggplot2)),
                                data.frame(p='gridExtra',f=unique(.functions.gridExtra)))
  
    structure(
      mapply(function(f,p){ getFromNamespace(f,p) },as.character(.functions$f), as.character(.functions$p)),
      class=c("internal")
    )
}

ggint <- .getFunctions()
