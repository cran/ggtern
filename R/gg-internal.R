#Expose some required functions from the parent ggplot2 namespace
.getFunctions <- function(){

  #' OLD FUNCTIONS  
  #'new_panel','train_layout','train_position','train_ranges','map_position','map_layout','reset_scales','facet_render',
  #'xlabel','ylabel'
  
  .functions.ggplot2   = c('create_layout',
                           #'expand_default', ## REMOVED
                           'plot_theme',
                           'element_render',# 'message_wrap',
                           'set_last_plot','make_labels',
                        # 'build_guides',
                        # 'is.zero',
                        #  is_zero,
                           'add_ggplot','labelGrob',
                           'is.layer','is.facet','is.Coord','GeomSegment',
                           '.element_tree',
                           # 'el_def', ## NOW EXPORTED
                           'expand_limits_scale', ## NEW
                           'view_scale_primary', ## NEW
                           'view_scale_secondary', ## NEW
                           'combine_elements','aes_to_scale',
                           'is.Coord','is.facet','is.layer','make_labels',
                           #'update_labels','update_guides', ## REMOVED 12 Nov 2025
                           # 'update_theme', ## REMOVED
                           'aes_to_scale',
                        # 'scales_add_missing',
                           'scales_list',
                           'guides_list',
                        # 'scales_transform_df',
                        # 'scales_map_df',
                        # 'scales_train_df',
                           'predictdf',
                           # 'contour_lines', ## REMOVED
                           'check_required_aesthetics','snake_class',
                           'ggname','ggplot_gtable','camelize',
                        
                           # 'element_grob.element_line', 'element_grob.element_rect','element_grob.element_text','element_grob.element_blank', #REMOVED 12 NOV 2025
                             'element_line', 'element_rect','element_text','element_blank', #REPLACED 12 NOV 2025
                        
                           'plot_clone','compute_just','labelGrob',
                           # 'hexGrob', ## REMOVED 12 Nov 2025
                           # 'try_require', ## REMOVED
                           'hex_binwidth','hexBinSummarise',
                           'find_args','is.margin',
                           #'justify_grobs', ## REMOVED 12 Nov 2025
                           'attach_plot_env',
                           'by_layer',
                           'table_add_tag',
                           'table_add_legends',
                           'table_add_tag',
                        
                           'validate_mapping', ##NEH V4.0
                           'arg_enquos', ##NEH V4.0
                           'setup_plot_labels', ## NEH 4.0
                           'plot_theme', ## NEH 4.0
                           'try_prop', ## NH 4.0
                           'attach_plot_env', ## NH 4.0
                           'is_rel' ## NH 4.0
                           )
  .functions.gridExtra  = c('latticeGrob')
    .functions          = rbind(data.frame(p='ggplot2',  f=unique(.functions.ggplot2)),
                                data.frame(p='gridExtra',f=unique(.functions.gridExtra)))
  
    structure(
      mapply(function(f,p){ getFromNamespace(f,p) },as.character(.functions$f), as.character(.functions$p)),
      class=c("internal")
    )
}

ggint <- .getFunctions()
