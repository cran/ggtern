#' Build ggplot for rendering (ggtern version).
#'
#' \code{ggplot_build} takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' \code{layer_data}, \code{layer_grob}, and \code{layer_scales} are helper
#' functions that returns the data, grob, or scales associated with a given
#' layer. These are useful for tests.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{\link{benchplot}} for
#'  functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @author Nicholas Hamilton
#' @name ggplot_build
#' @rdname ggplot_build
NULL

#' @export
ggplot_build <- function(plot) {
  # Attaching the plot env to be fetched by deprecations etc.
  ggint$attach_plot_env(plot$plot_env)
  UseMethod('ggplot_build')
}

#' @export
ggplot_build.ggplot <- function(plot) {
  plot <- ggint$plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  #NH Check the plot is a teryary plot
  isTernary = inherits(plot$coordinates,'CoordTern') ##NH
  if(isTernary){
    plot$layers <- strip_unapproved(plot$layers) ##NH
    plot <- layers_add_or_remove_mask(plot) #NH
  }

  layers <- plot$layers
  data <- rep(list(NULL), length(layers))

  scales <- plot$scales

  # Allow all layers to make any final adjustments based
  # on raw input data and plot info
  data <- ggint$by_layer(function(l, d) l$layer_data(plot$data), layers, data, "computing layer data")
  data <- ggint$by_layer(function(l, d) l$setup_layer(d, plot), layers, data, "setting up layer")

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data
  layout <- ggint$create_layout(plot$facet, plot$coordinates, plot$layout)
  data <- layout$setup(data, plot$data, plot$plot_env)

  # Compute aesthetics to produce data with generalised variable names
  data <- ggint$by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")
  data <- .ignore_data(data)

  # Transform all scales
  data <- lapply(data, scales$transform_df)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  data <- .expose_data(data)

  # Apply and map statistics
  data <- ggint$by_layer(function(l, d) l$compute_statistic(d, layout), layers, data, "computing stat")
  data <- ggint$by_layer(function(l, d) l$map_statistic(d, plot), layers, data, "mapping stat to aesthetics")

  # Make sure missing (but required) aesthetics are added
  # if(!isTernary)
  plot$scales$add_missing(c("x", "y"), plot$plot_env)

  # Make sure missing (but required) ternary aesthetics are added, and ensure the limits are common
  if(isTernary) 
    plot = scales_add_missing_tern(plot)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- ggint$by_layer(function(l, d) l$compute_geom_1(d), layers, data, "setting up geom")

  # Apply position adjustments
  data <- ggint$by_layer(function(l, d) l$compute_position(d, layout), layers, data, "computing position")

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what is
  # displayed, or does it include the range of underlying data
  data <- .ignore_data(data)
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Hand off position guides to layout
  layout$setup_panel_guides(plot$guides, plot$layers)

  # Train and map non-position scales and guides
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, npscales$train_df)
    plot$guides <- plot$guides$build(npscales, plot$layers, plot$labels, data)
    data <- lapply(data, npscales$map_df)
  } else {
    # Only keep custom guides if there are no non-position scales
    plot$guides <- plot$guides$get_custom()
  }
  data <- .expose_data(data)

  # Fill in defaults etc.
  data <- ggint$by_layer(function(l, d) l$compute_geom_2(d), layers, data, "setting up geom aesthetics")

  # Let layer stat have a final say before rendering
  data <- ggint$by_layer(function(l, d) l$finish_statistics(d), layers, data, "finishing layer stat")

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  # Consolidate alt-text
  plot$labels$alt <- get_alt_text(plot)

  structure(
    list(data = data, layout = layout, plot = plot),
    class = "ggplot_built"
  )
}

#ggplot_build.ggplot <- function(plot) {
#  plot <- ggint$plot_clone(plot)
#  if (length(plot$layers) == 0) {
#    plot <- plot + geom_blank()
#  }
#  
#  #NH Check the plot is a teryary plot
#  isTernary = inherits(plot$coordinates,'CoordTern') ##NH
#  if(isTernary){
#    plot$layers <- strip_unapproved(plot$layers) ##NH
#    plot <- layers_add_or_remove_mask(plot) #NH
#  }
#  
#  #Check the Layers
#  layers <- plot$layers
#  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
#  
#  scales <- plot$scales
#  
#  # Allow all layers to make any final adjustments based
#  # on raw input data and plot info
#  data <- layer_data
#  data <- by_layer(function(l, d) l$setup_layer(d, plot))
#  
#  # Initialise panels, add extra data for margins & missing facetting
#  # variables, and add on a PANEL variable to data
#  layout <- ggint$create_layout(plot$facet,plot$coordinates)
#  data   <- layout$setup(data, plot$data, plot$plot_env)
#  
#  # Compute aesthetics to produce data with generalised variable names
#  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
#  
#  # Transform all scales
#  data <- lapply(data, ggint$scales_transform_df, scales = scales) ##NH
#  
#  # Map and train positions so that statistics have access to ranges
#  # and all positions are numeric
#  scale_x <- function() scales$get_scales("x")
#  scale_y <- function() scales$get_scales("y")
#  
#  layout$train_position(data, scale_x(), scale_y())
#  data  <- layout$map_position(data)
#  
#  # Apply and map statistics
#  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
#  data <- by_layer(function(l, d) l$map_statistic(d, plot))
#  
#  # Make sure missing (but required) aesthetics are added
#  ggint$scales_add_missing(plot, c("x", "y"), plot$plot_env)
#  
#  # Make sure missing (but required) ternary aesthetics are added, and ensure the limits are common
#  if(isTernary) 
#    plot = scales_add_missing_tern(plot)
#  
#  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
#  data <- by_layer(function(l, d) l$compute_geom_1(d))
#  
#  # Apply position adjustments
#  data <- by_layer(function(l, d) l$compute_position(d, layout))
#  
#  # Reset position scales, then re-train and map.  This ensures that facets
#  # have control over the range of a plot: is it generated from what's
#  # displayed, or does it include the range of underlying data
#  layout$reset_scales()
#  layout$train_position(data, scale_x(), scale_y())
#  layout$setup_panel_params()
#  data <- layout$map_position(data)
#  
#  # Train and map non-position scales
#  npscales <- scales$non_position_scales()
#  if (npscales$n() > 0) {
#    lapply(data, ggint$scales_train_df, scales = npscales)       ##NH
#    data <- lapply(data, ggint$scales_map_df, scales = npscales) ##NH
#  }
#  
#  # Fill in defaults etc.
#  data <- by_layer(function(l, d) l$compute_geom_2(d))
#  
#  # Let layer stat have a final say before rendering
#  data <- by_layer(function(l, d) l$finish_statistics(d))
#  
#  # Let Layout modify data before rendering
#  data <- layout$finish_data(data)
#  
#  structure(
#    list(data = data, layout = layout, plot = plot),
#    class = "ggplot_built"
#  )
#}


#' @rdname ggplot_build
#' @export
layer_data <- function(plot, i = 1L) {
  ggplot_build(plot)$data[[i]]
}

# @export
# @rdname ggplot_build
#layer_scales <- function(plot, i = 1L, j = 1L) {
#  b <- ggplot_build(plot)
#  
#  layout <- b$panel$layout
#  selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]
#  
#  list(
#    x = b$panel$x_scales[[selected$SCALE_X]],
#    y = b$panel$y_scales[[selected$SCALE_Y]]
#  )
#}

# @export
# @rdname ggplot_build
#layer_grob <- function(plot, i = 1L) {
#  b <- ggplot_build(plot)
#  b$plot$layers[[i]]$draw_geom(b$data[[i]], b$panel, b$plot$coordinates)
#}

#' Build a plot with all the usual bits and pieces.
#'
#' This function builds all grobs necessary for displaying the plot, and
#' stores them in a special data structure called a \code{\link{gtable}}.
#' This object is amenable to programmatic manipulation, should you want
#' to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
#' a single display, preserving aspect ratios across the plots.
#'
#' @seealso \code{\link{print.ggplot}} and \code{link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @return a \code{\link{gtable}} object
#' @keywords internal
#' @param data plot data generated by \code{\link{ggplot_build}}
#' @rdname ggplot_gtable
#' @export
ggplot_gtable <- function(data) {
  # Attaching the plot env to be fetched by deprecations etc.
  ggint$attach_plot_env(data$plot$plot_env)

  UseMethod('ggplot_gtable')
}

#' @export
ggplot_gtable.ggplot_built <- function(data) {
  plot <- data$plot
  layout <- data$layout
  data <- data$data
  theme <- ggint$plot_theme(plot)

  # Is the current plot a ternary plot
  isTernary  <- inherits(plot$coordinates,'CoordTern') ##NH
  
  geom_grobs <- ggint$by_layer(function(l, d) l$draw_geom(d, layout), plot$layers, data, "converting geom to grob")
  
  if(!isTernary) ## NH This is in the base ggplot code, but causes error for ggtern
    layout$setup_panel_guides(plot$guides, plot$layers) #, plot$mapping) ## NH 21st Jul 2021
  
  plot_table <- layout$render(geom_grobs, data, theme, plot$labels)

  #NH For Ternary Plot.
  if(isTernary){
    latex      <- calc_element('tern.plot.latex', theme, verbose = FALSE)
    plot$labels<- lapply(plot$labels, function(x) label_formatter(x,latex = latex))
    plot_table <- plot$coordinates$remove_labels(plot_table)
  }

  # Legends
  legend_box <- plot$guides$assemble(theme)
  plot_table <- ggint$table_add_legends(plot_table, legend_box, theme)

  # Title
  title <- element_render(
    theme, "plot.title", plot$labels$title,
    margin_y = TRUE, margin_x = TRUE
  )
  title_height <- grobHeight(title)

  # Subtitle
  subtitle <- element_render(
    theme, "plot.subtitle", plot$labels$subtitle,
    margin_y = TRUE, margin_x = TRUE
  )
  subtitle_height <- grobHeight(subtitle)

  # whole plot annotation
  caption <- element_render(
    theme, "plot.caption", plot$labels$caption,
    margin_y = TRUE, margin_x = TRUE
  )
  caption_height <- grobHeight(caption)

  # positioning of title and subtitle is governed by plot.title.position
  # positioning of caption is governed by plot.caption.position
  #   "panel" means align to the panel(s)
  #   "plot" means align to the entire plot (except margins and tag)
  title_pos <- rlang::arg_match0(
    theme$plot.title.position %||% "panel",
    c("panel", "plot"),
    arg_nm = "plot.title.position",
    error_call = expr(theme())
  )

  caption_pos <- rlang::arg_match0(
    theme$plot.caption.position %||% "panel",
    values = c("panel", "plot"),
    arg_nm = "plot.caption.position",
    error_call = expr(theme())
  )

  pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), , drop = FALSE]
  if (title_pos == "panel") {
    title_l = min(pans$l)
    title_r = max(pans$r)
  } else {
    title_l = 1
    title_r = ncol(plot_table)
  }
  if (caption_pos == "panel") {
    caption_l = min(pans$l)
    caption_r = max(pans$r)
  } else {
    caption_l = 1
    caption_r = ncol(plot_table)
  }

  plot_table <- gtable_add_rows(plot_table, subtitle_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, subtitle, name = "subtitle",
    t = 1, b = 1, l = title_l, r = title_r, clip = "off")

  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, title, name = "title",
    t = 1, b = 1, l = title_l, r = title_r, clip = "off")

  plot_table <- gtable_add_rows(plot_table, caption_height, pos = -1)
  plot_table <- gtable_add_grob(plot_table, caption, name = "caption",
    t = -1, b = -1, l = caption_l, r = caption_r, clip = "off")

  plot_table <- ggint$table_add_tag(plot_table, plot$labels$tag, theme)

  # Margins
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], pos = 0)
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], pos = 0)

  if (inherits(theme$plot.background, "element")) {
    plot_table <- gtable_add_grob(plot_table,
      element_render(theme, "plot.background"),
      t = 1, l = 1, b = -1, r = -1, name = "background", z = -Inf)
    plot_table$layout <- plot_table$layout[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1)),]
    plot_table$grobs <- plot_table$grobs[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1))]
  }

  # add alt-text as attribute
  attr(plot_table, "alt-label") <- plot$labels$alt

  plot_table
}

#ggplot_gtable.ggplot_built <- function(data) {
#  plot <- data$plot
#  layout <- data$layout
#  data <- data$data
#  theme <- ggint$plot_theme(plot) #NH
#  
#  # Is the current plot a ternary plot
#  isTernary  <- inherits(plot$coordinates,'CoordTern') ##NH
#  
#  geom_grobs <- Map(function(l, d) l$draw_geom(d, layout), plot$layers, data)
#  if(!isTernary) ## NH This is in the base ggplot code, but causes error for ggtern
#    layout$setup_panel_guides(plot$guides, plot$layers, plot$mapping) ## NH 21st Jul 2021
#  plot_table <- layout$render(geom_grobs, data, theme, plot$labels)
#  
#  #NH For Ternary Plot.
#  if(isTernary){
#    latex      <- calc_element('tern.plot.latex', theme, verbose = FALSE)
#    plot$labels<- lapply(plot$labels, function(x) label_formatter(x,latex = latex))
#    plot_table <- plot$coordinates$remove_labels(plot_table)
#  }
#  
#  # Legends
#  position <- theme$legend.position %||% "right"
#  if (length(position) == 2) {
#    position <- "manual"
#  }
#  
#  legend_box <- if (position != "none") {
#    # ggint$build_guides(plot$scales, plot$layers, plot$mapping, position, theme, plot$guides, plot$labels) ##NH
#  } else {
#    zeroGrob()
#  }
#  
#  if (ggint$is.zero(legend_box)) { ##NH
#    position <- "none"
#  } else {
#    # these are a bad hack, since it modifies the contents of viewpoint directly...
#    legend_width  <- gtable_width(legend_box)
#    legend_height <- gtable_height(legend_box)
#    
#    # Set the justification of the legend box
#    # First value is xjust, second value is yjust
#    just <- valid.just(theme$legend.justification)
#    xjust <- just[1]
#    yjust <- just[2]
#    
#    if (position == "manual") {
#      xpos <- theme$legend.position[1]
#      ypos <- theme$legend.position[2]
#      
#      # x and y are specified via theme$legend.position (i.e., coords)
#      legend_box <- editGrob(legend_box,
#        vp = viewport(x = xpos, y = ypos, just = c(xjust, yjust),
#          height = legend_height, width = legend_width))
#    } else {
#      # x and y are adjusted using justification of legend box (i.e., theme$legend.justification)
#      legend_box <- editGrob(legend_box,
#        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust)))
#      legend_box <- gtable_add_rows(legend_box, unit(yjust, 'null'))
#      legend_box <- gtable_add_rows(legend_box, unit(1 - yjust, 'null'), 0)
#      legend_box <- gtable_add_cols(legend_box, unit(xjust, 'null'), 0)
#      legend_box <- gtable_add_cols(legend_box, unit(1 - xjust, 'null'))
#    }
#  }
#  
#  panel_dim <- find_panel(plot_table)
#  # for align-to-device, use this:
#  # panel_dim <-  summarise(plot_table$layout, t = min(t), r = max(r), b = max(b), l = min(l))
#  
#  theme$legend.box.spacing <- theme$legend.box.spacing %||% unit(0.2, 'cm')
#  if (position == "left") {
#    plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, pos = 0)
#    plot_table <- gtable_add_cols(plot_table, legend_width, pos = 0)
#    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
#      t = panel_dim$t, b = panel_dim$b, l = 1, r = 1, name = "guide-box")
#  } else if (position == "right") {
#    plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, pos = -1)
#    plot_table <- gtable_add_cols(plot_table, legend_width, pos = -1)
#    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
#      t = panel_dim$t, b = panel_dim$b, l = -1, r = -1, name = "guide-box")
#  } else if (position == "bottom") {
#    plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, pos = -1)
#    plot_table <- gtable_add_rows(plot_table, legend_height, pos = -1)
#    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
#      t = -1, b = -1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
#  } else if (position == "top") {
#    plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, pos = 0)
#    plot_table <- gtable_add_rows(plot_table, legend_height, pos = 0)
#    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
#      t = 1, b = 1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
#  } else if (position == "manual") {
#    # should guide box expand whole region or region without margin?
#    plot_table <- gtable_add_grob(plot_table, legend_box,
#      t = panel_dim$t, b = panel_dim$b, l = panel_dim$l, r = panel_dim$r,
#      clip = "off", name = "guide-box")
#  }
#  
#  # Title
#  title <- element_render(theme, "plot.title", plot$labels$title, margin_y = TRUE)
#  title_height <- grobHeight(title)
#  
#  # Subtitle
#  subtitle <- element_render(theme, "plot.subtitle", plot$labels$subtitle, margin_y = TRUE)
#  subtitle_height <- grobHeight(subtitle)
#  
#  # Tag
#  tag <- element_render(theme, "plot.tag", plot$labels$tag, margin_y = TRUE, margin_x = TRUE)
#  tag_height <- grobHeight(tag)
#  tag_width <- grobWidth(tag)
#  
#  # whole plot annotation
#  caption <- element_render(theme, "plot.caption", plot$labels$caption, expand_y = TRUE)
#  caption_height <- grobHeight(caption)
#  
#  pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), ,
#    drop = FALSE]
#  
#  plot_table <- gtable_add_rows(plot_table, subtitle_height, pos = 0)
#  plot_table <- gtable_add_grob(plot_table, subtitle, name = "subtitle",
#    t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")
#  
#  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
#  plot_table <- gtable_add_grob(plot_table, title, name = "title",
#    t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")
#  
#  plot_table <- gtable_add_rows(plot_table, caption_height, pos = -1)
#  plot_table <- gtable_add_grob(plot_table, caption, name = "caption",
#    t = -1, b = -1, l = min(pans$l), r = max(pans$r), clip = "off")
#  
#  plot_table <- gtable_add_rows(plot_table, unit(0, 'pt'), pos = 0)
#  plot_table <- gtable_add_cols(plot_table, unit(0, 'pt'), pos = 0)
#  plot_table <- gtable_add_rows(plot_table, unit(0, 'pt'), pos = -1)
#  plot_table <- gtable_add_cols(plot_table, unit(0, 'pt'), pos = -1)
#  
#  tag_pos <- theme$plot.tag.position %||% "topleft"
#  if (length(tag_pos) == 2) tag_pos <- "manual"
#  valid_pos <- c("topleft", "top", "topright", "left", "right", "bottomleft",
#                 "bottom", "bottomright")
#  
#  if (!(tag_pos == "manual" || tag_pos %in% valid_pos)) {
#    stop("plot.tag.position should be a coordinate or one of ",
#         paste(valid_pos, collapse = ', '), call. = FALSE)
#  }
#  
#  if (tag_pos == "manual") {
#    xpos <- theme$plot.tag.position[1]
#    ypos <- theme$plot.tag.position[2]
#    tag_parent <- ggint$justify_grobs(tag, x = xpos, y = ypos,
#                                hjust = theme$plot.tag$hjust,
#                                vjust = theme$plot.tag$vjust,
#                                int_angle = theme$plot.tag$angle,
#                                debug = theme$plot.tag$debug)
#    plot_table <- gtable_add_grob(plot_table, tag_parent, name = "tag", t = 1,
#                                  b = nrow(plot_table), l = 1,
#                                  r = ncol(plot_table), clip = "off")
#  } else {
#    # Widths and heights are reassembled below instead of assigning into them
#    # in order to avoid bug in grid 3.2 and below.
#    if (tag_pos == "topleft") {
#      plot_table$widths <- unit.c(tag_width, plot_table$widths[-1])
#      plot_table$heights <- unit.c(tag_height, plot_table$heights[-1])
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = 1, l = 1, clip = "off")
#    } else if (tag_pos == "top") {
#      plot_table$heights <- unit.c(tag_height, plot_table$heights[-1])
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = 1, l = 1, r = ncol(plot_table),
#                                    clip = "off")
#    } else if (tag_pos == "topright") {
#      plot_table$widths <- unit.c(plot_table$widths[-ncol(plot_table)], tag_width)
#      plot_table$heights <- unit.c(tag_height, plot_table$heights[-1])
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = 1, l = ncol(plot_table), clip = "off")
#    } else if (tag_pos == "left") {
#      plot_table$widths <- unit.c(tag_width, plot_table$widths[-1])
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = 1, b = nrow(plot_table), l = 1,
#                                    clip = "off")
#    } else if (tag_pos == "right") {
#      plot_table$widths <- unit.c(plot_table$widths[-ncol(plot_table)], tag_width)
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = 1, b = nrow(plot_table), l = ncol(plot_table),
#                                    clip = "off")
#    } else if (tag_pos == "bottomleft") {
#      plot_table$widths <- unit.c(tag_width, plot_table$widths[-1])
#      plot_table$heights <- unit.c(plot_table$heights[-nrow(plot_table)], tag_height)
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = nrow(plot_table), l = 1, clip = "off")
#    } else if (tag_pos == "bottom") {
#      plot_table$heights <- unit.c(plot_table$heights[-nrow(plot_table)], tag_height)
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = nrow(plot_table), l = 1, r = ncol(plot_table), clip = "off")
#    } else if (tag_pos == "bottomright") {
#      plot_table$widths <- unit.c(plot_table$widths[-ncol(plot_table)], tag_width)
#      plot_table$heights <- unit.c(plot_table$heights[-nrow(plot_table)], tag_height)
#      plot_table <- gtable_add_grob(plot_table, tag, name = "tag",
#                                    t = nrow(plot_table), l = ncol(plot_table), clip = "off")
#    }
#  }
#  
#  # Margins
#  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], pos = 0)
#  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
#  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
#  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], pos = 0)
#  
#  if (inherits(theme$plot.background, "element")) {
#    plot_table <- gtable_add_grob(plot_table,
#      element_render(theme, "plot.background"),
#      t = 1, l = 1, b = -1, r = -1, name = "background", z = -Inf)
#    plot_table$layout <- plot_table$layout[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1)),]
#    plot_table$grobs <- plot_table$grobs[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1))]
#  }
#  plot_table
#}

#' Generate a ggplot2 plot grob (ggtern version)
#'
#' @param x ggplot2 object
#' @rdname ggplot_gtable
#' @export
ggplotGrob <- function(x) {
  ggplot_gtable(ggplot_build(x))
}
