#' Create a new ggplot plot.
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to
#' declare the input data frame for a graphic and to specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' \code{ggplot()} is typically used to construct a plot
#' incrementally, using the + operator to add layers to the
#' existing ggplot object. This is advantageous in that the
#' code is explicit about which layers are added and the order
#' in which they are added. For complex graphics with multiple
#' layers, initialization with \code{ggplot} is recommended.
#'
#' There are three common ways to invoke \code{ggplot}:
#' \itemize{
#'    \item \code{ggplot(df, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(df)}
#'    \item \code{ggplot()}
#'   }
#' The first method is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used to add a layer using data from another
#' data frame. See the first example below. The second
#' method specifies the default data frame to use for the plot,
#' but no aesthetics are defined up front. This is useful when
#' one data frame is used predominantly as layers are added, 
#' but the aesthetics may vary from one layer to another. The
#' third method initializes a skeleton \code{ggplot} object which
#' is fleshed out as layers are added. This method is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
#' @inheritParams ggplot2::ggplot
#' @author Nicholas Hamilton
#' @rdname ggplot
#' @export
ggplot <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggplot")
}

#' @export
#' @rdname ggplot
#' @usage NULL
ggplot.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {

  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    #cli::cli_abort(c(
    #  "{.arg mapping} must be created with {.fn aes}.",
    #  "x" = "You've supplied {.obj_type_friendly {mapping}}."
    #))
    stop("Mapping should be created with aes or aes_string")
  }
  
  data = fortify(data, ...)
  
  p <- structure(list(
    data = data,
    layers = list(),
    scales = ggint$scales_list(),
    guides = ggint$guides_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(default = TRUE),
    facet = facet_null(),
    plot_env = environment,
    layout = ggproto(NULL, Layout)
  ), class = c("gg", "ggplot"))
  
  p$labels <- ggint$make_labels(mapping) #NH
  
  ggint$set_last_plot(p) ##NH
  
  p
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @return Invisibly returns the result of \code{\link{ggplot_build}}, which
#'   is a list with components that contain the plot itself, the data,
#'   information about the scales, panels etc.
#' @export
#' @rdname ggplot
#' @method print ggplot
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ggint$set_last_plot(x) ##NH
  if (newpage) grid.newpage()
  
  # Record dependency on 'ggplot2' on the display list
  # (AFTER grid.newpage())
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )

  data <- ggplot_build(x)
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }
  
  if (isTRUE(getOption("BrailleR.VI")) && rlang::is_installed("BrailleR")) {
    print(asNamespace("BrailleR")$VI(x))
  }
  
  invisible(x)
}

#' @rdname ggplot
#' @method plot ggplot
#' @export
plot.ggplot <- print.ggplot
