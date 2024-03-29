% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-build.R
\name{ggplot_gtable}
\alias{ggplot_gtable}
\alias{ggplotGrob}
\title{Build a plot with all the usual bits and pieces.}
\usage{
ggplot_gtable(data)

ggplotGrob(x)
}
\arguments{
\item{data}{plot data generated by \code{\link{ggplot_build}}}

\item{x}{ggplot2 object}
}
\value{
a \code{\link{gtable}} object
}
\description{
This function builds all grobs necessary for displaying the plot, and
stores them in a special data structure called a \code{\link{gtable}}.
This object is amenable to programmatic manipulation, should you want
to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
a single display, preserving aspect ratios across the plots.
}
\seealso{
\code{\link{print.ggplot}} and \code{link{benchplot}} for
 for functions that contain the complete set of steps for generating
 a ggplot2 plot.
}
\keyword{internal}
