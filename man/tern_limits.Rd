% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tern-limits.R
\name{tern_limits}
\alias{tern_limits}
\alias{tern_limit}
\alias{limit_tern}
\alias{limits_tern}
\title{Restrict Ternary Limits}
\usage{
tern_limit(T = 1, L = 1, R = 1, ...)

limit_tern(...)
}
\arguments{
\item{T, L, R}{numeric value (scalar) of the maximum \code{T,L,R} species limit for each scale respectively}

\item{...}{other arguments to pass to ALL of \code{scale_X_continuous} (\code{X = T, L, R})}
}
\value{
Either an empty list (when no solution can be found), or a list containing one of each 
of \code{scale_X_continuous} (\code{X = T, L, R})
}
\description{
\code{tern_limits} (or its aliasses) appends new \code{T}, \code{L} and \code{R} ternary continuous scales, 
where the maximum scale value is specified, and, where the minimums for each are solved.
}
\details{
The contra value (ie minimum value) for the \code{T}, \code{L} and \code{R} species is solved using
linear equations, therefore, if the solution is degenerate, or, the solution results in a zero range in either
of the proposed scales, then a warning message will be reported and an empty list returned. Note that 
\code{limits_tern(\dots), limit_tern(\dots)} and \code{tern_limit(\dots)} are all aliasses for 
the main function, \code{tern_limits(\dots)} and can be used interchangeably.
}
\examples{
#Display a non-zoomed and zoomed plot side by side
data(Feldspar)
df.lims = data.frame(Ab = c(1,.25,.25), 
                     An = c(0,.75,.00), 
                     Or = c(0,.00,.75))
#Build the non-zoomed plot
A = ggtern(Feldspar,aes(Ab,An,Or)) +
 stat_density_tern(geom='polygon',aes(fill=..level..,alpha=..level..)) + 
 geom_point() + 
 geom_mask() + 
 geom_polygon(data=df.lims,color='red',alpha=0,size=0.5) +
 guides(color='none',fill='none',alpha='none') + 
 labs(title = "Non-Zoomed")

#Build the zoomed plot
B = A + 
  tern_limits(T=max(df.lims$An), L=max(df.lims$Ab), R=max(df.lims$Or)) +
  labs(title = "Zoomed")

#Arrange the above plots side by side for illustration
grid.arrange(A,B,ncol=2,top="Demonstration of Limiting Region")
}
\seealso{
\code{\link{scale_T_continuous}}, \code{\link{scale_L_continuous}} and \code{\link{scale_R_continuous}}
}
\author{
Nicholas Hamilton
}
