.onLoad <- function(libname, pkgname){

  #Set the options
  .setOptionsCurrent()
  .setOptionsDepreciated()
  
  #Set the theme and the last coordinates.
  theme_set(theme_gray())
}

.onAttach <- function(libname, pkgname){
  lines = c(sprintf("If '%s' has been of assistance, please donate at http://ggtern.com -- even small amounts (say $10-50) are very much appreciated!",pkgname),
            sprintf("Please remember to cite if used within books, publications etc..., Run citation('%s') for further information.",pkgname))
  msg = paste(lines,collapse="\n")
  packageStartupMessage(msg)
}

#------------------------------------------------------------------------------
#CURRENT OPTIONS
#------------------------------------------------------------------------------
.setOptionsCurrent <- function(){
  options("tern.expand"                = 0.2)
  options('tern.margin'                = unit(0,'pt'))
  options('tern.arrow'                 = arrow(length=unit(2.5,"mm")))
  options("tern.default.T"             = "y")
  options("tern.default.L"             = "x")
  options("tern.default.R"             = "z")
  options("tern.clockwise"             = FALSE)
  options("tern.title.show"            = TRUE)
  options("tern.text.show"             = TRUE)
  options("tern.axis.ontop"            = FALSE)
  options("tern.arrow.start"           = 0.3)
  options("tern.arrow.finish"          = 0.7)
  options("tern.arrow.show"            = FALSE)
  options('tern.arrow.sep'             = 0.1)
  options('tern.vshift'                = 0.0)
  options('tern.hshift'                = 0.0)
  options("tern.ticks.outside"         = TRUE)
  options("tern.ticks.primary.show"    = TRUE)
  options("tern.ticks.secondary.show"  = FALSE)
  options("tern.breaks.default"        = seq(0.0, 1.0,by=0.2))
  options("tern.breaks.default.minor"  = seq(0.1, 0.9,by=0.2))
  options("tern.grid.major.show"       = TRUE)
  options("tern.grid.minor.show"       = TRUE)
  options("tern.grid.ontop"            = FALSE)
  options("tern.mask.show"             = TRUE)
  options("tern.rotate"                = 0)
}

#------------------------------------------------------------------------------
#DEPRECIATED OPTIONS -- ie either not used anymore or in depreciated functions.
#------------------------------------------------------------------------------
.setOptionsDepreciated <- function(){
  options("tern.discard.external"      = TRUE)
  options("tern.expand.contour.inner"  =-0.0005)
  options("tern.dont_transform"        = FALSE)
  options("tern.mesh.buffer"           = 1.50)
  options("tern.mesh.size"             = 200)
}
