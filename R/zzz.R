#' @importFrom methods loadMethod
#' @include numerical-comparisons.R
#' @include environment.R
#'
.onLoad <- function(libname, pkgname) {
  setTolerance()
}

#
#.onUnload <- function(libname, pkgname) {
#  rm(stuff, envir=.GlobalEnv)
#}
