#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  options(spades.modulesRepo = "PredictiveEcology/SpaDES-modules")
}

.onUnload <- function(libname, pkgname) {
  options(spades.modulesRepo = NULL)
}
