#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  options(spades.modulesRepo = "PredictiveEcology/SpaDES-modules")
  options(spades.lowMemory = FALSE)
}

.onUnload <- function(libname, pkgname) {
  options(spades.modulesRepo = NULL)
  options(spades.lowMemory = NULL)
}

