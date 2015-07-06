#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  options(spades.lowMemory = FALSE)
  options(spades.modulesRepo = "PredictiveEcology/SpaDES-modules")
  options(spades.nCompleted = 1000L)
  options(spades.tolerance = .Machine$double.eps^0.5)
}

.onUnload <- function(libname, pkgname) {
  options(spades.lowMemory = NULL)
  options(spades.modulesRepo = NULL)
  options(spades.nCompleted = NULL)
  options(spades.tolerance = NULL)
}

