#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  options(spades.lowMemory = FALSE)
  options(spades.modulesRepo = "PredictiveEcology/SpaDES-modules")
  options(spades.nCompleted = 10L)
}

.onUnload <- function(libname, pkgname) {
  options(spades.lowMemory = NULL)
  options(spades.modulesRepo = NULL)
  options(spades.nCompleted = NULL)
}

