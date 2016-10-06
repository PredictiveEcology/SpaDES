#' @section 6 Package options:
#'
#' \code{SpaDES} uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{spades.lowMemory}: If true, some functions will use more memory
#'     efficient (but slower) algorithms. Default \code{FALSE}.
#'
#'   \item \code{spades.modulesPath}: The default local directory where modules
#'     and data will be downloaded and stored.  Default is a temporary directory.
#'
#'   \item \code{spades.modulesRepo}: The default GitHub repository to use when
#'     downloading modules. Default \code{"PredictiveEcology/SpaDES-modules"}.
#'
#'   \item \code{spades.nCompleted}: The maximum number of completed events to
#'     retain in the \code{completed} event queue. Default \code{1000L}.
#'
#'   \item \code{spades.tolerance}: The default tolerance value used for floating
#'     point number comparisons. Default \code{.Machine$double.eps^0.5}.
#'
#' }
#' @docType package
#' @name SpaDES-package
#'
NULL

#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list(
    spades.lowMemory = FALSE,
    spades.modulesPath = file.path(tempdir(), "SpaDES_modules"),
    spades.modulesRepo = "PredictiveEcology/SpaDES-modules",
    spades.nCompleted = 1000L,
    spades.tolerance = .Machine$double.eps^0.5
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Default path for SpaDES modules set to: ",
                        getOption("spades.modulesPath"), ".\n",
                        "To change this, set the 'spades.modulesPath' option.")
}

.onUnload <- function(libpath) {
  if (getOption("spades.modulesPath") == file.path(tempdir(), "SpaDES_modules")) {
    options(spades.modulesPath = NULL)
  }
}
