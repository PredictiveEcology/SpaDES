#' @section 6 Package options:
#'
#' \code{SpaDES} uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{spades.cachePath}: The default local directory in which to
#'   cache simulation outputs.
#'   Default is a temporary directory (typically \code{/tmp/SpaDES/cache}).
#'
#'   \item \code{spades.inputPath}: The default local directory in which to
#'   look for simulation inputs.
#'   Default is a temporary directory (typically \code{/tmp/SpaDES/inputs}).
#'
#'   \item \code{spades.lowMemory}: If true, some functions will use more memory
#'     efficient (but slower) algorithms. Default \code{FALSE}.
#'
#'   \item \code{spades.modulePath}: The default local directory where modules
#'     and data will be downloaded and stored.
#'     Default is a temporary directory (typically \code{/tmp/SpaDES/modules}).
#'
#'   \item \code{spades.moduleRepo}: The default GitHub repository to use when
#'     downloading modules. Default \code{"PredictiveEcology/SpaDES-modules"}.
#'
#'   \item \code{spades.nCompleted}: The maximum number of completed events to
#'     retain in the \code{completed} event queue. Default \code{1000L}.
#'
#'   \item \code{spades.outputPath}: The default local directory in which to
#'   save simulation outputs.
#'   Default is a temporary directory (typically \code{/tmp/SpaDES/outputs}).
#'
#'   \item \code{spades.tolerance}: The default tolerance value used for floating
#'     point number comparisons. Default \code{.Machine$double.eps^0.5}.
#'
#' }
#' @docType package
#' @name SpaDES-package
#'
NULL

.onLoad <- function(libname, pkgname) {
  tmpdir <- file.path(tempdir(), "SpaDES")
  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list(
    spades.cachePath = file.path(tmpdir, "cache"),
    spades.inputPath = file.path(tmpdir, "inputs"),
    spades.lowMemory = FALSE,
    spades.modulePath = file.path(tmpdir, "modules"),
    spades.moduleRepo = "PredictiveEcology/SpaDES-modules",
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(tmpdir, "outputs"),
    spades.tolerance = .Machine$double.eps^0.5
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Default paths for SpaDES directories set to:\n",
                        "  cachePath:  ", getOption("spades.cachePath"), "\n",
                        "  inputPath:  ", getOption("spades.inputPath"), "\n",
                        "  modulePath: ", getOption("spades.modulePath"), "\n",
                        "  outputPath: ", getOption("spades.outputPath"), "\n",
                        "These can be changed using 'setPaths()'. See '?setPaths'.")
}

.onUnload <- function(libpath) {
  tmpdir <- file.path(tempdir(), "SpaDES")
  if (getOption("spades.modulePath") == file.path(tmpdir)) {
    options(spades.modulePath = NULL)
  }
}
