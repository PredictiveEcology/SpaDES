#' @section 6 Package options:
#'
#' \code{SpaDES} uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{spades.cachePath}: The default local directory in which to
#'   cache simulation outputs.
#'   Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/cache}).
#'
#'   \item \code{spades.inputPath}: The default local directory in which to
#'   look for simulation inputs.
#'   Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/inputs}).
#'
#'   \item \code{spades.lowMemory}: If true, some functions will use more memory
#'     efficient (but slower) algorithms. Default \code{FALSE}.
#'
#'   \item \code{spades.modulePath}: The default local directory where modules
#'     and data will be downloaded and stored.
#'     Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/modules}).
#'
#'   \item \code{spades.moduleRepo}: The default GitHub repository to use when
#'     downloading modules. Default \code{"PredictiveEcology/SpaDES-modules"}.
#'
#'   \item \code{spades.nCompleted}: The maximum number of completed events to
#'     retain in the \code{completed} event queue. Default \code{1000L}.
#'
#'   \item \code{spades.outputPath}: The default local directory in which to
#'   save simulation outputs.
#'   Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/outputs}).
#'
#'   \item \code{spades.tolerance}: The default tolerance value used for floating
#'     point number comparisons. Default \code{.Machine$double.eps^0.5}.
#'
#'   \item \code{spades.useragent}: The default user agent to use for downloading
#'     modules from GitHub.com. Default \code{"http://github.com/PredictiveEcology/SpaDES"}.
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
    spades.tolerance = .Machine$double.eps ^ 0.5,
    spades.useragent = "http://github.com/PredictiveEcology/SpaDES"
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES version ", packageVersion("SpaDES"), ".")
    packageStartupMessage("Default paths for SpaDES directories set to:\n",
                          "  cachePath:  ", getOption("spades.cachePath"), "\n",
                          "  inputPath:  ", getOption("spades.inputPath"), "\n",
                          "  modulePath: ", getOption("spades.modulePath"), "\n",
                          "  outputPath: ", getOption("spades.outputPath"), "\n",
                          "These can be changed using 'setPaths()'. See '?setPaths'.")
  }
}

.onUnload <- function(libpath) {
  ## if temp session dir is being used, ensure it gets reset each session
  tmpdir <- file.path(tempdir(), "SpaDES")
  if (getOption("spades.cachePath") == file.path(tmpdir, "cache")) {
    options(spades.cachePath = NULL)
  }
  if (getOption("spades.inputPath") == file.path(tmpdir, "inputs")) {
    options(spades.inputPath = NULL)
  }
  if (getOption("spades.modulePath") == file.path(tmpdir, "modules")) {
    options(spades.modulePath = NULL)
  }
  if (getOption("spades.outputPath") == file.path(tmpdir, "outputs")) {
    options(spades.outputPath = NULL)
  }
}
