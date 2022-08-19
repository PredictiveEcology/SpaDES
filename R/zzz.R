#' The `SpaDES` package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
#'
.pkgEnv <- new.env(parent = emptyenv())

#' Check if a package is in the search path
#'
#' @keywords internal
#' @rdname isAttached
.isAttached <- function(x) {
  paste0("package:", x) %in% search()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  pkgs <- c("reproducible", "quickPlot", "SpaDES.core", "SpaDES.tools")
            # "SpaDES.experiment", "SpaDES.addins", "SpaDES.shiny")

  needed <- pkgs[!.isAttached(pkgs)]
  assign("needed", value = needed, envir = .pkgEnv)

  out <- NULL

  if (length(needed) > 0) {
    suppressPackageStartupMessages({
      out <- lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
    })
  }

  .vers <- vapply(pkgs, function(x) {
    as.character(packageVersion(x))
  }, character(1))

  packageLoading <- paste0(c("using ", "loading ")[(names(.vers) %in% needed) + 1], names(.vers))

  widths <- vapply(packageLoading, nchar, numeric(1))
  maxWidth <- max(widths) + 4
  spaces <- vapply(maxWidth - widths, function(w) {
    paste0(rep(" ", w), collapse = "")
  }, character(1))


  pkgInfo <- paste0(packageLoading, spaces, .vers, collapse = "\n")

  packageStartupMessage(pkgInfo, "\n")

  packageStartupMessage("Default paths for SpaDES directories set to:\n",
                        "  cachePath:  ", getOption("spades.cachePath"), "\n",
                        "  inputPath:  ", getOption("spades.inputPath"), "\n",
                        "  modulePath: ", getOption("spades.modulePath"), "\n",
                        "  outputPath: ", getOption("spades.outputPath"), "\n",
                        "These can be changed using 'setPaths()'. See '?setPaths'.")

  return(invisible(out))
}
