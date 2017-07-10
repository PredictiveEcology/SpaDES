#' Core SpaDES packages
#'
#' Packages of the \code{SpaDES} ecosystem.
#'
#' @keywords internal
#' @rdname spades-packages
.pkgs <- c("reproducible", "quickPlot",
           "SpaDES.core", "SpaDES.tools", "SpaDES.addins", "SpaDES.shiny")

#' Check if a package is in the search path
#'
#' @keywords internal
#' @rdname isAttached
.isAttached <- function(x) {
  paste0("package:", x) %in% search()
}

.onAttach <- function(libname, pkgname) {
  needed <- .pkgs[!.isAttached(.pkgs)]
  out <- NULL

  if (length(needed) > 0) {
    suppressPackageStartupMessages(
      out <- lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
    )
  }

  .vers <- vapply(.pkgs, function(x) {
    as.character(utils::packageVersion(x))
  }, character(1))

  widths <- vapply(names(.vers), nchar, numeric(1))
  maxWidth <- max(widths) + 4
  spaces <- vapply(maxWidth - widths, function(w) {
    paste0(rep(" ", w), collapse = "")
  }, character(1))
  pkgInfo <- paste0("using ", names(.vers), spaces, .vers, collapse = "\n")

  packageStartupMessage(pkgInfo, "\n")

  packageStartupMessage("Default paths for SpaDES directories set to:\n",
                        "  cachePath:  ", getOption("spades.cachePath"), "\n",
                        "  inputPath:  ", getOption("spades.inputPath"), "\n",
                        "  modulePath: ", getOption("spades.modulePath"), "\n",
                        "  outputPath: ", getOption("spades.outputPath"), "\n",
                        "These can be changed using 'setPaths()'. See '?setPaths'.")

  return(invisible(out))
}
