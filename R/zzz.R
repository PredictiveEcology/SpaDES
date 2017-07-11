if (getRversion() >= "3.1.0") {
  utils::globalVariables(".pkgEnv")
}

#' Core SpaDES packages
#'
#' Packages of the \code{SpaDES} ecosystem.
#'
#' @keywords internal
#' @rdname spades-packages
.pkgs <- c("reproducible", "quickPlot",
           "SpaDES.core", "SpaDES.tools", "SpaDES.addins", "SpaDES.shiny")

#' The \code{SpaDES} package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
#'
.pkgEnv <- new.env(parent = emptyenv())

#' Check if a package is in the search path
#'
#' @importFrom utils packageVersion
#' @keywords internal
#' @rdname isAttached
.isAttached <- function(x) {
  paste0("package:", x) %in% search()
}

.onAttach <- function(libname, pkgname) {
  needed <- .pkgs[!.isAttached(.pkgs)]
  assign("needed", value = needed, envir = .pkgEnv)

  out <- NULL

  if (length(needed) > 0) {
    suppressPackageStartupMessages(
      out <- lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
    )
  }

  .vers <- vapply(.pkgs, function(x) {
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

# .onDetach <- function(libpath) {
#   for (p in rev(get("needed", envir = .pkgEnv))) {
#     tryCatch(detach(paste0("package:", p), character.only = TRUE, unload = TRUE),
#              error = function(x) NULL)
#   }
#
#   return(invisible())
# }
