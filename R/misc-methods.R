##############################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param packageList    A list of character strings specifying
#'                        the names of packages to be loaded.
#'
#' @param install         Logical flag. If required packages are not
#'                        already installed, should they be installed?
#'
#' @param quiet           Logical flag. Should the final "packages loaded"
#'                        message be suppressed?
#'
#' @return Nothing is returned. Specified packages are loaded and attached using \code{library()}.
#'
#' @seealso \code{\link{library}}.
#'
#' @export
#' @docType methods
#' @rdname loadPackages-method
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{pkgs <- list("ggplot2", "lme4")}
#' \dontrun{loadPackages(pkgs) # loads packages if installed}
#' \dontrun{loadPackages(pkgs, install=TRUE) # loads packages after installation (if needed)}
setGeneric("loadPackages", function(packageList, install=FALSE, quiet=TRUE) {
    standardGeneric("loadPackages")
})

#' @rdname loadPackages-method
setMethod("loadPackages",
          signature="list",
          definition = function(packageList, install, quiet) {
              load <- function(name, install) {
                  if (!require(name, character.only=TRUE)) {
                      if (install) {
                          install.packages(name, repos="http://cran.r-project.org")
                          library(name, character.only=TRUE)
                      } else {
                          warning(paste("Warning: unable to load package ", name, ". Is it installed?", sep=""))
                      }
                  }
              }
              lapply(packageList, load, install)
              if (!quiet) print(paste("Loaded", length(packageList), "packages.", sep=" "))
})

##############################################################
#' Check filepath.
#'
#' Checks the specified filepath for formatting consistencies,
#' such as leading/trailing slashes, etc.
#'
#' @param path      A character string corresponding to a filepath.
#'
#' @param relative  A logical indicating whether return \code{path}
#'                  is relative (\code{TRUE}) or absolute (\code{FALSE}).
#'                  Default \code{TRUE} to return relative path.
#'
#' @param create    A logical indicating whether the path should be
#'                  created if it doesn't exist. Default is \code{FALSE}.

#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso \code{\link{file.exists}}, \code{\link{dir.create}}.
#'
#' @export
#' @docType methods
#' @rdname checkpath
#'
# @examples
# need examples
checkPath = function(path, relative=TRUE, create=FALSE) {
  if (is.character(path)) {
    path = gsub("\\\\", "/", path)    # use slash instead of backslash
    path = gsub("/$", "", path)       # remove trailing slash

    if (relative==TRUE) {
      path = gsub("^[.]/", "", path)  # remove leading dotslash

      # if user provided absolute path, make it relative to .data.dir
      if (identical(path, normalizePath(path, winslash="/"))) {
        path = gsub(getwd(), "", path)
        path = gsub("^/", "", path)
      }
      full.path = file.path(getwd(), path)
    } else {
      full.path = normalizePath(path, winslash="/")
    }

    if (!file.exists(full.path)) {
      if (create==TRUE) {
        dir.create(file.path(full.path), recursive=TRUE, showWarnings=FALSE)
      } else {
        warning("Specified path doesn't exist. Create it and try again.")
      }
    }
  } else {
    stop("Specify `path` as a character string.")
  }
  return(path)
}

