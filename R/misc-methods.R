##############################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param package.list    A list of character strings specifying
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
#' @rdname loadpackages-method
#' 
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{pkgs <- list("ggplot2", "lme4")}
#' \dontrun{load.packages(pkgs) # loads packages if installed}
#' \dontrun{load.packages(pkgs, install=TRUE) # loads packages after installation (if needed)}
setGeneric("load.packages", function(package.list, install, quiet) {
    standardGeneric("load.packages")
})

#' @rdname loadpackages-method
setMethod("load.packages",
          signature(package.list="list", install="logical", quiet="logical"),
          definition = function(package.list, install, quiet) {
              load <- function(name, install) {
                  if (!require(name, character.only=TRUE)) {
                      if (install) {
                          install.packages(name, repos="http://cran.r-project.org")
                          library(name, character.only=TRUE)
                      } else {
                          print(paste("Warning: unable to load package ", name, ". Is it installed?", sep=""))
                      }
                  }
              }
              lapply(package.list, load, install)
              if (!quiet) print(paste("Loaded", length(package.list), "packages.", sep=" "))
})

#' @rdname loadpackages-method
setMethod("load.packages",
          signature(package.list="list", install="missing", quiet="missing"),
          definition = function(package.list) {
              load.packages(package.list=package.list, install=FALSE, quiet=FALSE)
})

#' @rdname loadpackages-method
setMethod("load.packages",
          signature(package.list="list", install="missing", quiet="logical"),
          definition = function(package.list, quiet) {
              load.packages(package.list=package.list, install=FALSE, quiet=quiet)
})

#' @rdname loadpackages-method
setMethod("load.packages",
          signature(package.list="list", install="logical", quiet="missing"),
          definition = function(package.list, install) {
              load.packages(package.list=package.list, install=install, quiet=FALSE)
})

##############################################################
#' Check filepath.
#'
#' Checks the specified filepath for formatting consistencies, 
#' such as trailing slashes, etc.
#'
#' @param path A character string corresponding to a filepath.
#' 
#' @param create A logical indicating whether the path should
#' be created if it doesn't exist. Default is \code{FALSE}.
#'
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
checkPath = function(path, create=FALSE) {
    if (is.character(path)) {
        # check if path has a trailing slash and remove it
        strlets <- strsplit(path, "")[[1]]
        strlen <- length(strlets)
        if (strlets[strlen]=="/") {
            path <- cat("\"",strlets[-strlen], "\"", sep="")
        } else {}
        
        if (file.exists(path)) {
            exists = TRUE # basically, do nothing if it exists
        } else {
            print("Warning: the specified path doesn't exist...")
            if (create==TRUE) {
                print(paste("...creating directory structure:", path))
                dir.create(file.path(path), recursive=TRUE, showWarnings=FALSE)
            } else {
                print("...please create it and try again.")
            }
        }
    } else {
        stop("Error: `path` should be specified as a character string.")
    }
    return(path)
}
