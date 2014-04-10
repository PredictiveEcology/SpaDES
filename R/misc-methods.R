##############################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param package.list A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#'  already installed, should they be installed?  
#'
#' @return Nothing is returned. Specified packages are loaded
#'  and attached using \code{library()}.
#' 
#' @seealso \code{\link{library}}.
#' 
#' @export
#' @docType methods
#' @rdname loadpackages
#'
#' @examples
#' \dontrun{pkgs <- list("ggplot2", "lme4")}
#' \dontrun{load.packages(pkgs) # loads packages if installed}
#' \dontrun{load.packages(pkgs, install=TRUE) # loads packages after installation (if needed)}
setGeneric("load.packages", function(package.list, install) {
    standardGeneric("load.packages")
})

#' @rdname loadpackages
setMethod("load.packages",
          signature(package.list="list", install="logical"),
          definition = function(package.list, install) {
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
})

#' @rdname loadpackages
setMethod("load.packages",
          signature(package.list="list", install="missing"),
          definition = function(package.list) {
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
              lapply(package.list, load, install=FALSE)
})



##############################################################
#' Load modules for simulation.
#'
#' Checks the dependencies of the current module on other modules.
#' These dependencies need to be loaded first, so if they are not
#' already loaded, hold off loading the cuurent module until after
#' dependencies are loaded.
#'
#' @param depends A list of character strings specifying
#' the names of modules upon which the current module depends.
#'
#' @return \code{TRUE}/\code{FALSE}.
#' 
#' @seealso \code{\link{library}}.
#' 
#' @export
#' @docType methods
#' @rdname loadmodules
#'
# @examples
# need examples
setGeneric("reload.module.later", function(depends, ...) {
    standardGeneric("reload.module.later")
})

#' @rdname loadmodules
setMethod("reload.module.later",
          signature(depends="character"),
          definition = function(depends, ...) {
              if (depends=="NONE") {
                  return(FALSE)
              } else {
                  f = all(depends %in% sim.loaded(sim))
                  return(!f)
              }
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
#' @seealso \code{\link{file.exists}}, \code{\link{create.dir}}.
#' 
#' @export
#' @docType methods
#' @rdname checkpath
#'
# @examples
# need examples
check.path = function(path, create=FALSE) {
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
