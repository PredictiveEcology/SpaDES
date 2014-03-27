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
# @examples
# pkgs <- list("ggplot2", "lme4")
# load.packages(pkgs) # loads packages if installed
# load.packages(pkgs, install=TRUE) # loads packages after installation (if needed)
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

# check whether a module should be reloaded later
setGeneric("reload.module.later", function(depends, ...) {
    standardGeneric("reload.module.later")
})

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

##########################################################################
# where should this go? at this point it needs to be run at package load
##########################################################################
pkgs <- list("CircStats",
             "data.table",
             "geoR",
             "igraph",
             "methods",
             "plotrix",
             "raster",
             "RandomFields",
             "sp")
load.packages(pkgs)
