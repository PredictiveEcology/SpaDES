##############################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param packageList A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#' already installed, should they be installed?
#'
#' @param quiet Logical flag. Should the final "packages loaded"
#' message be suppressed?
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
#'
setGeneric("loadPackages", function(packageList, install=FALSE, quiet=TRUE) {
  standardGeneric("loadPackages")
})

#' @rdname loadPackages-method
setMethod("loadPackages",
           signature="list",
            definition=function(packageList, install, quiet) {
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
              if (!quiet) message(paste("Loaded", length(packageList), "packages.", sep=" "))
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
#' @rdname checkPath-method
#'
# @examples
# need examples
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @rdname checkPath-method
setMethod("checkPath",
          signature(path="character", create="logical"),
          definition=function(path, create) {
            if (!is.na(path)) {
              if (length(path)>0) {
                path = gsub("\\\\", "/", path)  # use slash instead of backslash
                path = gsub("/$", "", path)     # remove trailing slash
                path = gsub("^[.]/", "", path)  # remove leading dotslash

                if (!file.exists(path)) {
                  if (create==TRUE) {
                    dir.create(file.path(path), recursive=TRUE, showWarnings=FALSE)
                  } else {
                    stop(paste("Specified path", normalizePath(path, winslash="/"),
                               "doesn't exist. Create it and try again."))
                  }
                }
              return(path)
            } else {
              stop("Invalid path: cannot be empty.")
            }
          } else {
            stop("Invalid path: cannot be NA.")
          }
})

#' @rdname checkPath-method
setMethod("checkPath",
          signature(path="character", create="missing"),
          definition=function(path) {
            return(checkPath(path, create=FALSE))
})

#' @rdname checkPath-method
setMethod("checkPath",
          signature(path="NULL", create="ANY"),
          definition=function(path) {
            stop("Invalid path: cannot be NULL.")
})

##############################################################
#' Check for existence of a global object
#'
#' Check that a named object exists in the global environment, and optionally has
#' desired attributes.
#'
#' @param name    A character string specifying the name of an object to be checked.
#'
#' @param object  An object. This is mostly used internally, or with layer,
#'                  because it will fail if the object does not exist.
#'
#' @param layer   Character string, specifying a layer name in a Raster, if the
#'                \code{name} is a Raster* object.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return Invisibly return \code{TRUE} indicating object exists; code{FALSE} if not.
#'
#' @seealso \code{\link{library}}.
#'
#' @export
#' @docType methods
#' @rdname checkObject-method
#'
#' @author Alex Chubaty
#' @author Eliot McIntire
#'
setGeneric("checkObject", function(name, object, layer, ...) {
  standardGeneric("checkObject")
})


#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="missing", object="Raster", layer="character"),
          definition = function(name, object, layer, ...) {
              if (!is.na(match(layer, names(object)))) {
                return(invisible(TRUE))
              } else {
                warning(paste(deparse(substitute(object,env=.GlobalEnv)),"exists, but",layer,"is not a layer"))
                return(FALSE)
              }
})

#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="missing", object="ANY", layer="missing"),
          definition = function(name, object, layer, ...) {
            if(exists(deparse(substitute(object)),envir=.GlobalEnv)) {
              return(invisible(TRUE))
            } else {
              warning(paste(deparse(substitute(object,env=.GlobalEnv)),"does not exist"))
              return(FALSE)
            }
})


#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="character", object="missing", layer="missing"),
          definition = function(name, ...) {
            if (exists(name, envir=.GlobalEnv)) {
                return(invisible(TRUE))
            } else {
              warning(paste(name,"does not exist in the global environment"))
              return(FALSE)
            }
})

#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="character", object="missing", layer="character"),
          definition = function(name, layer, ...) {
            if (exists(name, envir=.GlobalEnv)) {
              if(is(get(name, envir=.GlobalEnv),"Raster")) {
                checkObject(object=get(name, envir=.GlobalEnv), layer=layer, ...)
              } else {
                warning(paste("The object \"",name,"\" exists, but is not
                              a Raster, so layer is ignored",sep=""))
                return(invisible(TRUE))
              }
            } else {
              warning(paste(name,"does not exist in the global environment"))
              return(FALSE)
            }
})

##############################################################
#' Check use and existence of params passed to simulation.
#'
#' Checks that all parameters passed are used in a module,
#' and that all parameters used in a module are passed.
#'
#' @param sim    A simList simulation object.
#'
#' @param defaultModules List of default modules.
#'
#' @param defaultParams List of default parameters.
#'
#' @param path The location of the modules' source files.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return  Invisibly return \code{TRUE} indicating object exists; code{FALSE} if not.
#'          Sensible warning messages are be produced identifying missing params.
#'
#' @export
#' @docType methods
#' @rdname checkParams-method
#'
#' @author Alex Chubaty
#'
# @examples
# \dontrun{}
#'
setGeneric("checkParams", function(sim, defaultModules, defaultParams, path, ...) {
  standardGeneric("checkParams")
})


#' @rdname checkParams-method
setMethod("checkParams",
          signature(sim="simList", defaultModules="list", defaultParams="list", path="character"),
          definition=function(sim, defaultModules, defaultParams, path, ...) {
            params <- simParams(sim)
            modules <- simModules(sim)
            userModules <- modules[-which(defaultModules %in% modules)]
            globalParams <- simGlobals(sim)
            allFound <- TRUE

            ### check whether each param in simInit occurs in a module's .R file
            globalsFound <- list()
            for (uM in userModules) {
              # check global params
              if (length(globalParams)>0) {
                for (i in 1:length(globalParams)) {
                  gP <- names(globalParams[i])
                  result <- grep(gP, readLines(paste(path, "/", uM, ".R", sep="")), value=FALSE)
                  if (length(result)>0) {
                    globalsFound <- append(globalsFound, gP)
                  }
                }
              }

              # check user params
              userParams <- params[[uM]][-which(names(params[[uM]]) %in% defaultParams)]
              if (length(userParams)>0) {
                for (i in 1:length(userParams)) {
                  uP <- names(userParams[i])
                  result <- grep(uP, readLines(paste(path, "/", uM, ".R", sep="")), value=FALSE)
                  if (length(result)<=0) {
                    allFound <- FALSE
                    warning(paste("Parameter", uP, "is not used in module", uM))
                  }
                }
              }
            }

            globalsFound <- unique(globalsFound)
            notFound <- setdiff(names(globalParams), globalsFound)
            if (length(notFound)>0) {
              allFound <- FALSE
              warning(paste("Global parameters", notFound, "are not used in any module."))
            }

            ### check whether each param in a module's .R file occurs in simInit
            globalsFound <- list()
            for (uM in userModules) {
              # read in and cleanup/isolate the global params in the module's .R file
              moduleParams <- grep("simGlobals\\(sim\\)\\$",
                                   readLines(paste(path, "/", uM, ".R", sep="")), value=TRUE)
              moduleParams <- strsplit(moduleParams, " ")
              moduleParams <- unlist(lapply(moduleParams, function(x) x[nchar(x)>0] ))
              moduleParams <- grep("simGlobals\\(sim\\)\\$", moduleParams, value=TRUE)
              moduleParams <- gsub(",", "", moduleParams)
              moduleParams <- gsub("\\)\\)", "", moduleParams)
              moduleParams <- gsub("^.*\\(simGlobals\\(sim\\)", "\\simGlobals\\(sim\\)", moduleParams)
              moduleParams <- gsub("^simGlobals\\(sim\\)", "", moduleParams)
              moduleParams <- gsub("\\)\\$.*", "", moduleParams)
              moduleParams <- sort(unique(moduleParams))
              moduleParams <- gsub("\\$", "", moduleParams)

              if (length(moduleParams)>0) {
                if (length(globalParams)>0) {
                  for (i in 1:length(moduleParams)) {
                    mP <- moduleParams[i]
                    if (mP %in% names(globalParams)) {
                      globalsFound <- append(globalsFound, mP)
                    }
                  }
                }
              }

              # read in and cleanup/isolate the user params in the module's .R file
              moduleParams <- grep(paste0("simParams\\(sim\\)\\$", uM, "\\$"),
                                   readLines(paste(path, "/", uM, ".R", sep="")), value=TRUE)
              moduleParams <- strsplit(moduleParams, " ")
              moduleParams <- unlist(lapply(moduleParams, function(x) x[nchar(x)>0] ))
              moduleParams <- grep(paste0("simParams\\(sim\\)\\$", uM, "\\$"),
                                   moduleParams, value=TRUE)
              moduleParams <- unlist(strsplit(moduleParams, "="))
              moduleParams <- grep(paste0("simParams\\(sim\\)\\$", uM, "\\$"),
                                   moduleParams, value=TRUE)
              moduleParams <- gsub(",", "", moduleParams)
              moduleParams <- gsub("\\)\\)", "", moduleParams)
              moduleParams <- gsub("^.*\\(simParams\\(sim\\)", "\\simParams\\(sim\\)", moduleParams)
              moduleParams <- sort(unique(moduleParams))
              moduleParams <- gsub(paste0("simParams\\(sim\\)\\$", uM, "\\$"), "", moduleParams)

              if (length(moduleParams)>0) {
                # which params does the user supply to simInit?
                userParams <- sort(unlist(names(params[[uM]])))
                if (length(userParams)>0) {
                  for (i in 1:length(moduleParams)) {
                    mP <- moduleParams[i]
                    if (!(mP %in% userParams)) {
                      allFound <- FALSE
                      warning(paste("Parameter", mP, "is not supplied to module", uM, "during simInit"))
                    }
                  }
                }
              }

              globalsFound <- unique(globalsFound)
              notFound <- setdiff(globalsFound, names(globalParams))
              if (length(notFound)>0) {
                allFound <- FALSE
                warning(paste("The following global parameters are used in module", uM,
                              "but not supplied to simInit in .globals:", unlist(notFound)))
              }
            }

            return(invisible(allFound))
})
