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
                      message(paste("NOTE: unable to load package ", name, ". Is it installed?", sep=""))
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
#' @importFrom magrittr '%>%'
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
                # use slash instead of backslash; remove trailing slash
                path = normalizePath(path, winslash="/") %>% gsub("/$", "", .)

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
#' @importFrom methods is
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
                message(paste(deparse(substitute(object,env=.GlobalEnv)),"exists, but",layer,"is not a layer"))
                return(FALSE)
              }
})

#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="missing", object="ANY", layer="missing"),
          definition = function(name, object, layer, ...) {
            if(existsGlobal(deparse(substitute(object)))) {
              return(invisible(TRUE))
            } else {
              message(paste(deparse(substitute(object, env=.GlobalEnv)), "does not exist"))
              return(FALSE)
            }
})


#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="character", object="missing", layer="missing"),
          definition = function(name, ...) {
            if (existsGlobal(name)) {
                return(invisible(TRUE))
            } else {
              message(paste(name,"does not exist in the global environment"))
              return(FALSE)
            }
})

#' @rdname checkObject-method
setMethod("checkObject",
          signature(name="character", object="missing", layer="character"),
          definition = function(name, layer, ...) {
            if (existsGlobal(name)) {
              if(is(getGlobal(name),"Raster")) {
                checkObject(object=getGlobal(name), layer=layer, ...)
              } else {
                message(paste("The object \"",name,"\" exists, but is not
                              a Raster, so layer is ignored",sep=""))
                return(invisible(TRUE))
              }
            } else {
              message(paste(name,"does not exist in the global environment"))
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
#'          Sensible messages are be produced identifying missing params.
#'
#' @importFrom magrittr '%>%'
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

            if (length(userModules)) {
              ### check whether each param in simInit occurs in a module's .R file
              globalsFound <- list()
              for (uM in userModules) {
                # check global params
                if (length(globalParams)>0) {
                  for (i in 1:length(globalParams)) {
                    gP <- names(globalParams[i])
                    result <- grep(gP, readLines(paste(path, "/", uM, "/", uM, ".R", sep="")), value=FALSE)
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
                    result <- grep(uP, readLines(paste(path, "/", uM, "/", uM, ".R", sep="")), value=FALSE)
                    if (length(result)<=0) {
                      allFound <- FALSE
                      message(paste("Parameter", uP, "is not used in module", uM))
                    }
                  }
                }
              }

              globalsFound <- unique(globalsFound)
              notFound <- setdiff(names(globalParams), globalsFound)
              if (length(notFound)>0) {
                allFound <- FALSE
                message("Global parameter(s) not used in any module: ", paste(notFound, collapse=", "), ".")
              }

              ### check whether each param in a module's .R file occurs in simInit
              globalsFound <- list()
              for (uM in userModules) {
                # read in and cleanup/isolate the global params in the module's .R file
                moduleParams <- grep("simGlobals\\(sim\\)\\$",
                                     readLines(paste(path, "/", uM, "/", uM, ".R", sep="")),
                                     value=TRUE) %>%
                  strsplit(., " ") %>%
                  unlist(lapply(., function(x) x[nchar(x)>0] )) %>%
                  grep("simGlobals\\(sim\\)\\$", ., value=TRUE) %>%
                  gsub(",", "", .) %>%
                  gsub("\\)\\)", "", .) %>%
                  gsub("^.*\\(simGlobals\\(sim\\)", "\\simGlobals\\(sim\\)", .) %>%
                  gsub("^simGlobals\\(sim\\)", "", .) %>%
                  gsub("\\)\\$.*", "", .) %>%
                  unique(.) %>%
                  sort(.) %>%
                  gsub("\\$", "", .)

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
                                     readLines(paste(path, "/", uM, "/", uM, ".R", sep="")),
                                     value=TRUE) %>%
                  gsub(paste0("^.*simParams\\(sim\\)\\$", uM, "\\$"), "", .) %>%
                  gsub("[!\"#$%&\'()*+,/:;<=>?@[\\^`{|}~-].*$","", .) %>%
                  gsub("]*", "", .) %>%
                  unique(.) %>%
                  sort(.)

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
            } else {
              allFound <- FALSE
            }
            return(invisible(allFound))
})
