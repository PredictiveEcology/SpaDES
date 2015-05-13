if(getRversion() >= "3.1.0") utils::globalVariables(".")

#' Get the name of a source file
#'
#' This will only work for files that are \code{source}d.
#' Based on this: \url{http://stackoverflow.com/a/1816487/1380598}.
#'
#' @param fullname Logical (default \code{FALSE}) indicating whether the full path should be returned.
#'
#' @return Character string representing the filename.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname getFileName
#'
#' @author Alex Chubaty
#'
setGeneric("getFileName", function(fullname) {
  standardGeneric("getFileName")
})

#' @rdname getFileName
setMethod("getFileName",
          signature="logical",
          definition=function(fullname) {
            f <- lapply(sys.frames(), function(i) i$filename) %>%
              Filter(Negate(is.null), .) %>%
              unlist
            if (fullname) {
              f <- normalizePath(file.path(getwd(), f), winslash="/")
            } else {
              f <- basename(f)
            }
            return(f)
})

################################################################################
#' Update elements of a named list with elements of a second named list
#'
#' Merge two named list based on their named entries. Where
#' any element matches in both lists, the value from the
#' second list is used in the updated list.
#' Subelements are not examined and are simply replaced.
#'
#' @param x   a named list
#' @param y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list \code{y}
#'          replace the values in list \code{x}.
#'
#' @export
#' @docType methods
#' @rdname updateList
#'
#' @author Alex Chubaty
#'
#' @examples
#' L1 <- list(a="hst", b=NA_character_, c=43)
#' L2 <- list(a="gst", c=42, d=list(letters))
#' updateList(L1, L2)
#'
setGeneric("updateList", function(x, y) {
  standardGeneric("updateList")
})

#' @rdname updateList
setMethod("updateList",
          signature=c("list", "list"),
          definition=function(x, y) {
            if (any(is.null(names(x)), is.null(names(y)))) {
              stop("All elements in lists x,y must be named.")
            } else {
              i <- which(names(x) %in% names(y))
              z <- append(x[-i], y)
              return(z[order(names(z))])
            }
})

#' @rdname updateList
setMethod("updateList",
          signature=c("NULL", "list"),
          definition=function(x, y) {
            if (is.null(names(y))) {
              stop("All elements in list y must be named.")
            }
            return(y[order(names(y))])
})

#' @rdname updateList
setMethod("updateList",
          signature=c("list", "NULL"),
          definition=function(x, y) {
            if (is.null(names(x))) {
              stop("All elements in list x must be named.")
            }
            return(x[order(names(x))])
})

#' @rdname updateList
setMethod("updateList",
          signature=c("NULL", "NULL"),
          definition=function(x, y) {
            return(list())
})

################################################################################
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
#' @rdname loadPackages
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

#' @rdname loadPackages
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

################################################################################
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
#' @rdname checkPath
#'
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path="character", create="logical"),
          definition=function(path, create) {
            if (!is.na(path)) {
              if (length(path)>0) {
                # use slash instead of backslash
                # do tilde etc. expansion
                # remove trailing slash
                path = gsub("\\\\", "/", path) %>%
                       normalizePath(., winslash="/", mustWork=FALSE) %>%
                       gsub("/$", "", .)

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

#' @rdname checkPath
setMethod("checkPath",
          signature(path="character", create="missing"),
          definition=function(path) {
            return(checkPath(path, create=FALSE))
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path="NULL", create="ANY"),
          definition=function(path) {
            stop("Invalid path: cannot be NULL.")
})

################################################################################
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
#' @return Invisibly return \code{TRUE} indicating object exists; \code{FALSE} if not.
#'
#' @seealso \code{\link{library}}.
#'
#' @importFrom methods is
#' @export
#' @docType methods
#' @rdname checkObject
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setGeneric("checkObject", function(name, object, layer, ...) {
  standardGeneric("checkObject")
})

#' @rdname checkObject
setMethod("checkObject",
          signature(name="missing", object="Raster", layer="character"),
          definition = function(name, object, layer, ...) {
              if (!is.na(match(layer, names(object)))) {
                return(invisible(TRUE))
              } else {
                message(paste(deparse(substitute(object, env=.GlobalEnv)), "exists, but",
                              layer, "is not a layer"))
                return(FALSE)
              }
})

#' @rdname checkObject
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


#' @rdname checkObject
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

#' @rdname checkObject
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

################################################################################
#' Check use and existence of params passed to simulation.
#'
#' Checks that all parameters passed are used in a module,
#' and that all parameters used in a module are passed.
#'
#' @param sim    A simList simulation object.
#'
#' @param coreModules List of core modules.
#'
#' @param coreParams List of default core parameters.
#'
#' @param path The location of the modules' source files.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return  Invisibly return \code{TRUE} indicating object exists; \code{FALSE} if not.
#'          Sensible messages are be produced identifying missing parameters.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname checkParams
#'
#' @author Alex Chubaty
#'
setGeneric("checkParams", function(sim, coreModules, coreParams, path, ...) {
  standardGeneric("checkParams")
})

#' @rdname checkParams
setMethod("checkParams",
          signature(sim="simList", coreModules="list", coreParams="list", path="character"),
          definition=function(sim, coreModules, coreParams, path, ...) {

            params <- simParams(sim)
            modules <- simModules(sim)
            userModules <- modules[-which(coreModules %in% modules)]
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
                userParams <- params[[uM]][-which(names(params[[uM]]) %in% coreParams)]
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
                        message(paste("Parameter", mP, "is not supplied to module", uM, "during simInit"))
                      }
                    }
                  }
                }

                globalsFound <- unique(globalsFound)
                notFound <- setdiff(globalsFound, names(globalParams))
                if (length(notFound)>0) {
                  allFound <- FALSE
                  message(paste("The following global parameters are used in module", uM,
                                "but not supplied to simInit in .globals:", unlist(notFound)))
                }
              }
            } else {
              allFound <- FALSE
            }
            return(invisible(allFound))
})


###############################################################
#' Convert numeric to character with padding
#'
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param pad character to use as padding (\code{nchar(pad)==1} must be \code{TRUE}).
#'            Passed to \code{\link[stringr]{str_pad}}
#'
#' @return Character string representing the filename.
#'
#' @importFrom fpCompare '%==%'
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_pad
#' @export
#' @docType methods
#' @rdname paddedFloatToChar
#'
#' @author Eliot McIntire and Alex Chubaty
paddedFloatToChar <- function(x, padL=ceiling(log10(x+1)), padR=3, pad="0") {
  xIC <- x %/% 1 %>%
    format(., trim=TRUE, digits=5,scientific=FALSE) %>%
    str_pad(., pad=pad, width=padL, side="left")
  xf <- x %% 1
  xFC <- if(xf %==% 0) { "" } else {
    strsplit(format(xf, digits=padR, scientific=FALSE), split="\\.")[[1]][2] %>%
      str_pad(., width=padR, side="right", pad=pad) %>%
      paste0(".", .)
  }
  return(paste0(xIC, xFC))
}
