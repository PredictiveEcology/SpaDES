###############################################################################
#' Check for existence of object(s) referenced by a \code{objects} slot of a
#' \code{simList} object
#'
#' Check that a named object exists in the provide \code{simList} environment slot,
#' and optionally has desired attributes.
#'
#' @param sim     A \code{\link{simList}} object.
#'
#' @param name    A character string specifying the name of an object to be checked.
#'
#' @param object  An object. This is mostly used internally, or with layer,
#'                  because it will fail if the object does not exist.
#'
#' @param layer   Character string, specifying a layer name in a Raster, if the
#'                \code{name} is a \code{Raster*} object.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return Invisibly return \code{TRUE} indicating object exists; \code{FALSE} if not.
#'
#' @seealso \code{\link{library}}.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname checkObject
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setGeneric("checkObject", function(sim, name, object, layer, ...) {
  standardGeneric("checkObject")
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", name = "missing", object = "Raster", layer = "character"),
  definition = function(sim, object, layer, ...) {
    if (exists(deparse(substitute(object)), envir = sim@.envir)) {
      if (!is.na(match(layer, names(object)))) {
        return(invisible(TRUE))
      } else {
        message(paste(deparse(substitute(object, env = sim@.envir)),
                      "exists, but", layer, "is not a layer"))
        return(FALSE)
      }
    } else {
      message(paste(deparse(substitute(object, env = sim@.envir)),
                    "does not exist."))
      return(FALSE)
    }
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", name = "missing", object = "ANY", layer = "missing"),
  definition = function(sim, name, object, ...) {
    if (exists(deparse(substitute(object)), envir = sim@.envir)) {
      return(invisible(TRUE))
    } else {
      message(paste(deparse(substitute(object, env = sim@.envir)),
                    "does not exist"))
      return(FALSE)
    }
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", name = "character", object = "missing", layer = "missing"),
  definition = function(sim, name, ...) {
    if (exists(name, envir = sim@.envir)) {
      return(invisible(TRUE))
    } else {
      simName <- objectNames("spades", "simList", "sim")[[1]]$objs
      message(paste(name, "does not exist in", simName))
      return(FALSE)
    }
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", name = "character", object = "missing", layer = "character"),
  definition = function(sim, name, layer, ...) {
    if (exists(name, envir = sim@.envir)) {
      if (is(sim[[name]], "Raster")) {
        if (!is(sim[[name]][[layer]], "Raster")) {
          message(paste("The object \"", name, "\" exists, but is not
                        a Raster, so layer is ignored", sep = ""))
          return(invisible(TRUE))
        }
      }
    } else {
      message(
        paste(name, "does not exist in", deparse(substitute(sim)))
      )
      return(FALSE)
    }
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "missing", name = "ANY", object = "missing", layer = "ANY"),
  definition = function(name, object, layer, ...) {
    stop(paste("Must provide a simList object"))
    return(FALSE)
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
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname checkParams
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("checkParams", function(sim, coreModules, coreParams, path, ...) {
  standardGeneric("checkParams")
})

#' @rdname checkParams
setMethod(
  "checkParams",
  signature(sim = "simList", coreModules = "list", coreParams = "list",
            path = "character"),
  definition = function(sim, coreModules, coreParams, path, ...) {
    params <- sim@params
    modules <- sim@modules
    userModules <- modules[-which(coreModules %in% modules)]
    globalParams <- sim@params$.globals
    allFound <- TRUE

    if (length(userModules)) {
      ### check whether each param in simInit occurs in a module's .R file
      globalsFound <- list()
      for (uM in userModules) {
        # check global params
        if (length(globalParams) > 0) {
          for (i in 1:length(globalParams)) {
            gP <- names(globalParams[i])
            result <- grep(gP, readLines(paste(path, "/", uM, "/", uM, ".R",
                                               sep = "")), value = FALSE)
            if (length(result) > 0) {
              globalsFound <- append(globalsFound, gP)
            }
          }
        }

        # check user params
        userParams <- params[[uM]][-which(names(params[[uM]]) %in% coreParams)]
        if (length(userParams) > 0) {
          for (i in 1:length(userParams)) {
            uP <- names(userParams[i])
            result <- grep(uP, readLines(paste(path, "/", uM, "/", uM, ".R",
                                               sep = "")), value = FALSE)
            if (length(result) <= 0) {
              allFound <- FALSE
              message(paste("Parameter", uP, "is not used in module", uM))
            }
          }
        }
      }

      globalsFound <- unique(globalsFound)
      notFound <- setdiff(names(globalParams), globalsFound)
      if (length(notFound) > 0) {
        allFound <- FALSE
        message("Global parameter(s) not used in any module: ",
                paste(notFound, collapse = ", "), ".")
      }

      ### check whether each param in a module's .R file occurs in simInit
      globalsFound <- list()
      for (uM in userModules) {
        # read in and cleanup/isolate the global params in the module's .R file
        moduleParams <- grep("globals\\(sim\\)\\$",
                             readLines(paste(path, "/", uM, "/", uM, ".R", sep = "")),
                             value = TRUE) %>%
          strsplit(., " ") %>%
          unlist(lapply(., function(x) x[nzchar(x, keepNA=TRUE)] )) %>%
          grep("globals\\(sim\\)\\$", ., value = TRUE) %>%
          gsub(",", "", .) %>%
          gsub("\\)\\)", "", .) %>%
          gsub("^.*\\(globals\\(sim\\)", "\\globals\\(sim\\)", .) %>%
          gsub("^globals\\(sim\\)", "", .) %>%
          gsub("\\)\\$.*", "", .) %>%
          unique(.) %>%
          sort(.) %>%
          gsub("\\$", "", .)

        if (length(moduleParams) > 0) {
          if (length(globalParams) > 0) {
            for (i in 1:length(moduleParams)) {
              mP <- moduleParams[i]
              if (mP %in% names(globalParams)) {
                globalsFound <- append(globalsFound, mP)
              }
            }
          }
        }

        # read in and cleanup/isolate the user params in the module's .R file
        moduleParams <- grep(paste0("params\\(sim\\)\\$", uM, "\\$"),
                             readLines(paste(path, "/", uM, "/", uM, ".R",
                                             sep = "")),
                             value = TRUE) %>%
          gsub(paste0("^.*params\\(sim\\)\\$", uM, "\\$"), "", .) %>%
          gsub("[!\"#$%&\'()*+,/:;<=>?@[\\^`{|}~-].*$", "", .) %>%
          gsub("]*", "", .) %>%
          gsub(" *", "", .) %>%
          unique(.) %>%
          sort(.)

        if (length(moduleParams) > 0) {
          # which params does the user supply to simInit?
          userParams <- sort(unlist(names(params[[uM]])))
          if (length(userParams) > 0) {
            for (i in 1:length(moduleParams)) {
              mP <- moduleParams[i]
              if (!(mP %in% userParams)) {
                allFound <- FALSE
                message(paste("Parameter", mP, "is not supplied to module",
                              uM, "during simInit"))
              }
            }
          }
        }

        globalsFound <- unique(globalsFound)
        notFound <- setdiff(globalsFound, names(globalParams))
        if (length(notFound) > 0) {
          allFound <- FALSE
          message(paste(
            "The following global parameters are used in module", uM,
            "but not supplied to simInit in .globals:", unlist(notFound)
          ))
        }
      }
    } else {
      allFound <- FALSE
    }
    return(invisible(allFound))
})
