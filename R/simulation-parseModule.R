if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

################################################################################
#' Determine which modules in a list are unparsed
#'
#' Internal function, used during \code{\link{simInit}}.
#'
#' @param modules A character vector specifying the modules to parse.
#'
#' @return The ids of the unparsed list elements.
#'
#' @docType methods
#' @keywords internal
#' @rdname unparsed
#'
#' @author Alex Chubaty
#'
setGeneric(".unparsed",
           function(modules) {
             standardGeneric(".unparsed")
})

#' @rdname unparsed
setMethod(
  ".unparsed",
  signature(modules = "list"),
  definition = function(modules) {
    ids <- lapply(modules, function(x) {
      (attr(x, "parsed") == FALSE)
    }) %>% `==`(., TRUE) %>% which()
    return(ids)
})

################################################################################
#' @return \code{.parseModulePartial} extracts just the individual element
#' requested from the module. This can be useful if parsing the whole module
#' would cause an error.
#'
#' @param filename The filename of the module to be parsed.
#'
#' @inheritParams spades
#'
#' @param defineModuleElement Character string indicating which of the list
#'                            elements in defineModule should be extracted
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @rdname parseModule
#'
setGeneric(".parseModulePartial",
           function(sim, modules, filename, defineModuleElement) {
             standardGeneric(".parseModulePartial")
})

#' @rdname parseModule
setMethod(
  ".parseModulePartial",
  signature(
    sim = "missing",
    modules = "missing",
    filename = "character",
    defineModuleElement = "character"
  ),
  definition = function(filename, defineModuleElement) {
    parsedFile <- parse(filename)
    defineModuleItem <- grepl(pattern = "defineModule", parsedFile)
    pf <- parsedFile[defineModuleItem]

    namesParsedList <- names(parsedFile[defineModuleItem][[1]][[3]])

    element <- (namesParsedList == defineModuleElement)
    out <- pf[[1]][[3]][element][[1]]
    out <- tryCatch(
      eval(out),
      error = function(x) out
    )
    return(out)
})

#' @rdname parseModule
setMethod(
  ".parseModulePartial",
  signature(
    sim = "simList",
    modules = "list",
    filename = "missing",
    defineModuleElement = "character"
  ),
  definition = function(sim, modules, defineModuleElement) {
    out <- list()
    for (j in seq_along(modules)) {
      m <- modules[[j]][1]
      filename <-
        paste(sim@paths[["modulePath"]], "/", m, "/", m, ".R", sep = "")
      out[[m]] <- .parseModulePartial(filename = filename,
                                      defineModuleElement = defineModuleElement)
    }
    return(out)
})

################################################################################
#' Parse and initialize a module
#'
#' Internal function, used during \code{\link{simInit}}.
#'
#' @param sim     A \code{simList} simulation object.
#'
#' @param modules A list of modules with a logical attribute "parsed".
#'
#' @param userSuppliedObjNames Character string (or \code{NULL}, the default)
#'                             indicating the names of objects that user has passed
#'                             into simInit via objects or inputs.
#'                             If all module inputObject dependencies are provided by user,
#'                             then the \code{.inputObjects} code will be skipped.
#'
#' @param notOlderThan Passed to \code{Cache} that may be used for .inputObjects function call.
#'
#' @param ... All \code{simInit} parameters.
#'
#' @return A \code{simList} simulation object.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @docType methods
#' @keywords internal
#' @importFrom reproducible Cache
#' @include environment.R
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @rdname parseModule
#'
setGeneric(".parseModule",
           function(sim, modules, userSuppliedObjNames = NULL, notOlderThan, ...) {
             standardGeneric(".parseModule")
})

#' @rdname parseModule
setMethod(
  ".parseModule",
  signature(sim = "simList", modules = "list"),
  definition = function(sim, modules, userSuppliedObjNames, notOlderThan, ...) {
    all_children <- list()
    children <- list()
    parent_ids <- integer()
    dots <- list(...)
    if (!is.null(dots$objects)) objs <- dots$objects
    for (j in .unparsed(modules)) {
      m <- modules[[j]][1]
      prevNamedModules <- if (!is.null(unlist(sim@depends@dependencies))) {
        unlist(lapply(sim@depends@dependencies, function(x) slot(x, "name")))
      } else {
        NULL
      }
      if (!(m %in% prevNamedModules)) { # This is about duplicate named modules
        filename <- paste(sim@paths[["modulePath"]], "/", m, "/", m, ".R", sep = "")
        parsedFile <- parse(filename)
        defineModuleItem <- grepl(pattern = "defineModule", parsedFile)

        # duplicate -- put in namespaces location
        funs <- paste0("._",m) # generic name for hidden environment
        sim@.envir[[funs]] <- new.env(parent = sim@.envir)
        eval(parsedFile[!defineModuleItem], envir = sim@.envir[[funs]])

        doesntUseNamespacing <- isTRUE(any(grepl(paste0("^", m), ls(sim@.envir[[funs]]))))
        # evaluate the rest of the parsed file
        if (doesntUseNamespacing)
          eval(parsedFile[!defineModuleItem], envir = sim@.envir)

        # parse any scripts in R subfolder
        RSubFolder <- file.path(dirname(filename), "R")
        RScript <- dir(RSubFolder)
        if (length(RScript) > 0) {
          for (Rfiles in RScript) {
            parsedFile1 <- parse(file.path(RSubFolder, Rfiles))
            if (doesntUseNamespacing)
              eval(parsedFile1, envir = sim@.envir)
            # duplicate -- put in namespaces location
            eval(parsedFile1, envir = sim@.envir[[funs]])
          }
        }

        # evaluate all but inputObjects and outputObjects part of 'defineModule'
        #  This allow user to use params(sim) in their inputObjects
        namesParsedList <- names(parsedFile[defineModuleItem][[1]][[3]])
        inObjs <- (namesParsedList == "inputObjects")
        outObjs <- (namesParsedList == "outputObjects")
        pf <- parsedFile[defineModuleItem]
        pf[[1]][[3]] <- pf[[1]][[3]][!(inObjs | outObjs)]
        sim <- suppressWarnings(eval(pf))

        # check that modulename == filename
        fname <- unlist(strsplit(basename(filename), "[.][r|R]$"))
        k <- length(sim@depends@dependencies)

        if (sim@depends@dependencies[[k]]@name == m) {
          i <- k
        } else {
          stop("Module name metadata (", sim@depends@dependencies[[k]]@name, ") ",
               "does not match filename (", m, ".R).")
        }

        # assign default param values
        deps <- sim@depends@dependencies[[i]]@parameters
        sim@params[[m]] <- list()
        if (NROW(deps) > 0) {
          for (x in 1:NROW(deps)) {
            sim@params[[m]][[deps$paramName[x]]] <- deps$default[[x]]
          }
        }
        # override immediately with user supplied values
        pars <- list(...)$params
        if (!is.null(pars[[m]])) {
          if (length(pars[[m]]) > 0) {
            sim@params[[m]][names(pars[[m]])] <- pars[[m]]
          }
        }

        # do inputObjects and outputObjects
        pf <- parsedFile[defineModuleItem]
        if (any(inObjs)) {
          sim@depends@dependencies[[i]]@inputObjects <- data.frame(
            rbindlist(fill = TRUE,
                      list(sim@depends@dependencies[[i]]@inputObjects,
                           eval(pf[[1]][[3]][inObjs][[1]]))
            )
          )
        }
        if (any(outObjs)) {
          sim@depends@dependencies[[i]]@outputObjects <- data.frame(
            rbindlist(fill = TRUE,
                      list(sim@depends@dependencies[[i]]@outputObjects,
                           eval(pf[[1]][[3]][outObjs][[1]]))
            )
          )
        }

        # add child modules to list of all child modules, to be parsed later
        children <- as.list(sim@depends@dependencies[[i]]@childModules) %>%
          lapply(., `attributes<-`, list(parsed = FALSE))
        all_children <- append_attr(all_children, children)

        # remove parent module from the list
        if (length(children)) {
          parent_ids <- c(parent_ids, j)
        }

        ## run .inputObjects() from each module file from each module, one at a time,
        ## and remove it from the simList so next module won't rerun it.

        # If user supplies the needed objects, then test whether all are supplied.
        # If they are all supplied, then skip the .inputObjects code
        cacheIt <- FALSE
        allObjsProvided <- sim@depends@dependencies[[i]]@inputObjects$objectName %in% userSuppliedObjNames
        if (!all(allObjsProvided)) {
          if (!is.null(sim@.envir[[funs]]$.inputObjects)) {
            list2env(objs[sim@depends@dependencies[[i]]@inputObjects$objectName[allObjsProvided]],
                     envir = sim@.envir)
            a <- sim@params[[m]][[".useCache"]]
            if (!is.null(a)) {
              if (".useCache" %in% names(list(...)$params)) {  # user supplied values
                b <- list(...)$params[[i]]$.useCache
                if (!is.null(b)) a <- b
              }
              #.useCache is a parameter
              if (!identical(FALSE, a)) {
                #.useCache is not FALSE
                if (!isTRUE(a)) {
                  #.useCache is not TRUE
                  if (".inputObjects" %in% a) {
                    cacheIt <- TRUE
                  }
                } else {
                  cacheIt <- TRUE
                }
              }
            }

            if (cacheIt) {
              message("Using cached copy of .inputObjects for ", m)
              objNam <- sim@depends@dependencies[[i]]@outputObjects$objectName

              # ensure backwards compatibility with non-namespaced modules
              if (doesntUseNamespacing) {
                moduleSpecificObjects <- c(grep(ls(sim@.envir, all.names = TRUE),
                                                pattern = m, value = TRUE),
                                           na.omit(objNam))
                moduleSpecificOutputObjects <- objNam
                sim <- Cache(FUN = sim@.envir$.inputObjects, sim = sim,
                             objects = moduleSpecificObjects,
                             notOlderThan = notOlderThan,
                             outputObjects = moduleSpecificOutputObjects,
                             userTags = c(paste0("module:",m),paste0("eventType:.inputObjects")))
              } else {
                moduleSpecificObjects <- c(grep(ls(sim@.envir[[funs]], all.names = TRUE),
                                                pattern = m, value = TRUE),
                                           na.omit(objNam))
                moduleSpecificOutputObjects <- objNam
                sim <- Cache(FUN = sim@.envir[[funs]]$.inputObjects, sim = sim,
                             objects = moduleSpecificObjects,
                             notOlderThan = notOlderThan,
                             outputObjects = moduleSpecificOutputObjects,
                             userTags = c(paste0("module:",m),paste0("eventType:.inputObjects")))
              }
            } else {
              message("Running .inputObjects for ", m)
              .modifySearchPath(pkgs = sim@depends@dependencies[[i]]@reqdPkgs)

              # ensure backwards compatibility with non-namespaced modules
              if (doesntUseNamespacing) {
                sim <- sim@.envir$.inputObjects(sim)
                rm(".inputObjects", envir = sim@.envir)
              } else {
                sim <- sim@.envir[[funs]]$.inputObjects(sim)
              }

            }
          }
        }
      } else {
        message("Duplicate module, ",m,", specified. Skipping loading it twice.")
      }

      # update parse status of the module
      attributes(modules[[j]]) <- list(parsed = TRUE)
    }

    names(sim@depends@dependencies) <- unique(unlist(modules))

    modules(sim) <- if (length(parent_ids)) {
      append_attr(modules, all_children)[-parent_ids]
    } else {
      append_attr(modules, all_children)
    } %>%
      unique()

    return(sim)
})
