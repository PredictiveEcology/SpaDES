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
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @export
#' @param filename The filename of the module to be parsed.
#' @inheritParams spades
#' @param defineModuleElement Character string indicating which of the list
#'                            elements in defineModule should be extracted
#' @docType methods
#' @rdname parseModule
#'
#' @author Eliot McIntire
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
      error = function(x)
        out
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
#' @param notOlderThan Passed to \code{Cache} that may be used for .inputObjects function call.
#' @param ... All \code{simInit} parameters.
#'
#' @return A \code{simList} simulation object.
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @importFrom reproducible Cache
#' @include environment.R
#' @docType methods
#' @keywords internal
#' @rdname parseModule
#'
#' @author Alex Chubaty
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
      prevNamedModules <- if(!is.null(unlist(sim@depends@dependencies))) {
          unlist(lapply(sim@depends@dependencies, function(x) slot(x, "name")))
        } else { NULL }
      if(!(m %in% prevNamedModules)) { # This is about duplicate named modules
        filename <- paste(sim@paths[["modulePath"]], "/", m, "/", m, ".R", sep = "")
        parsedFile <- parse(filename)
        defineModuleItem <- grepl(pattern = "defineModule", parsedFile)

        # evaluate the rest of the parsed file
        eval(parsedFile[!defineModuleItem], envir = sim@.envir)

        # duplicate -- put in namespaces location
        funs <- paste0("._",m) # generic name for hidden environment
        sim@.envir[[funs]] <- new.env(parent = sim@.envir)
        eval(parsedFile[!defineModuleItem], envir = sim@.envir[[funs]])


        # parse any scripts in R subfolder
        RSubFolder <- file.path(dirname(filename), "R")
        RScript <- dir(RSubFolder)
        if (length(RScript) > 0) {
          for (Rfiles in RScript) {
            parsedFile1 <- parse(file.path(RSubFolder, Rfiles))
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
          if (!is.null(sim@.envir$.inputObjects)) {
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
              moduleSpecificObjects <- c(grep(ls(sim), pattern = m, value = TRUE),
                                         na.omit(objNam))
              moduleSpecificOutputObjects <- objNam
              sim <- Cache(FUN = sim@.envir$.inputObjects, sim = sim,
                           objects = moduleSpecificObjects,
                           notOlderThan = notOlderThan,
                           outputObjects = moduleSpecificOutputObjects,
                           userTags=c(paste0("module:",m),paste0("eventType:.inputObjects")))
            } else {
              message("Running .inputObjects for ", m)
              .modifySearchPath(pkgs = sim@depends@dependencies[[i]]@reqdPkgs)
              sim <- sim@.envir$.inputObjects(sim)
              rm(".inputObjects", envir = sim@.envir)
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

################################################################################
#' Initialize a new simulation
#'
#' Create a new simulation object, the "sim" object. This object is implemented
#' using an \code{environment} where all objects and functions are placed.
#' Since environments in \code{R} are
#' pass by reference, "putting" objects in the sim object does no actual copy. This
#' is also the location of all parameters, and other important simulation information, such
#' as times, paths, modules, and module load order. See more details below.
#'
#' Calling this simInit function does several things including the following:
#' - sources all module files, placing all function definitions in the sim object
#' - optionally copies objects from the global environment to the sim object
#' - optionally loads objects from disk
#' - schedules all "init" events from all modules
#' - assesses module dependencies via the inputs and outputs identified in their metadata
#' - determines time units of modules and how they fit together
#'
#' \code{params} can only contain updates to any parameters that are defined in
#' the metadata of modules. Take the example of a module named, \code{Fire}, which
#' has a parameter named \code{.plotInitialTime}. In the metadata of that moduel,
#' it says TRUE. Here we can override that default with:
#' \code{list(Fire=list(.plotInitialTime=NA))}, effectively turning off plotting. Since
#' this is a list of lists, one can override the module defaults for multiple parameters
#' from multiple modules all at once, with say:
#' \code{list(Fire = list(.plotInitialTime = NA, .plotInterval = 2),
#'            caribouModule = list(N = 1000))}.
#'
#' We implement a discrete event simulation in a more modular fashion so it is
#' easier to add modules to the simulation. We use S4 classes and methods,
#' and use \code{data.table} instead of \code{data.frame} to implement the event
#' queue (because it is much faster).
#'
#' \code{paths} specifies the location of the module source files,
#' the data input files, and the saving output files. If no paths are specified
#' the defaults are as follows:
#'
#' \itemize{
#'   \item \code{cachePath}: \code{getOption("spades.cachePath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.modulePath")};
#'
#'   \item \code{modulePath}: \code{getOption("spades.inputPath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.outputPath")}.
#' }
#'
#' @note
#' The user can opt to run a simpler simInit call without inputs, outputs, and times.
#' These can be added later with the accessor methods (See example). These are not required for initializing the
#' simulation via simInit. \code{modules}, \code{paths}, \code{params}, and \code{objects}
#' are all needed for initialization.
#'
#' @param times A named list of numeric simulation start and end times
#'        (e.g., \code{times = list(start = 0.0, end = 10.0)}).
#'
#' @param params A list of lists of the form list(moduleName=list(param1=value, param2=value)).
#' See details.
#'
#' @param modules A named list of character strings specifying the names
#' of modules to be loaded for the simulation. Note: the module name
#' should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file
#' \file{caribou.R}, located at the specified \code{modulePath(simList)} (see below).
#'
#' @param objects (optional) A vector of object names (naming objects
#'                that are in the calling environment of
#'                the \code{simInit}, which is often the
#'                \code{.GlobalEnv} unless used programmatically
#'                -- NOTE: this mechanism will
#'                fail if object name is in a package dependency), or
#'                a named list of data objects to be
#'                passed into the simList (more reliable).
#'                These objects will be accessible
#'                from the simList as a normal list, e.g,. \code{mySim$obj}.
#'
#' @param paths  An optional named list with up to 4 named elements,
#' \code{modulePath}, \code{inputPath}, \code{outputPath}, and \code{cachePath}.
#' See details.
#'
#' @param inputs A \code{data.frame}. Can specify from 1 to 6
#' columns with following column names: \code{objectName} (character, required),
#' \code{file} (character), \code{fun} (character), \code{package} (character),
#' \code{interval} (numeric), \code{loadTime} (numeric).
#' See \code{\link{inputs}} and vignette("ii-modules") section about inputs.
#'
#' @param outputs A \code{data.frame}. Can specify from 1 to 5
#' columns with following column names: \code{objectName} (character, required),
#' \code{file} (character), \code{fun} (character), \code{package} (character),
#' \code{saveTime} (numeric). See \code{\link{outputs}} and
#' \code{vignette("ii-modules")} section about outputs.
#'
#' @param loadOrder  An optional list of module names specifying the order in
#'                   which to load the modules. If not specified, the module
#'                   load order will be determined automatically.
#'
#' @param notOlderThan A time, as in from \code{Sys.time()}. This is passed into
#'                     the \code{Cache} function that wraps \code{.inputObjects}.
#'                     If the module has a parameter, \code{.useCache} and it is
#'                     \code{TRUE}, then the \code{.inputObjects} will be cached.
#'                     Passing the current time into to \code{notOlderThan} will cause the
#'                     Cache to be refreshed, i.e., rerun.
#'
#' @return A \code{simList} simulation object, pre-initialized from values
#' specified in the arguments supplied.
#'
#' @seealso \code{\link{spades}},
#' \code{\link{times}}, \code{\link{params}}, \code{\link{objs}}, \code{\link{paths}},
#' \code{\link{modules}}, \code{\link{inputs}}, \code{\link{outputs}}
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @include priority.R
#' @export
#' @docType methods
#' @rdname simInit
#'
#' @author Alex Chubaty and Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  spades(mySim, .plotInitialTime = NA)
#'
#'  # Change more parameters, removing plotting
#'  wantPlotting <- FALSE
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      fireSpread = list(.plotInitialTime=wantPlotting),
#'      #caribouMovement = list(.plotInitialTime=wantPlotting),
#'      #randomLandscapes = list(.plotInitialTime=wantPlotting)
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  outSim <- spades(mySim)
#'
#' # A little more complicated with inputs and outputs
#' if (require(rgdal)) {
#'    mapPath <- system.file("maps", package = "quickPlot")
#'    mySim <- simInit(
#'      times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'      params = list(
#'        .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'      ),
#'      modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'      paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                   outputPath = tempdir()),
#'      inputs = data.frame(
#'        files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'        functions = "raster",
#'        package = "raster",
#'        loadTime = 0,
#'        stringsAsFactors = FALSE),
#'      outputs = data.frame(
#'        expand.grid(objectName = c("caribou","landscape"),
#'        saveTime = 1:2,
#'        stringsAsFactors = FALSE))
#'    )
#'
#'    # Use accessors for inputs, outputs, times
#'    mySim2 <- simInit(modules = list("randomLandscapes", "fireSpread",
#'                                     "caribouMovement"),
#'                      params = list(.globals = list(stackName = "landscape",
#'                                                    burnStats = "nPixelsBurned")),
#'                      paths = list(modulePath = system.file("sampleModules",
#'                                                            package = "SpaDES"),
#'                                   outputPath = tempdir()))
#'    # add by accessor: note need current in times() accessor
#'    times(mySim2) <- list(current=0, start = 0.0, end = 2.0, timeunit = "year")
#'    inputs(mySim2) <- data.frame(
#'        files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'        functions = "raster",
#'        package = "raster",
#'        loadTime = 3,
#'        stringsAsFactors = FALSE)
#'    outputs(mySim2) <- data.frame(
#'        expand.grid(objectName = c("caribou","landscape"),
#'        saveTime = 1:2,
#'        stringsAsFactors = FALSE))
#'    all.equal(mySim, mySim2) # TRUE
#'   }
#' }
#'
setGeneric(
  "simInit",
  function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
           notOlderThan = NULL) {
    standardGeneric("simInit")
})

#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "list",
    params = "list",
    modules = "list",
    objects = "list",
    paths = "list",
    inputs = "data.frame",
    outputs = "data.frame",
    loadOrder = "character"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan) {

    # For namespacing of each module; keep a snapshot of the search path
    .spadesEnv$searchPath <- search()
    on.exit({
      .modifySearchPath(.spadesEnv$searchPath, removeOthers = TRUE)
    })

    paths <- lapply(paths, checkPath, create = TRUE)

    objNames <- names(objects)
    if (length(objNames) != length(objects)) {
      stop(
        "Please pass a named list or character vector of object names whose values",
        "can be found in the parent frame of the simInit() call"
      )
    }

    # user modules
    modulesLoaded <- list()
    modules <- modules[!sapply(modules, is.null)] %>%
      lapply(., `attributes<-`, list(parsed = FALSE))

    # core modules
    core <- .coreModules() %>% unname()

    # parameters for core modules
    dotParamsReal <- list(".saveInterval",
                          ".saveInitialTime",
                          ".plotInterval",
                          ".plotInitialTime")
    dotParamsChar <- list(".savePath", ".saveObjects")
    dotParams <- append(dotParamsChar, dotParamsReal)

    # create simList object for the simulation
    sim <- new("simList")
    paths(sim) <- paths #paths accessor does important stuff
    sim@modules <- modules  ## will be updated below

    ## timeunit is needed before all parsing of modules.
    ## It could be used within modules within defineParameter statements.
    timeunits <- .parseModulePartial(sim, modules(sim), defineModuleElement = "timeunit")

    allTimeUnits <- FALSE

    findSmallestTU <- function(sim, mods) {
      out <- lapply(.parseModulePartial(sim, mods, defineModuleElement = "childModules"), as.list)
      isParent <- lapply(out, length) > 0
      tu <- .parseModulePartial(sim, mods, defineModuleElement = "timeunit")
      hasTU <- !is.na(tu)
      out[hasTU] <- tu[hasTU]
      if (!all(hasTU)) {
        out[!isParent] <- tu[!isParent]
        while (any(isParent & !hasTU)) {
          for (i in which(isParent & !hasTU)) {
            out[[i]] <- findSmallestTU(sim, as.list(unlist(out[i])))
            isParent[i] <- FALSE
          }
        }
      }
      minTimeunit(as.list(unlist(out)))
    }

    # recursive function to extract parent and child structures
    buildModuleGraph <- function(sim, mods) {
      out <- lapply(.parseModulePartial(sim, mods, defineModuleElement = "childModules"), as.list)
      isParent <- lapply(out, length) > 0
      to <- unlist(lapply(out, function(x) {
          if (length(x) == 0) {
            names(x)
          } else {
            x
          }
      }))
      if (is.null(to))
        to <- character(0)
      from <- rep(names(out), unlist(lapply(out, length)))
      outDF <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
      while (any(isParent)) {
        for (i in which(isParent)) {
          outDF <- rbind(outDF, buildModuleGraph(sim, as.list(unlist(out[i]))))
          isParent[i] <- FALSE
        }
      }
      outDF
    }

    # run this only once, at the highest level of the hierarchy, so before the parse tree happens
    moduleGraph <- buildModuleGraph(sim, modules(sim))

    timeunits <- findSmallestTU(sim, modules(sim))

    if (length(timeunits) == 0) timeunits <- list("second") # no modules at all

    if (!is.null(times$unit)) {
      message(
        paste0(
          "times contains \'unit\', rather than \'timeunit\'. ",
          "Using \"",
          times$unit,
          "\" as timeunit"
        )
      )
      times$timeunit <- times$unit
      times$unit <- NULL
    }

    # Get correct time unit now that modules are loaded
    timeunit(sim) <- if (!is.null(times$timeunit)) {
      #sim@simtimes[["timeunit"]] <- if (!is.null(times$timeunit)) {
      times$timeunit
    } else {
      minTimeunit(timeunits)
    }

    timestep <- inSeconds(sim@simtimes[["timeunit"]], sim@.envir)
    times(sim) <- list(
      current = times$start * timestep,
      start = times$start * timestep,
      end = times$end * timestep,
      timeunit = sim@simtimes[["timeunit"]]
    )

    # START OF simInit overrides for inputs, then objects
    if (NROW(inputs)) {
      inputs <- .fillInputRows(inputs, startTime = start(sim))
    }

    # used to prevent .inputObjects from loading if object is passed in by user.
    sim$.userSuppliedObjNames <- c(objNames, inputs$objectName)

    # for now, assign only some core & global params
    sim@params$.globals <- params$.globals

    # add core module name to the loaded list (loaded with the package)
    modulesLoaded <- append(modulesLoaded, core)

    # source module metadata and code files
    lapply(modules(sim), function(m) moduleVersion(m, sim = sim))

    ## do multi-pass if there are parent modules; first for parents, then for children
    all_parsed <- FALSE
    while (!all_parsed) {
      sim <- .parseModule(sim,
                          sim@modules,
                          userSuppliedObjNames = sim$.userSuppliedObjNames,
                          notOlderThan = notOlderThan, params = params,
                          objects = objects, paths = paths)
      if (length(.unparsed(sim@modules)) == 0) {
        all_parsed <- TRUE
      }
    }

    # add name to depends
    if (!is.null(names(sim@depends@dependencies))) {
      names(sim@depends@dependencies) <- sim@depends@dependencies %>%
        lapply(., function(x)
          x@name) %>%
        unlist()
    }

    # load core modules
    for (c in core) {
      # schedule each module's init event:
      .refreshEventQueues()
      sim <- scheduleEvent(sim, start(sim, unit = sim@simtimes[["timeunit"]]),
                           c, "init", .normal())
    }

    # assign user-specified non-global params, while
    # keeping defaults for params not specified by user
    omit <- c(which(core == "load"), which(core == "save"))
    pnames <-
      unique(c(paste0(".", core[-omit]), names(sim@params)))

    if (is.null(params$.progress) || any(is.na(params$.progress))) {
      params$.progress <- list(type = NA_character_, interval = NA_real_)
    }

    tmp <- list()
    lapply(pnames, function(x) {
      tmp[[x]] <<- updateList(sim@params[[x]], params[[x]])
    })
    sim@params <- tmp

    # check user-supplied load order
    if (!all(length(loadOrder),
             all(sim@modules %in% loadOrder),
             all(loadOrder %in% sim@modules))) {
      loadOrder <- depsGraph(sim, plot = FALSE) %>% .depsLoadOrder(sim, .)
    }

    # load user-defined modules
    for (m in loadOrder) {
      # schedule each module's init event:
      sim <- scheduleEvent(sim, sim@simtimes[["start"]], m, "init", .normal())

      ### add module name to the loaded list
      modulesLoaded <- append(modulesLoaded, m)

      ### add NAs to any of the dotParams that are not specified by user
      # ensure the modules sublist exists by creating a tmp value in it
      if (is.null(sim@params[[m]])) {
        sim@params[[m]] <- list(.tmp = NA_real_)
      }

      # add the necessary values to the sublist
      for (x in dotParamsReal) {
        if (is.null(sim@params[[m]][[x]])) {
          sim@params[[m]][[x]] <- NA_real_
        } else if (is.na(sim@params[[m]][[x]])) {
          sim@params[[m]][[x]] <- NA_real_
        }
      }

      # remove the tmp value from the module sublist
      sim@params[[m]]$.tmp <- NULL

      ### Currently, everything in dotParamsChar is being checked for NULL
      ### values where used (i.e., in save.R).
    }

    # check that modules all loaded correctly and store result
    if (all(append(core, loadOrder) %in% modulesLoaded)) {
      modules(sim) <- append(core, loadOrder)
    } else {
      stop("There was a problem loading some modules.")
    }

    # Add the data.frame as an attribute
    attr(sim@modules, "modulesGraph") <- moduleGraph

    # END OF MODULE PARSING AND LOADING
    if (length(objects)) {
      if (is.list(objects)) {
        if (length(objNames) == length(objects)) {
          objs(sim) <- objects
        } else {
          stop(
            paste(
              "objects must be a character vector of object names",
              "to retrieve from the .GlobalEnv, or a named list of",
              "objects"
            )
          )
        }
      } else {
        newInputs <- data.frame(
          objectName = objNames,
          loadTime = as.numeric(sim@simtimes[["current"]]),
          stringsAsFactors = FALSE
        ) %>%
          .fillInputRows(startTime = start(sim))
        inputs(sim) <- newInputs
      }
    }

    # load files in the filelist
    if (NROW(inputs) | NROW(inputs(sim))) {
      inputs(sim) <- rbind(inputs(sim), inputs)
      if (NROW(sim@events[moduleName == "load" &
                           eventType == "inputs" &
                           eventTime == start(sim)]) > 0) {
        sim <- doEvent.load(sim, sim@simtimes[["current"]], "inputs")
        sim@events <- sim@events[!(eventTime == sim@simtimes[["current"]] &
                                                 moduleName == "load" &
                                                 eventType == "inputs"), ]
      }
      if (any(sim@events[["eventTime"]] < sim@simtimes[["start"]])) {
        warning(
          paste0(
            "One or more objects in the inputs filelist was ",
            "scheduled to load before start(sim). ",
            "It is being be removed and not loaded. To ensure loading, loadTime ",
            "must be start(sim) or later. See examples using ",
            "loadTime in ?simInit"
          )
        )
        sim@events <-
          sim@events[eventTime >= sim@simtimes[["start"]]]
      }
    }

    if (length(outputs)) {
      outputs(sim) <- outputs
    }

    # check the parameters supplied by the user
    checkParams(sim, core, dotParams, sim@paths[["modulePath"]])

    # keep session info for debugging & checkpointing
    # sim$.sessionInfo <- sessionInfo() # commented out because it gives too much information
                                        # i.e., it includes all packages in a user search
                                        #  path, which is not necessarily the correct info

    return(invisible(sim))
})

## Only deal with objects as character
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "ANY",
    objects = "character",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan) {
    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]
    # find the simInit call that was responsible for this, get the objects
    #   in the environment of the parents of that call, and pass them to new
    #   environment.
    li$objects <- .findObjects(objects)
    names(li$objects) <- objects
    sim <- do.call("simInit", args = li)

    return(invisible(sim))
})

## Only deal with modules as character vector
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "character",
    objects = "ANY",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan) {
    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]
    li$modules <- as.list(modules)
    sim <- do.call("simInit", args = li)

    return(invisible(sim))
  }
)

###### individual missing elements
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "ANY",
    objects = "ANY",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan) {
    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]

    if (missing(times)) li$times <- list(start = 0, end = 10)
    if (missing(params)) li$params <- list()
    if (missing(modules)) li$modules <- list()
    if (missing(objects)) li$objects <- list()
    if (missing(paths)) li$paths <- .paths()
    if (missing(inputs)) li$inputs <- as.data.frame(NULL)
    if (missing(outputs)) li$outputs <- as.data.frame(NULL)
    if (missing(loadOrder)) li$loadOrder <- character(0)

    expectedClasses <- c("list",
                         "list",
                         "list",
                         "list",
                         "list",
                         "data.frame",
                         "data.frame",
                         "character")
    listNames <- names(li)
    expectedOrder <- c("times",
                       "params",
                       "modules",
                       "objects",
                      "paths",
                      "inputs",
                      "outputs",
                      "loadOrder")
    ma <- match(expectedOrder, listNames)
    li <- li[ma]

    if (!all(sapply(1:length(li), function(x) {
      is(li[[x]], expectedClasses[x])
    }))) {
      stop(
        "simInit is incorrectly specified. simInit takes 8 arguments. ",
        "Currently, times, params, modules, and paths must be lists (or missing), ",
        "objects can be named list or character vector (or missing),",
        "inputs and outputs must be data.frames (or missing)",
        "and loadOrder must be a character vector (or missing)",
        "For the currently defined options for simInit, type showMethods('simInit')."
      )
    }
    sim <- do.call("simInit", args = li)

    return(invisible(sim))
})

################################################################################
#' Process a simulation event
#'
#' Internal function called from \code{spades}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim Character string for the \code{simList} simulation object.
#'
#' @param debug Optional. Either Logical or character. If logical, entire \code{simList}
#'              will be printed at each event. If a character string, then it can be one
#'              of the many simList accessors, such as \code{events}, \code{params}.
#'              It can also be any R expression that will be evaluated with access
#'              to the \code{sim} object.
#'              If \code{"current"} is used, then it will be a compact list of the events
#'              as they go by.
#' @inheritParams spades
#' @return Returns the modified \code{simList} object.
#'
#' @include helpers.R
#' @importFrom data.table data.table rbindlist setkey
#' @importFrom stringi stri_pad_right stri_pad stri_length
#' @importFrom reproducible Cache
# @importFrom utils tail
#' @export
#' @keywords internal
#' @docType methods
#' @rdname doEvent
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
# igraph exports %>% from magrittr
setGeneric("doEvent", function(sim, debug, notOlderThan) {
  standardGeneric("doEvent")
})

#' @rdname doEvent
setMethod(
  "doEvent",
  signature(sim = "simList"),
  definition = function(sim, debug, notOlderThan) {
    if (class(sim) != "simList") {
      # use inherits()?
      stop("doEvent can only accept a simList object")
    }

    # core modules
    core <- .coreModules() %>% unname()

    cur <- sim@current #current(sim, "second")
    if (NROW(cur) == 0) {# || any(is.na(cur))) {
      #evnts <- sim@events #events(sim, "second")
      # get next event from the queue and remove it from the queue
      nrowEvnts <- NROW(sim@events)
      if (nrowEvnts) {

        # Next block  much faster than sim@current <- sim@events[1L,]!
        if (nrowEvnts < .lengthEventsDT) {
          for (i in 1:.numColsEventList) {
            set(.currentEventDT, 1L, i, sim@events[[i]][[1]])
            set(.eventsDT[[nrowEvnts]], , i, sim@events[[i]][-1])
          }
          sim@current <- .currentEventDT
          sim@events <- .eventsDT[[nrowEvnts]]
        } else {
          # above replaces these two lines
          sim@current <- sim@events[1L, ]
          sim@events <- sim@events[-1L, ]
        }
      } else {
        # no more events, return empty event list
        sim@current <- .emptyEventListObj
      }
    }

    # catches the situation where no future event is scheduled,
    #  but stop time is not reached
    cur <- sim@current #current(sim, "second")
    if  (NROW(cur) == 0) {
      sim@simtimes[["current"]] <- sim@simtimes[["end"]] + 1
    } else {
      if (cur[["eventTime"]] <= sim@simtimes[["end"]]) {
        # update current simulated time
        sim@simtimes[["current"]] <- cur[["eventTime"]]

        # call the module responsible for processing this event
        moduleCall <- paste("doEvent", cur[["moduleName"]], sep = ".")

        # Debug internally in the doEvent?
        debugDoEvent <- FALSE

        # check the module call for validity
        if (!(all(sapply(debug, identical, FALSE)))) {
          for (i in seq_along(debug)) {
            if (isTRUE(debug[[i]]) | debug[[i]] == "current") {
              if (NROW(cur) > 0) {
                evnts1 <- data.frame(current(sim))
                widths <- stri_length(format(evnts1))
                .spadesEnv[[".spadesDebugWidth"]] <-
                  pmax(widths, .spadesEnv[[".spadesDebugWidth"]])
                evnts1[1L, ] <-
                  stri_pad_right(format(evnts1), .spadesEnv[[".spadesDebugWidth"]])

                if (.spadesEnv[[".spadesDebugFirst"]]) {
                  evnts2 <- evnts1
                  #browser()
                  evnts2[1L:2L, ] <- rbind(
                    stri_pad(names(evnts1), .spadesEnv[[".spadesDebugWidth"]]),
                    evnts1)
                  cat("This is the current event, printed as it is happening:\n")
                  write.table(
                    evnts2,
                    quote = FALSE,
                    row.names = FALSE,
                    col.names = FALSE
                  )
                  .spadesEnv[[".spadesDebugFirst"]] <- FALSE
                } else {
                  colnames(evnts1) <- NULL
                  write.table(evnts1,
                              quote = FALSE,
                              row.names = FALSE)
                }
              }
            } else if (debug[[i]] == "simList") {
              print(sim)
            } else if (grepl(debug[[i]], pattern = "\\(")) {
              print(eval(parse(text = debug[[i]])))
            } else if (any(debug[[i]] == unlist(sim@modules))) {
              if (debug[[i]] == cur[["moduleName"]]) {
                #debugDoEvent <- TRUE
                debugonce(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
                on.exit(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
              }
            } else if (!any(debug[[i]] == c("step", "browser"))) {
              print(do.call(debug[[i]], list(sim)))
            }

            if (debug[[i]] == "step") {
              readline("Press any key to continue")
            }
          }
        }

        if (cur[["moduleName"]] %in% sim@modules) {
          if (cur[["moduleName"]] %in% core) {
              sim <- get(moduleCall)(sim, cur[["eventTime"]],
                                     cur[["eventType"]], debugDoEvent)
           } else {
             # for future caching of modules
             cacheIt <- FALSE
             a <- sim@params[[cur[["moduleName"]]]][[".useCache"]]
             if (!is.null(a)) {
               #.useCache is a parameter
               if (!identical(FALSE, a)) {
                 #.useCache is not FALSE
                 if (!isTRUE(a)) {
                   #.useCache is not TRUE
                   if (cur[["eventType"]] %in% a) {
                     cacheIt <- TRUE
                   }
                 } else {
                   cacheIt <- TRUE
                 }
               }
             }


             # if (!is.null(sim@params[[cur[["moduleName"]]]][[".useCache"]])) {
             #   if (!identical(FALSE, sim@params[[cur[["moduleName"]]]][[".useCache"]])) {
             #     if (!isTRUE(sim@params[[cur[["moduleName"]]]][[".useCache"]])) {
             #       if (cur[["eventType"]] %in% sim@params[[cur[["moduleName"]]]][[".useCache"]]) {
             #
             #       }
             #     }
             #   }
             # }

             # This is to create a namespaced module call
             .modifySearchPath(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs,
                                removeOthers = FALSE)

             if (cacheIt) {
               objNam <- sim@depends@dependencies[[cur[["moduleName"]]]]@outputObjects$objectName
               moduleSpecificObjects <- c(grep(ls(sim), pattern = cur[["moduleName"]], value = TRUE),
                                          na.omit(objNam))
               moduleSpecificOutputObjects <- objNam
               sim <- Cache(FUN = get(moduleCall, envir = sim@.envir),
                            sim = sim,
                            eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                            debug = debugDoEvent,
                            objects = moduleSpecificObjects,
                            notOlderThan = notOlderThan,
                            outputObjects = moduleSpecificOutputObjects,
                            cacheRepo = sim@paths[["cachePath"]],
                            userTags = c("function:doEvent"))
             } else {
               sim <- get(moduleCall,
                          envir = sim@.envir[[paste0("._",cur[["moduleName"]])]])(sim, cur[["eventTime"]],
                                              cur[["eventType"]], debugDoEvent)
             }
           }
        } else {
          stop(
            paste(
              "Invalid module call. The module `",
              cur[["moduleName"]],
              "` wasn't specified to be loaded."
            )
          )
        }

        # add to list of completed events
        compl <- sim@completed # completed(sim, "second")
        if (NROW(compl)) {
          # Do not use pre-existing data.tables that get updated b/c completed will almost
          #  always be large (NROW(completed) > 20), so can't realistically pre-create
          #  many data.tables
          completed <- list(compl, cur) %>%
            rbindlist()
          if (NROW(completed) > getOption("spades.nCompleted")) {
            completed <- tail(completed, n = getOption("spades.nCompleted"))
          }
        } else {
          completed <- data.table::copy(cur)
        }
        sim@completed <- completed
        # current event completed, replace current with empty
        sim@current <- .emptyEventListObj
      } else {
        # update current simulated time and event
        sim@simtimes[["current"]] <- sim@simtimes[["end"]] + 1
        if (NROW(sim@events)) { #i.e., if no more events
          sim@events <- rbind(sim@current, sim@events)
          sim@current <- .emptyEventListObj
        }
      }
    }
    return(invisible(sim))
})

#' @rdname doEvent
setMethod(
  "doEvent",
  signature(sim = "simList", debug = "missing"),
  definition = function(sim) {
    stopifnot(class(sim) == "simList")
    return(doEvent(sim, debug = FALSE))
})

################################################################################
#' Schedule a simulation event
#'
#' Adds a new event to the simulation's event queue, updating the simulation object.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim            A \code{simList} simulation object.
#'
#' @param eventTime      A numeric specifying the time of the next event.
#'
#' @param moduleName     A character string specifying the module from which to
#'                       call the event.
#'
#' @param eventType      A character string specifying the type of event from
#'                       within the module.
#'
#' @param eventPriority  A numeric specifying the priority of the event.
#'                       Lower number means higher priority.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @importFrom data.table setkey
#' @include priority.R
#' @export
#' @docType methods
#' @rdname scheduleEvent
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn") # default priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()) # default priority
#'
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()-1) # higher priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()+1) # lower priority
#'
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .highest()) # highest priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .lowest()) # lowest priority
#' }
setGeneric("scheduleEvent",
           function(sim,
                    eventTime,
                    moduleName,
                    eventType,
                    eventPriority) {
             standardGeneric("scheduleEvent")
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(
    sim = "simList",
    eventTime = "numeric",
    moduleName = "character",
    eventType = "character",
    eventPriority = "numeric"
  ),
  definition = function(sim,
                        eventTime,
                        moduleName,
                        eventType,
                        eventPriority) {
    if (length(eventTime)) {
      if (!is.na(eventTime)) {
        # if there is no metadata, meaning for the first
        #  "default" modules...load, save, checkpoint, progress
        if (!is.null(sim@depends@dependencies[[1]])) {
          # first check if this moduleName matches the name of a module
          #  with meta-data (i.e., depends(sim)@dependencies filled)
          if (moduleName %in% sapply(sim@depends@dependencies, function(x) {
            x@name
          })) {
            # If the eventTime doesn't have units, it's a user generated
            #  value, likely because of times in the simInit call.
            #  This must be intercepted, and units added based on this
            #  assumption, that the units are in \code{timeunit}
            if (is.null(attr(eventTime, "unit"))) {
              attributes(eventTime)$unit <- .callingFrameTimeunit(sim)
              eventTimeInSeconds <- convertTimeunit((
                eventTime -
                  convertTimeunit(sim@simtimes[["start"]],
                                  sim@simtimes[["timeunit"]], sim@.envir)
              ),
              "seconds",
              sim@.envir) +
                sim@simtimes[["current"]] %>%
                as.numeric()
            } else {
              eventTimeInSeconds <-
                convertTimeunit(eventTime, "seconds", sim@.envir) %>%
                as.numeric()
            }
          } else {
            # for core modules because they have no metadata
            eventTimeInSeconds <-
              convertTimeunit(eventTime, "seconds", sim@.envir) %>%
              as.numeric()
          }
        } else {
          # when eventTime is NA... can't seem to get an example
          eventTimeInSeconds <-
            convertTimeunit(eventTime, "seconds", sim@.envir) %>%
            as.numeric()
        }
        attributes(eventTimeInSeconds)$unit <- "second"

        # newEvent <- .emptyEventList(
        #   eventTime = eventTimeInSeconds,
        #   moduleName = moduleName,
        #   eventType = eventType,
        #   eventPriority = eventPriority
        # )

        #newEvent <- .singleEventListDT
        newEvent <- data.table::copy(.singleEventListDT)
        newEventList <- list(
          eventTime = eventTimeInSeconds,
          moduleName = moduleName,
          eventType = eventType,
          eventPriority = eventPriority
        )
        for (i in 1:.numColsEventList) set(newEvent, , i, newEventList[[i]])

        # if the event list is empty, set it to consist of newEvent and return;
        # otherwise, add newEvent and re-sort (rekey).
        #evnts <- sim@events #events(sim, "second")
        nrowEvnts <- NROW(sim@events)

        if (nrowEvnts == 0L) {
          # here, copy the newEVent to break connection between .singleEventListDT and sim@events
          #sim@events <- data.table::copy(newEvent) %>% setkey("eventTime", "eventPriority")
          sim@events <- newEvent #%>% setkey("eventTime", "eventPriority")
        } else {
          # This is faster than rbindlist below. So, use for smaller event queues
          if (nrowEvnts < .lengthEventsDT) {
            for (i in 1:.numColsEventList) {
              set(.eventsDT[[nrowEvnts + 2]], , i, c(sim@events[[i]], newEvent[[i]]))
            }
            sim@events <- .eventsDT[[nrowEvnts + 2]]
          } else {
            sim@events <- rbindlist(list(sim@events, newEvent))
          }
        }
        setkey(sim@events, "eventTime", "eventPriority")

      }
    } else {
      warning(
        paste(
          "Invalid or missing eventTime. This is usually caused by",
          "an attempt to scheduleEvent at an empty eventTime or by",
          "using an undefined parameter."
        )
      )
    }

    return(invisible(sim))
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(
    sim = "simList",
    eventTime = "NULL",
    moduleName = "character",
    eventType = "character",
    eventPriority = "numeric"
  ),
  definition = function(sim,
                        eventTime,
                        moduleName,
                        eventType,
                        eventPriority) {
    warning(
      paste(
        "Invalid or missing eventTime. This is usually",
        "caused by an attempt to scheduleEvent at time NULL",
        "or by using an undefined parameter."
      )
    )
    return(invisible(sim))
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(
    sim = "simList",
    eventTime = "numeric",
    moduleName = "character",
    eventType = "character",
    eventPriority = "missing"
  ),
  definition = function(sim,
                        eventTime,
                        moduleName,
                        eventType,
                        eventPriority) {
    scheduleEvent(
      sim = sim,
      eventTime = eventTime,
      moduleName = moduleName,
      eventType = eventType,
      eventPriority = .normal()
    )
})

################################################################################
#' Run a spatial discrete event simulation
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim A \code{simList} simulation object, generally produced by \code{simInit}.
#'
#' @param debug Optional logical flag or character vector indicating what to print to
#'              console at each event. See details.
#'              Default is \code{debug=FALSE}.
#'
#' @param progress Logical (TRUE or FALSE show a graphical progress bar),
#'                 character ("graphical", "text") or numeric indicating
#'                 the number of update intervals to show in a graphical progress bar.
#'
#' @param cache Logical. If TRUE, then the spades call will be cached. This means that
#'              if the call is made again with the same simList, then spades will return
#'              the return value from the previous run of that exact same simList. Default
#'              FALSE. See Details.
#'
#' @param .plotInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param notOlderThan Date or time. Passed to \code{reproducible::Cache} to update the cache. Default is
#'                     NULL, meaning don't update the cache. If \code{Sys.time()} is provided
#'                     then, it will force a recache, i.e., remove old value and replace
#'                     with new value. Ignored if \code{cache} is FALSE.
#'
#' @param ... Any. Can be used to make a unique cache identity, such as "replicate = 1". This
#'            will be included in the SpaDES::cache call, so will be unique and thus
#'            spades will not use a cached copy as long
#'            as anything passed in ... is unique, i.e., not cached previously.
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}
#'
#' @details
#' The is the workhorse function in the SpaDES package. It runs simulations by
#' implementing the rules outlined in the \code{simList}.
#'
#' This function gives simple access to two sets of module parameters:
#' \code{.plotInitialTime} and with \code{.plotInitialTime}. The primary use of
#' these arguments is to temporarily turn off plotting and saving. "Temporary"
#' means that the \code{simList} is not changed, so it can be used again with
#' the simList values reinstated. To turn off plotting and saving, use
#' \code{.plotInitialTime = NA} or \code{.saveInitialTime = NA}. NOTE: if a
#' module did not use \code{.plotInitialTime} or \code{.saveInitialTime}, then
#' these arguments will not do anything.
#'
#' If \code{cache} is TRUE, this allows for a seamless way to "save" results
#' of a simulation. The  user does not have to intentionally do any saving manually.
#' Instead, upon a call to \code{spades} in which the simList is identical,
#' the function will simply return the result that would have come if it had
#' been rerun. Use this with caution, as it will return exactly the result from
#' a previous run, even if there is stochasticity internally. Caching is only
#' based on the input simList. See also \code{experiment} for the same mechanism,
#' but it can be used with replication.
#'
#' If \code{debug} is specified, it can be a logical or character vector.
#' In all cases, something will be printed to the console immediately before each
#' event is being executed.
#' If \code{TRUE}, then the event immediately following will be printed as it
#' runs (equivalent to \code{current(sim)}).
#' If a character string, then it can be one of the many \code{simList} accessors,
#' such as \code{events}, \code{params}, \code{"simList"} (print the entire simList),
#' or any R expression.
#' If an R expression it will be evaluated with access to the \code{sim} object.
#' If this is more than one character string, then all will be printed to the
#' screen in their sequence.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user.
#' Will print additional outputs informing the user of updates to the values of
#' various \code{simList} slot components.
#' See \url{https://github.com/PredictiveEcology/SpaDES/wiki/Debugging} for details.
#'
#' @export
#' @docType methods
#' @rdname spades
#' @seealso \code{\link{experiment}} for using replication with \code{spades}.
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  spades(mySim)
#'
#'  # Different debug options
#'  spades(mySim, debug = TRUE) # Fastest
#'  spades(mySim, debug = "simList")
#'  spades(mySim, debug = "print(table(sim$landscape$Fires[]))")
#'
#'  # Can turn off plotting, and inspect the output simList instead
#'  out <- spades(mySim, .plotInitialTime = NA) # much faster
#'  completed(out) # shows completed events
#'
#'  # use cache -- simInit should generally be rerun each time a spades call is made
#'  #   to guarantee that it is identical. Here, run spades call twice, first
#'  #   time to establish cache, second time to return cached result
#'  for(i in 1:2) {
#'    mySim <- simInit(
#'      times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'      params = list(
#'        .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'      ),
#'      modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'      paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'    )
#'    print(system.time(out <- spades(mySim, cache = TRUE)))
#'  }
#' }
#'
setGeneric("spades", function(sim,
                              debug = FALSE,
                              progress = NA,
                              cache,
                              .plotInitialTime = NULL,
                              .saveInitialTime = NULL,
                              notOlderThan = NULL,
                              ...) {
  standardGeneric("spades")
})

#' @rdname spades
setMethod(
  "spades",
  signature(sim = "simList", cache = "missing"),
  definition = function(sim,
                        debug,
                        progress,
                        cache,
                        .plotInitialTime,
                        .saveInitialTime,
                        notOlderThan,
                        ...) {

    # The event queues are not uncopied data.tables, for speed during simulation
    #  Must, therefore, break connection between spades calls
    .refreshEventQueues()
    .spadesEnv$searchPath <- search()

    on.exit({
      .modifySearchPath(.spadesEnv$searchPath, removeOthers=TRUE)
    })

    if (!is.null(.plotInitialTime)) {
      if (!is.numeric(.plotInitialTime))
        .plotInitialTime <- as.numeric(.plotInitialTime)
      paramsLocal <- sim@params
      whNonHiddenModules <-
        !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <-
        lapply(paramsLocal[whNonHiddenModules], function(x) {
          x$.plotInitialTime <- .plotInitialTime
          x
        })
      sim@params <- paramsLocal
    }
    if (!is.null(.saveInitialTime)) {
      if (!is.numeric(.saveInitialTime))
        .saveInitialTime <- as.numeric(.saveInitialTime)
      paramsLocal <- sim@params
      whNonHiddenModules <-
        !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <-
        lapply(paramsLocal[whNonHiddenModules], function(x) {
          x$.saveInitialTime <- NA_real_
          x
        })
      sim@params <- paramsLocal
    }

    if (!is.na(progress)) {
      tu <- sim@simtimes[["timeunit"]]
      if (isTRUE(progress)) {
        progress <- "graphical"
      }
      if (is.numeric(progress)) {
        sim@params$.progress$interval <- (end(sim, tu) - start(sim, tu)) / progress
        progress <- "graphical"
      }

      if (!is.na(pmatch(progress, "graphical"))) {
        sim@params$.progress$type <- "graphical"
      } else if (!is.na(pmatch(progress, "text"))) {
        sim@params$.progress$type <- "text"
      }

      if (!is.na(sim@params$.progress$type) &&
          is.na(sim@params$.progress$interval)) {
        sim@params$.progress$interval <- NULL
      }
    }

    if (!(all(sapply(debug, identical, FALSE)))) {
      .spadesEnv[[".spadesDebugFirst"]] <- TRUE
      .spadesEnv[[".spadesDebugWidth"]] <- c(9, 10, 9, 13)
    }
    while (sim@simtimes[["current"]] <= sim@simtimes[["end"]]) {
      sim <- doEvent(sim, debug = debug, notOlderThan = notOlderThan)  # process the next event

    }
    sim@simtimes[["current"]] <- sim@simtimes[["end"]]
    return(invisible(sim))
})

#' @rdname spades
#' @importFrom reproducible Cache
setMethod(
  "spades",
  signature(cache = "logical"),
  definition = function(sim,
                        debug,
                        progress,
                        cache,
                        .plotInitialTime,
                        .saveInitialTime,
                        notOlderThan=NULL,
                        ...) {
    stopifnot(class(sim) == "simList")

    # if (missing(notOlderThan))
    #   notOlderThan <- NULL

    if (cache) {
       # if (is(try(archivist::showLocalRepo(sim@paths$cachePath), silent = TRUE)
       #        , "try-error"))
       #   archivist::createLocalRepo(paths(sim)$cachePath)

      return(
        Cache(
          cacheRepo=sim@paths$cachePath,
          spades,
          sim = sim,
          debug = debug,
          progress = progress,
          .plotInitialTime = .plotInitialTime,
          .saveInitialTime = .saveInitialTime,
          notOlderThan = notOlderThan,
          ...
        )
      )
    } else {
      return(
        spades(
          sim,
          debug = debug,
          progress = progress,
          .plotInitialTime = .plotInitialTime,
          .saveInitialTime = .saveInitialTime
        )
      )
    }
})
