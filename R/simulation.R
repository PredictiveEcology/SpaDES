if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

################################################################################
#' Determine which modules in a list are unparsed
#'
#' Internal function, used during \code{\link{simInit}}.
#'
#' @param modules A chracter vector specifying the modules to parse.
#'
#' @return The ids of the unparsed list elements.
#'
#' @export
#' @docType methods
#' @rdname unparsed
#'
#' @author Alex Chubaty
#'
setGeneric(
  ".unparsed",
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
    }) %>% `==`(., TRUE) %>% which
    return(ids)
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
#' @return A \code{simList} simulation object.
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @export
#' @docType methods
#' @rdname parseModule
#'
#' @author Alex Chubaty
#'
setGeneric(
  ".parseModule",
  function(sim, modules) {
    standardGeneric(".parseModule")
})

#' @rdname parseModule
setMethod(
  ".parseModule",
  signature(sim = "simList", modules = "list"),
  definition = function(sim, modules) {
    all_children <- list()
    children <- list()
    parent_ids <- integer()
    for (j in .unparsed(modules)) {
      m <- modules[[j]][1]
      filename <- paste(modulePath(sim), "/", m, "/", m, ".R", sep = "")
      parsedFile <- parse(filename)
      defineModuleItem <- grepl(pattern = "defineModule", parsedFile)

      # evaluate only the 'defineModule' function of parsedFile
      sim <- eval(parsedFile[defineModuleItem])

      # check that modulename == filename
      fname <- unlist(strsplit(basename(filename), "[.][r|R]$"))
      for (k in length(depends(sim)@dependencies)) {
        if (depends(sim)@dependencies[[k]]@name == m) i <- k
      }

      # assign default param values
      apply(depends(sim)@dependencies[[i]]@parameters, 1, function(x) {
        capture.output(
          tt <- paste0("params(sim)$", m, "$", x$paramName, " <<- ",
                     dput(deparse(x$default)))
          )
        eval(parse(text = tt), envir = environment())
      })

      # evaluate the rest of the parsed file
      eval(parsedFile[!defineModuleItem], envir = envir(sim))

      # update parse status of the module
      attributes(modules[[j]]) <- list(parsed = TRUE)

      # add child modules to list of all child modules, to be parsed later
      children <- as.list(depends(sim)@dependencies[[i]]@childModules) %>%
        lapply(., `attributes<-`, list(parsed = FALSE))
      all_children <- append_attr(all_children, children)

      # remove parent module from the list
      if (length(children)) {
        parent_ids <- c(parent_ids, j)
      }
    }

    modules(sim) <- if (length(parent_ids)) {
        append_attr(modules, all_children)[-parent_ids]
      } else {
        append_attr(modules, all_children)
      } %>%
      unique

    return(sim)
  }
)

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
#' \code{list(Fire=list(.plotInitialTime=NA, .plotInterval=2),
#'            caribouModule=list(N=1000))}.
#'
#' We implement a discrete event simulation in a more modular fashion so it is
#' easier to add modules to the simulation. We use S4 classes and methods,
#' and use \code{data.table} instead of \code{data.frame} to implement the event
#' queue (because it is much faster).
#'
#' \code{paths} specifies the location of the module source files,
#' the data input files, and the saving output files. If no paths are specified,
#' default is current working directory.
#'
#' @note
#' The user can opt to run a simpler simInit call without inputs, outputs, and times.
#' These can be added later with the accessor methods (See last example). These are not required for initializing the
#' simulation via simInit. \code{modules}, \code{paths}, \code{params}, and \code{objects}
#' are all needed for initialization.
#'
#' @param times A named list of numeric simulation start and end times
#'        (e.g., \code{times = list(start = 0.0, end = 10.0)}).
#'
#' @param params A list of lists of the form list(moduleName=list(param1=value, param2=value)).
#' See details.
#'
#' @param modules A named list of character strings specfying the names
#' of modules to be loaded for the simulation. Note: the module name
#' should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file
#' \file{caribou.R}, located at the specified \code{modulePath(simList)} (see below).
#'
#' @param objects An optional list of data objects to be passed into the simList.
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
#' @param loadOrder  An optional list of module names specfiying the order in
#'                   which to load the modules. If not specified, the module
#'                   load order will be determined automatically.
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
#' @importFrom gtools smartbind
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
#'  spades(mySim)
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
#'  mapPath <- system.file("maps", package = "SpaDES")
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tempdir()),
#'    inputs = data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "raster",
#'      package = "raster",
#'      loadTime = 3,
#'      stringsAsFactors = FALSE),
#'    outputs = data.frame(
#'      expand.grid(objectName = c("caribou","landscape"),
#'      saveTime = 1:2,
#'      stringsAsFactors = FALSE))
#'  )
#'
#'  # Use accessors for inputs, outputs, times
#'  mySim2 <- simInit(modules = list("randomLandscapes", "fireSpread",
#'                                   "caribouMovement"),
#'                    params = list(.globals = list(stackName = "landscape",
#'                                                  burnStats = "nPixelsBurned")),
#'                    paths = list(modulePath = system.file("sampleModules",
#'                                                          package = "SpaDES"),
#'                                 outputPath = tempdir()))
#'  # add by accessor: note need current in times() accessor
#'  times(mySim2) <- list(current=0, start = 0.0, end = 2.0, timeunit = "year")
#'  inputs(mySim2) <- data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "raster",
#'      package = "raster",
#'      loadTime = 3,
#'      stringsAsFactors = FALSE)
#'  outputs(mySim2) <- data.frame(
#'      expand.grid(objectName = c("caribou","landscape"),
#'      saveTime = 1:2,
#'      stringsAsFactors = FALSE))
#'  all.equal(mySim, mySim2) # TRUE
#' }
#'
# igraph exports %>% from magrittr
setGeneric(
  "simInit",
   function(times, params, modules, objects, paths, inputs, outputs, loadOrder) {
     standardGeneric("simInit")
})

#' @rdname simInit
setMethod(
  "simInit",
  signature(times = "list", params = "list", modules = "list", objects = "list",
            paths = "list", inputs = "data.frame", outputs = "data.frame",
            loadOrder = "character"),
  definition = function(times, params, modules, objects, paths, inputs, outputs,
                        loadOrder) {
    paths <- lapply(paths, checkPath, create = TRUE)
    modulesLoaded <- list()

    if (length(names(objects)) != length(objects)) {
      stop("Please pass a named list or character vector of object names whose values",
           "can be found in the parent frame of the simInit call")
    }
    # user modules
    modules <- modules[!sapply(modules, is.null)] %>%
      lapply(., `attributes<-`, list(parsed = FALSE))

    # core modules
    core <- list("checkpoint", "save", "progress", "load")

    # parameters for core modules
    dotParamsReal <- list(".saveInterval", ".saveInitialTime",
                         ".plotInterval", ".plotInitialTime")
    dotParamsChar <- list(".savePath", ".saveObjects")
    dotParams <- append(dotParamsChar, dotParamsReal)

    # create simList object for the simulation
    sim <- new("simList")
    modules(sim) <- modules
    paths(sim) <- paths

    # for now, assign only some core & global params
    globals(sim) <- params$.globals

    # load core modules
    for (c in core) {
      ### sourcing the code in each core module is already done
      ### because they are loaded with the package

      # add core module name to the loaded list:
      modulesLoaded <- append(modulesLoaded, c)
    }

    # source module metadata and code files, checking version info
    lapply(modules(sim), function(m) {
      mVersion <- moduleMetadata(m, modulePath(sim))$version
      versionWarning(m, mVersion)
    })
    all_parsed <- FALSE
    while (!all_parsed) {
      sim <- .parseModule(sim, modules(sim))
      if (length(.unparsed(modules(sim))) == 0) { all_parsed <- TRUE }
    }

    # timeunit has no meaning until all modules are loaded,
    #  so this has to be after loading
    timeunit(sim) <- if (!is.null(times$timeunit)) {
      times$timeunit
    } else {
      minTimeunit(sim)
    }

    timestep <- inSeconds(timeunit(sim), envir(sim))
    times(sim) <- list(current = times$start * timestep,
                       start = times$start * timestep,
                       end = times$end * timestep,
                       timeunit = timeunit(sim))

    # load core modules
    for (c in core) {
      # schedule each module's init event:
      sim <- scheduleEvent(sim, start(sim, unit=timeunit(sim)),
                           c, "init", .normal())
    }

    # assign user-specified non-global params, while
    # keeping defaults for params not specified by user
    omit <- c(which(core == "load"), which(core == "save"))
    pnames <- unique(c(paste0(".", core[-omit]), names(params(sim))))

    if ( (is.null(params$.progress)) || (any(is.na(params$.progress))) ) {
      params$.progress <- list(type = NA_character_, interval = NA_real_)
    }

    tmp <- list()
    lapply(pnames, function(x) {
      tmp[[x]] <<- updateList(params(sim)[[x]], params[[x]])
    })
    params(sim) <- tmp

    # check user-supplied load order
    if (!all( length(loadOrder),
              all(modules(sim) %in% loadOrder),
              all(loadOrder %in% modules(sim)) )) {
      loadOrder <- depsGraph(sim, plot = FALSE) %>% .depsLoadOrder(sim, .)
    }

    # load user-defined modules
    for (m in loadOrder) {
      # schedule each module's init event:
      sim <- scheduleEvent(sim, start(sim, "seconds"), m, "init", .normal())

      ### add module name to the loaded list
      modulesLoaded <- append(modulesLoaded, m)

      ### add NAs to any of the dotParams that are not specified by user
      # ensure the modules sublist exists by creating a tmp value in it
      if (is.null(params(sim)[[m]])) {
        params(sim)[[m]] <- list(.tmp = NA_real_)
      }

      # add the necessary values to the sublist
      for(x in dotParamsReal) {
        if (is.null(params(sim)[[m]][[x]])) {
          params(sim)[[m]][[x]] <- NA_real_
        } else if (is.na(params(sim)[[m]][[x]])) {
          params(sim)[[m]][[x]] <- NA_real_
        }
      }

      # remove the tmp value from the module sublist
      params(sim)[[m]]$.tmp <- NULL

      ### Currently, everything in dotParamsChar is being checked for NULL
      ### values where used (i.e., in save.R).
    }

    # check that modules all loaded correctly and store result
    if (all( append(core, loadOrder) %in% modulesLoaded )) {
      modules(sim) <- append(core, loadOrder)
    } else {
      stop("There was a problem loading some modules.")
    }

    # load files in the filelist
    if (length(inputs)) {
      inputs(sim) <- inputs
      if (NROW(
        events(sim)[moduleName == "load" & eventType == "inputs" &
                    eventTime == start(sim)]
        ) > 0) {
        sim <- doEvent.load(sim, time(sim, "second"), "inputs")
        events(sim) <- events(sim, "second")[
          !(eventTime == time(sim, "second") &
              moduleName == "load" &
              eventType == "inputs"),]
      }
    }

    if (length(outputs)) {
      outputs(sim) <- outputs
    }

    # check the parameters supplied by the user
    checkParams(sim, core, dotParams, modulePath(sim))

    if (length(objects)) {
      list2env(objects, envir = envir(sim))
      newInputs <- data.frame(
        file = NA_character_,
        fun = NA_character_,
        package = NA_character_,
        objectName = names(objects),
        loadTime = as.numeric(time(sim, "seconds")),
        loaded = TRUE,
        stringsAsFactors = FALSE)
      if (NROW(inputs)) {
        inputs(sim) <- smartbind(inputs(sim), newInputs)
      } else {
        inputs(sim) <- newInputs
      }
    }

    # keep session info for debugging & checkpointing
    sim$.sessionInfo <- sessionInfo()

    return(invisible(sim))
})

## Only deal with objects as character
#' @rdname simInit
setMethod(
  "simInit",
  signature(times = "ANY", params = "ANY", modules = "ANY",
            objects = "character", paths = "ANY",
            inputs = "ANY", outputs = "ANY", loadOrder = "ANY"),
  definition = function(times, params, modules, objects, paths, inputs, outputs, loadOrder) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]
    # find the simInit call that was responsible for this, get the objects
    #   in the environment of the parents of that call, and pass them to new
    #   environment.
    scalls <- sys.calls()
    grep1 <- grep(as.character(scalls), pattern = "simInit")
    grep1 <- pmax(min(grep1[sapply(scalls[grep1], function(x) {
      tryCatch(
        is(parse(text = x), "expression"),
        error = function(y) { NA })
    })], na.rm = TRUE)-1, 1)
    # Convert character strings to their objects
    li$objects <- lapply(objects, function(x) get(x, envir = sys.frames()[[grep1]]))
    names(li$objects) <- objects
    sim <- do.call("simInit", args = li)

    return(invisible(sim))
})

## Only deal with modules as character vector
#' @rdname simInit
setMethod(
  "simInit",
  signature(times = "ANY", params = "ANY", modules = "character",
            objects = "ANY", paths = "ANY",
            inputs = "ANY", outputs = "ANY", loadOrder = "ANY"),
  definition = function(times, params, modules, objects, paths, inputs, outputs, loadOrder) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]
    li$modules <- as.list(modules)
    sim <- do.call("simInit", args = li)

    return(invisible(sim))
})

###### individual missing elements
#' @rdname simInit
setMethod(
  "simInit",
  signature(),
  definition = function(times, params, modules, objects, paths, inputs, outputs, loadOrder) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]

    if (missing(times)) li$times <- list(start = 0, end = 10)
    if (missing(params)) li$params <- list()
    if (missing(modules)) li$modules <- list()
    if (missing(objects)) li$objects <- list()
    if (missing(paths)) li$paths <- list(".")
    if (missing(inputs)) li$inputs <- as.data.frame(NULL)
    if (missing(outputs)) li$outputs <- as.data.frame(NULL)
    if (missing(loadOrder)) li$loadOrder <- character(0)

    expectedClasses <- c("list", "list", "list", "list", "list",
                         "data.frame", "data.frame", "character")
    listNames <- names(li)
    expectedOrder <- c("times", "params", "modules", "objects", "paths",
                       "inputs", "outputs","loadOrder")
    ma <- match(expectedOrder, listNames)
    li <- li[ma]

    if (!all(sapply(1:length(li), function(x) {
      is(li[[x]], expectedClasses[x])
    }))) {
      stop("simInit is incorrectly specified. simInit takes 8 arguments. ",
           "Currently, times, params, modules, and paths must be lists (or missing), ",
           "objects can be named list or character vector (or missing),",
           "inputs and outputs must be data.frames (or missing)",
           "and loadOrder must be a character vector (or missing)",
           "For the currently defined options for simInit, type showMethods('simInit').")
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
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @include helpers.R
#' @importFrom data.table data.table rbindlist setkey
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
setGeneric("doEvent", function(sim, debug) {
    standardGeneric("doEvent")
})

#' @rdname doEvent
setMethod(
  "doEvent",
  signature(sim = "simList", debug = "logical"),
  definition = function(sim, debug) {
    if (class(sim) != "simList") { # use inherits()?
      stop("doEvent can only accept a simList object")
    }

    # core modules
    core <- list("checkpoint", "save", "progress", "load")

    cur <- current(sim)
    if ( NROW(cur) == 0 || any(is.na(cur)) ) {
      evnts <- events(sim, "second")
      # get next event from the queue and remove it from the queue
      if (NROW(evnts)) {
        current(sim) <- evnts[1L,]
        events(sim) <- evnts[-1L,]
      } else {
        # no more events, return event list of NAs
        current(sim) <- .emptyEventListNA
      }
    }

    # catches the situation where no future event is scheduled,
    #  but stop time is not reached
    cur <- current(sim, "second")
    if (any(is.na(cur))) {
      time(sim) <- end(sim, "second") + 1
    } else {
      if (cur$eventTime <= end(sim, "second")) {
        # update current simulated time
        time(sim) <- cur$eventTime

        # call the module responsible for processing this event
        moduleCall <- paste("doEvent", cur$moduleName, sep = ".")

        # check the module call for validity
        if (cur$moduleName %in% modules(sim)) {
          if (cur$moduleName %in% core) {
              sim <- get(moduleCall)(sim, cur$eventTime,
                                     cur$eventType, debug)
           } else {
              sim <- get(moduleCall,
                         envir = envir(sim))(sim, cur$eventTime,
                                             cur$eventType, debug)
           }
        } else {
          stop(paste("Invalid module call. The module `",
                     cur$moduleName,
                     "` wasn't specified to be loaded."))
        }

        # add to list of completed events
        compl <- completed(sim, "second")
        if (NROW(compl)) {
          completed <- list(compl, cur) %>%
            rbindlist()
          if (NROW(completed) > getOption("spades.nCompleted")) {
            completed <- tail(completed, n = getOption("spades.nCompleted"))
          }
        } else {
          completed <- cur
        }
        completed(sim) <- completed
        current(sim) <- .emptyEventListNA
      } else {
        # update current simulated time and event
        time(sim) <- end(sim) + 1
      }
    }
    return(invisible(sim))
})

#' @rdname doEvent
setMethod("doEvent",
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
setGeneric(
  "scheduleEvent",
  function(sim, eventTime, moduleName, eventType, eventPriority) {
    standardGeneric("scheduleEvent")
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(sim = "simList", eventTime = "numeric", moduleName = "character",
            eventType = "character", eventPriority = "numeric"),
  definition = function(sim, eventTime, moduleName, eventType, eventPriority) {

    if (length(eventTime)) {
      if (!is.na(eventTime)) {
        # if there is no metadata, meaning for the first
        #  "default" modules...load, save, checkpoint, progress
        if (!is.null(depends(sim)@dependencies[[1]])) {
          # first check if this moduleName matches the name of a module
          #  with meta-data (i.e., depends(sim)@dependencies filled)
          if (moduleName %in% sapply(
            depends(sim)@dependencies, function(x) { x@name })) {
            # If the eventTime doesn't have units, it's a user generated
            #  value, likely because of times in the simInit call.
            #  This must be intercepted, and units added based on this
            #  assumption, that the units are in \code{timeunit}
            if (is.null(attr(eventTime, "unit"))) {
              attributes(eventTime)$unit <- .callingFrameTimeunit(sim)
              eventTimeInSeconds <- convertTimeunit(
                  (eventTime -
                     convertTimeunit(start(sim),timeunit(sim), envir(sim))),
                  "seconds", envir(sim)
                ) +
                time(sim, "seconds") %>%
                as.numeric()
            } else {
              eventTimeInSeconds <- convertTimeunit(eventTime, "seconds", envir(sim)) %>%
                as.numeric()
            }
          } else { # for core modules because they have no metadata
            eventTimeInSeconds <- convertTimeunit(eventTime, "seconds", envir(sim)) %>%
              as.numeric()
          }
        } else { # when eventTime is NA... can't seem to get an example
          eventTimeInSeconds <- convertTimeunit(eventTime, "seconds", envir(sim)) %>%
            as.numeric()
        }
        attributes(eventTimeInSeconds)$unit <- "second"

        newEvent <- .emptyEventList(eventTime = eventTimeInSeconds,
                                    moduleName = moduleName,
                                    eventType = eventType,
                                    eventPriority = eventPriority)

        # if the event list is empty, set it to consist of newEvent and return;
        # otherwise, add newEvent and re-sort (rekey).
        evnts <- events(sim, "second")
        if (NROW(evnts) == 0L) {
          events(sim) <- setkey(newEvent, "eventTime", "eventPriority")
        } else {
          events(sim) <- rbindlist(list(evnts, newEvent)) %>%
            setkey("eventTime", "eventPriority")
        }
      }
    } else {
      warning(paste("Invalid or missing eventTime. This is usually caused by",
                    "an attempt to scheduleEvent at an empty eventTime or by",
                    "using an undefined parameter."))
    }

    return(invisible(sim))
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(sim = "simList", eventTime = "NULL", moduleName = "character",
            eventType = "character", eventPriority = "numeric"),
  definition = function(sim, eventTime, moduleName, eventType, eventPriority) {
    warning(paste("Invalid or missing eventTime. This is usually",
                  "caused by an attempt to scheduleEvent at time NULL",
                  "or by using an undefined parameter."))
    return(invisible(sim))
})

#' @rdname scheduleEvent
setMethod(
  "scheduleEvent",
  signature(sim = "simList", eventTime = "numeric", moduleName = "character",
            eventType = "character", eventPriority = "missing"),
  definition = function(sim, eventTime, moduleName, eventType, eventPriority) {
    scheduleEvent(sim = sim, eventTime = eventTime, moduleName = moduleName,
                  eventType = eventType, eventPriority = .normal())
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
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @param .plotInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
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
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Will print additional outputs informing the user of updates
#' to the values of various simList slot components.
#'
#' @export
#' @docType methods
#' @rdname spades
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
#'  # Can turn off plotting, and inspect the output simList instead
#'  out <- spades(mySim, .plotInitialTime = NA) # much faster
#'  completed(out) # shows completed events
#' }
#'
setGeneric("spades", function(sim, debug, .plotInitialTime, .saveInitialTime) {
    standardGeneric("spades")
})

#' @rdname spades
setMethod(
  "spades",
  signature(sim = "simList", debug = "logical", .plotInitialTime = "ANY",
            .saveInitialTime = "ANY"),
  definition = function(sim, debug, .plotInitialTime, .saveInitialTime) {

    if (missing(.plotInitialTime)) .plotInitialTime = NULL
    if (missing(.saveInitialTime)) .saveInitialTime = NULL

    if (!is.null(.plotInitialTime)) {
      if (!is.numeric(.plotInitialTime)) .plotInitialTime <- as.numeric(.plotInitialTime)
      paramsLocal <- params(sim)
      whNonHiddenModules <- !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <- lapply(paramsLocal[whNonHiddenModules], function(x) {
        x$.plotInitialTime <- .plotInitialTime
        x
      })
      params(sim) <- paramsLocal
    }
    if (!is.null(.saveInitialTime)) {
      if (!is.numeric(.saveInitialTime)) .saveInitialTime <- as.numeric(.saveInitialTime)
      paramsLocal <- params(sim)
      whNonHiddenModules <- !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <- lapply(paramsLocal[whNonHiddenModules], function(x) {
        x$.saveInitialTime <- NA_real_
        x
      })
      params(sim) <- paramsLocal
    }

    while (time(sim, "second") <= end(sim, "second")) {

      sim <- doEvent(sim, debug)  # process the next event

      # print debugging info: this can, and should, be more sophisticated;
      #  i.e., don't simply print the entire object
      if (debug) {
          print(sim)
      }
    }
    time(sim) <- end(sim, "second")
    return(invisible(sim))
})

#' @rdname spades
setMethod("spades",
          signature(sim = "simList", debug = "missing",
                    .plotInitialTime = "ANY", .saveInitialTime = "ANY"),
          definition = function(sim, .plotInitialTime, .saveInitialTime) {
            stopifnot(class(sim) == "simList")

            if (missing(.plotInitialTime)) .plotInitialTime = NULL
            if (missing(.saveInitialTime)) .saveInitialTime = NULL

            return(spades(sim, debug = FALSE, .plotInitialTime = .plotInitialTime ,
                          .saveInitialTime = .saveInitialTime))
})

