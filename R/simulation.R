if (getRversion() >= "3.1.0") utils::globalVariables(".")

################################################################################
#' Initialize a new simulation
#'
#' Create a new simulation object, preloaded with parameters, modules, times, etc.
#'
#' We implement a discrete event simulation in a more modular fashion so it's
#' easier to add submodules to the simulation. We use S4 classes and methods,
#' and use \code{data.table} instead of \code{data.frame} to implement the event
#' queue (because it is much faster).
#'
#' @param times A named list of numeric simulation start and stop times
#'        (e.g., \code{times=list(start=0.0, stop=10.0)}).
#'
#' @param params A named list of simulation parameters and their values.
#'
#' @param modules A named list of character strings specfying the names
#' of modules to be loaded for the simulation. Note: the module name
#' should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file
#' \code{caribou.R}, located at the specified \code{path} (see below).
#'
#' @param objects A list of data objects to be used in the simulation.
#'
#' @param path  An optional character string specifying the location of the module source files.
#'              If no path is specified, it defaults to the current working directory.
#'
#' @param loadOrder  An optional list of module names specfiying the order in
#'                   which to load the modules. If not specified, the module
#'                   load order will be determined automatically.
#'
#' @return A \code{simList} simulation object, pre-initialized from values
#' specified in the arguments supplied.
#'
#' @seealso \code{\link{spades}}.
#'
#' @include module-dependencies-class.R
#' @include simList.R
#' @include environment.R
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname simInit
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#'  modules=list("habitat", "caribou"), path="/path/to/my/modules/")
#'  mySim
#' }
#'
setGeneric("simInit", function(times, params, modules, objects, path, loadOrder) {
    standardGeneric("simInit")
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list", objects="list",
                    path="character", loadOrder="character"),
          definition=function(times, params, modules, objects, path, loadOrder) {

            path <- checkPath(path, create=TRUE)

            # core modules
            core <- list("checkpoint", "save", "progress", "load")

            # parameters for core modules
            dotParamsReal <- list(".saveInterval", ".saveInitialTime",
                                 ".plotInterval", ".plotInitialTime")
            dotParamsChar <- list(".savePath", ".saveObjects")
            dotParams <- append(dotParamsChar, dotParamsReal)

            sim <- new("simList")

            simTimes(sim) <- list(current=times$start, start=times$start, stop=times$stop)
            simModules(sim) <- modules[!sapply(modules, is.null)]

            # assign some core & global params only for now
            if (".outputPath" %in% names(params$.globals)) {
              params$.globals$.outputPath <- checkPath(params$.globals$.outputPath, TRUE)
            }
            simGlobals(sim) <- params$.globals

            # load core modules
            for (c in core) {
              ### sourcing the code in each core module is already done
              ### because they are loaded with the package

              # add core module name to the loaded list:
              simModulesLoaded(sim) <- append(simModulesLoaded(sim), c)

              # schedule each module's init event:
              sim <- scheduleEvent(sim, simStartTime(sim), c, "init")
            }

            # source module metadata and code files
            for (m in simModules(sim)) {
              filename <- paste(path, "/", m, "/", m, ".R", sep="")
              parsedFile <- parse(filename)
              defineModuleItem <- grepl(pattern="defineModule", parsedFile)

              # evaluated only the 'defineModule' function of parsedFile
              sim <- eval(parsedFile[defineModuleItem])

              # check that modulename == filename
              fname <- unlist(strsplit(basename(filename), "[.][r|R]$"))
              i <- which(simModules(sim)==m)
              mname <- simDepends(sim)@dependencies[[i]]@name
              if (fname != mname) stop("module name \'", mname, "\'",
                                       "does not match filename \'", fname, "\'")

              # assign default param values
              apply(simDepends(sim)@dependencies[[i]]@parameters, 1, function(x) {
                if (is.character(x$default)) {
                  tt <- paste0("simParams(sim)$", m, "$", x$name, "<<-\"", x$default, "\"")
                } else {
                  tt <- paste0("simParams(sim)$", m, "$", x$name, "<<-", x$default)
                }
                eval(parse(text=tt), envir=environment())
              })

              # evaluate the rest of the parsed file
              eval(parsedFile[!defineModuleItem], envir=simEnv(sim))
            }

            # timestepUnit has no meaning until all modules are loaded, so this has to be after loading
            simTimestepUnit(sim) <- if(!is.null(times$timestepUnit)) {
              times$timestepUnit
            } else {
              smallestTimestepUnit(sim)
            }

            # assign user-specified non-global params, while
            # keeping defaults for params not specified by user
            omit <- c(which(core=="load"), which(core=="save"))
            pnames <- c(paste0(".", core[-omit]), names(simParams(sim)))

            if ( (is.null(params$.progress)) || (any(is.na(params$.progress))) ) {
              params$.progress <- list(graphical=NA, interval=NA_real_)
            }

            tmp <- list()
            lapply(pnames, function(x) {
              tmp[[x]] <<- updateList(simParams(sim)[[x]], params[[x]])
            })
            simParams(sim) <- tmp

            # check user-supplied load order
            if ( length(loadOrder) && all(modules %in% loadOrder) && all(loadOrder %in% modules) ) {
              simModulesLoadOrder(sim) <- loadOrder
            } else {
              simModulesLoadOrder(sim) <- depsGraph(sim, plot=FALSE) %>% .depsLoadOrder(sim, .)
            }


            # load user-defined modules
            for (m in simModulesLoadOrder(sim)) {
              # schedule each module's init event:
              sim <- scheduleEvent(sim, simStartTime(sim), m, "init")

              ### add module name to the loaded list
              simModulesLoaded(sim) <- append(simModulesLoaded(sim), m)

              ### add NAs to any of the dotParams that are not specified by user
              # ensure the modules sublist exists by creating a tmp value in it
              if(is.null(simParams(sim)[[m]])) {
                simParams(sim)[[m]] <- list(.tmp=NA_real_)
              }

              # add the necessary values to the sublist
              for(x in dotParamsReal) {
                if (is.null(simParams(sim)[[m]][[x]])) {
                  simParams(sim)[[m]][[x]] <- NA_real_
                } else if (is.na(simParams(sim)[[m]][[x]])) {
                  simParams(sim)[[m]][[x]] <- NA_real_
                }
              }

              # remove the tmp value from the module sublist
              simParams(sim)[[m]]$.tmp <- NULL

              ### Currently, everything in dotParamsChar is being checked for NULL
              ### values where used (i.e., in save.R).
            }

            sim$.sessionInfo <- sessionInfo()

            simModules(sim) <- append(core, simModulesLoadOrder(sim))

            # load files in the filelist
            if(!is.null(params$.load$fileList)) {
              simFileList(sim) <- params$.load$fileList
            }

            if (is.null(simFileList(sim))) {
              sim <- loadFiles(sim, usedFileList=TRUE)
            } else {
              sim <- loadFiles(sim)
            }

            # check the parameters supplied by the user
            checkParams(sim, core, dotParams, path) # returns invisible TRUE/FALSE

            if(length(objects)>0) {
              changeObjEnv(x=objects, toEnv=simEnv(sim), fromEnv=.GlobalEnv,
                           rmSrc=getOption("spades.lowMemory"))
            }

            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="list", path="character", loadOrder="missing"),
          definition=function(times, params, modules, objects, path) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=objects, path=path, loadOrder=character())
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="list", path="missing", loadOrder="character"),
          definition=function(times, params, modules, objects, loadOrder) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=objects, path="./", loadOrder=loadOrder)
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="missing", path="character", loadOrder="character"),
          definition=function(times, params, modules, path, loadOrder) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=list(), path=path, loadOrder=loadOrder)
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="list", path="missing", loadOrder="missing"),
          definition=function(times, params, modules, objects) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=objects, path="./", loadOrder=character())
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="missing", path="missing", loadOrder="character"),
          definition=function(times, params, modules, loadOrder) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=list(), path="./", loadOrder=loadOrder)
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="missing", path="character", loadOrder="missing"),
          definition=function(times, params, modules, path) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=list(), path=path, loadOrder=character())
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="list", params="list", modules="list",
                    objects="missing", path="missing", loadOrder="missing"),
          definition=function(times, params, modules) {
            sim <- simInit(times=times, params=params, modules=modules,
                           objects=list(), path="./", loadOrder=character())
            return(invisible(sim))
})

#' @rdname simInit
setMethod("simInit",
          signature(times="missing", params="missing", modules="missing",
                    objects="missing", path="missing", loadOrder="missing"),
          definition=function() {
            sim <- simInit(times=list(start=0, stop=1),
                           params=list(),
                           modules=list(),
                           objects=list(), path="./", loadOrder=character())
            return(invisible(sim))
})

################################################################################
#' Load modules for simulation (deprecated).
#'
#' DEPRECATED. Checks the dependencies of the current module on other modules.
#' These dependencies need to be loaded first, so if they are not
#' already loaded, hold off loading the current module until after
#' dependencies are loaded.
#'
#' @param sim     A \code{simList} simulation object.
#'
#' @param depends A list of character strings specifying the names
#'                of modules upon which the current module depends.
#'
#' @return \code{Logical}.
#'
#' @seealso \code{\link{library}}.
#'
#' @export
#' @docType methods
#' @rdname loadmodules
#'
#' @author Alex Chubaty
#'
setGeneric("reloadModuleLater", function(sim, depends) {
  standardGeneric("reloadModuleLater")
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
          signature(sim="simList", depends="NULL"),
            definition=function(sim, depends) {
              stopifnot(class(sim) == "simList")
              return(FALSE)
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
        signature(sim="simList", depends="character"),
          definition=function(sim, depends) {
            stopifnot(class(sim) == "simList")
            # deprecated in v0.6.0
            .Deprecated(msg=paste0("Warning: 'reloadModuleLater' is deprecated.\n",
                        "Module dependencies should be specified using 'defineModule'"))
            return(!all(depends %in% simModulesLoaded(sim)))
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
#' @import data.table
#' @importFrom fpCompare '%<=%'
#' @importFrom magrittr '%>%'
#' @export
#' @keywords internal
#' @docType methods
#' @rdname doEvent
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setGeneric("doEvent", function(sim, debug) {
    standardGeneric("doEvent")
})

#' @rdname doEvent
setMethod("doEvent",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            stopifnot(class(sim) == "simList")

            # get next event
            nextEvent <- simEvents(sim)[1L, ] # extract the next event from queue

            # Catches the situation where no future event is scheduled, but StopTime is not reached
            if(any(is.na(nextEvent))) {
               simCurrentTime(sim) <- simStopTime(sim) + 2*getOption("fpCompare.tolerance")
            } else {
              if (nextEvent$eventTime %<=% simStopTime(sim)) {
                # update current simulated time
                simCurrentTime(sim) <- nextEvent$eventTime

                # call the module responsible for processing this event
                moduleCall <- paste("doEvent", nextEvent$moduleName, sep=".")

                # check the module call for validity
                if(nextEvent$moduleName %in% simModules(sim)) {
                  sim <- get(moduleCall)(sim, nextEvent$eventTime, nextEvent$eventType, debug)
                } else {
                  stop(paste("Invalid module call. The module `", nextEvent$moduleName,
                             "` wasn't specified to be loaded."))
                }

                # now that it is run, without error, remove it from the queue
                simEvents(sim) <- simEvents(sim)[-1L,]

                # add to list of completed events
                if(length(simCompleted(sim))) {
                  completed <- list(simCompleted(sim), nextEvent) %>%
                    rbindlist %>%
                    setkey("eventTime")
                  if (!debug) completed <- tail(completed, n=getOption("spades.nCompleted"))
                } else {
                  completed <- setkey(nextEvent, "eventTime")
                }
                simCompleted(sim) <- completed
              } else {
                # update current simulated time to
                simCurrentTime(sim) <- simStopTime(sim) + 2*getOption("fpCompare.tolerance")
              }
            }
            return(invisible(sim))
})

#' @rdname doEvent
setMethod("doEvent",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            stopifnot(class(sim) == "simList")
            return(doEvent(sim, debug=FALSE))
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
#' @param sim          A \code{simList} simulation object.
#'
#' @param eventTime    A numeric specifying the time of the next event.
#'
#' @param moduleName   A character string specifying the module from which to call the event.
#'
#' @param eventType    A character string specifying the type of event from within the module.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname scheduleEvent
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{scheduleEvent(x, 10.5, "firemodule", "burn")}
setGeneric("scheduleEvent", function(sim, eventTime, moduleName, eventType) {
    standardGeneric("scheduleEvent")
})

#' @rdname scheduleEvent
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="numeric",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            if (length(eventTime)) {
              if (!is.na(eventTime)) {
                # if there is no metadata, meaning for the first
                #  "default" modules...load, save, checkpoint, progress
                if(!is.null(simDepends(sim)@dependencies[[1]])){
                  # first check if this moduleName matches the name of a module with meta-data
                  #   (i.e., simDepends(sim)@dependencies filled)
                  if (moduleName %in% sapply(simDepends(sim)@dependencies,function(x) x@name)) {
                    eventTimeIncrementSec <- (eventTime - simCurrentTime(sim))*
                      timestepInSeconds(sim, moduleName)

                    eventTimeInLargestUnit <- suppressMessages(simCurrentTime(sim)+
                      eventTimeIncrementSec/as.numeric(
                        eval(parse(text=paste0("d",simTimestepUnit(sim),"(1)")))))
                  } else {
                    eventTimeInLargestUnit <- eventTime
                  }
                } else {
                  eventTimeInLargestUnit <- eventTime
                }

                newEvent <- as.data.table(list(eventTime=eventTimeInLargestUnit,
                                              moduleName=moduleName,
                                              eventType=eventType))

                # if the event list is empty, set it to consist of newEvent and return;
                # otherwise, add newEvent and re-sort (rekey).
                if (length(simEvents(sim))==0L) {
                  simEvents(sim) <- setkey(newEvent, "eventTime")
                } else {
                  simEvents(sim) <- setkey(rbindlist(list(simEvents(sim), newEvent)), "eventTime")
                }
              }
            } else {
                warning(paste("Invalid or missing eventTime. This is usually",
                                "caused by an attempt to scheduleEvent at an empty eventTime",
                                "or by using an undefined parameter."))
            }


            return(invisible(sim))
})

#' @rdname scheduleEvent
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="NULL",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            stopifnot(class(sim) == "simList")
            warning(paste("Invalid or missing eventTime. This is usually",
                          "caused by an attempt to scheduleEvent at time NULL",
                          "or by using an undefined parameter."))
            return(invisible(sim))
})

################################################################################
#' Convert an scheduled event time to seconds
#'
#' Ensure all events from modules are working in the same time units.
#'
#' @param sim          A \code{simList} simulation object.
#'
#' @param moduleName   Character string specifying the module from which to call the event.
#'
#' @return Returns the eventTime in seconds, based on the default \code{timestepUnit} of
#' the \code{moduleName}.
#'
#' @export
#' @docType methods
#' @rdname timestepInSeconds
#'
#' @author Eliot McIntire
#'
setGeneric("timestepInSeconds", function(sim, moduleName) {
  standardGeneric("timestepInSeconds")
})

#' @rdname timestepInSeconds
setMethod("timestepInSeconds",
          signature(sim="simList", moduleName="character"),
          definition=function(sim, moduleName) {
            a <- sapply(simDepends(sim)@dependencies, function(x) x@name)
            wh <- which(a==moduleName)
            timestepUnit <- simDepends(sim)@dependencies[[wh]]@timestepUnit

            if(is.character(timestepUnit)) {
              return(as.numeric(eval(parse(text=paste0("d",timestepUnit,"(1)")))))
            }

            if(is.na(timestepUnit)) {
              return(as.numeric(eval(parse(text=paste0("d",simTimestepUnit(sim),"(1)")))))
            } else {
              return(timestepUnit)
            }
})

################################################################################
#' Determine what the smallest timestepUnit in a simObject
#'
#' When modules have different timestepUnit, SpaDES automatically takes the
#' largest (e.g., "year") as the unit for a simulation. This function determines which
#' is the largest unit
#'
#' @param sim          A \code{simList} simulation object.
#'
#' @return The timestepUnit as a character string
#'
#' @export
#' @docType methods
#' @rdname smallestTimestepUnit
#'
#' @author Eliot McIntire
#'
setGeneric("smallestTimestepUnit", function(sim) {
  standardGeneric("smallestTimestepUnit")
})

#' @rdname smallestTimestepUnit
setMethod("smallestTimestepUnit",
          signature(sim="simList"),
          definition=function(sim) {
  if(!is.null(simDepends(sim)@dependencies[[1]])) {

    timesteps <- lapply(simDepends(sim)@dependencies, function(x) x@timestepUnit)
    #timesteps[!sapply(timesteps, is.na)] <-
    #  lapply(timesteps[!sapply(timesteps, is.na)], function(x) x[grepl(pattern="[^s]$", x)] <- paste0(x,"s"))
    if(all(sapply(timesteps, is.na))) {
      return(NA_character_)
    } else {
      return(timesteps[!is.na(timesteps)][[which.min(sapply(timesteps[!sapply(timesteps, is.na)],
                                         function(ts) eval(parse(text=paste0("d",ts,"(1)")))))]])

    }
  }
  return(NA_character_)
})
################################################################################
#' Run a spatial discrete event simulation
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
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Will print additional outputs informing the user of updates
#' to the values of various simList slot components.
#'
#' @importFrom fpCompare '%<=%'
#' @export
#' @docType methods
#' @rdname spades
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#'                  modules=list("habitat", "caribou"), path="/path/to/my/modules/)
#' spades{mySim}
#' }
#'
setGeneric("spades", function(sim, debug) {
    standardGeneric("spades")
})

#' @rdname spades
setMethod("spades",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            envName <- paste("SpaDES", deparse(substitute(sim)), sep="_")
            attach(simEnv(sim), name=envName)
            on.exit(detach(pos=match(envName, search())))
            while(simCurrentTime(sim) %<=% simStopTime(sim)) {
              sim <- doEvent(sim, debug)  # process the next event

              # print debugging info:
              #  this can, and should, be more sophisticated;
              #  i.e., don't simply print the entire object
              if (debug) {
                  print(sim)
              }
            }
            return(invisible(sim))
})

#' @rdname spades
setMethod("spades",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            stopifnot(class(sim) == "simList")
            return(spades(sim, debug=FALSE))
})

################################################################################
#' New time units
#'
#' SpaDES commonly needs generic durations, like "year" which will round neatly over
#' a century (i.e., leap years are added within each year with an extra 1/4 day,
#' i.e., year=365.25 days), months are defined as year/12, weeks as year/52. This
#' year is also known as the "astronomical" or "Julian" year.
#'
#' @param x numeric. Number of the desired units
#'
#' @return Number of seconds within each unit
#'
#' @export
#' @docType methods
#' @rdname spadesTimeUnits
#'
#' @author Eliot McIntire
#' @examples
#' dsecond(1)
#' dhour(1)
#' dday(1)
#' dweek(1)
#' dmonth(1)
#' dyear(1)
#'
setGeneric("dyears", function(x) {
  standardGeneric("dyears")
})

#' @importFrom lubridate new_duration
#' @rdname spadesTimeUnits
setMethod("dyears",
          signature(x="numeric"),
          definition=function(x){
  lubridate::new_duration(x * 60 * 60 * 24 * 365.25)
})

#' @inheritParams dyears
#' @export
#' @rdname spadesTimeUnits
setGeneric("dmonths", function(x) {
  standardGeneric("dmonths")
})

#' @rdname spadesTimeUnits
setMethod("dmonths",
          signature(x="numeric"),
          definition=function(x){
            lubridate::new_duration(x * as.numeric(SpaDES::dyears(1))/12)
})

#' @inheritParams dyears
#' @export
#' @aliases dweek
#' @rdname spadesTimeUnits
setGeneric("dweeks", function(x) {
  standardGeneric("dweeks")
})

#' @export
#' @rdname spadesTimeUnits
setMethod("dweeks",
          signature(x="numeric"),
          definition=function(x){
            lubridate::new_duration(x * as.numeric(SpaDES::dyears(1))/52)
})

#' @export
#' @rdname spadesTimeUnits
dweek <- function(x) {
  dweeks(x)
}

#' @export
#' @rdname spadesTimeUnits
dmonth <- function(x) {
  dmonths(x)
}

#' @export
#' @rdname spadesTimeUnits
dyear <- function(x) {
  dyears(x)
}

#' @export
#' @rdname spadesTimeUnits
#' @importFrom lubridate dseconds
dsecond <- function(x) {
  lubridate::dseconds(x)
}

#' @export
#' @rdname spadesTimeUnits
#' @importFrom lubridate ddays
dday <- function(x) {
  lubridate::ddays(x)
}

#' @export
#' @rdname spadesTimeUnits
#' @importFrom lubridate dhours
dhour <- function(x) {
  lubridate::dhours(x)
}

#' @inheritParams dyears
#' @export
#' @rdname spadesTimeUnits
setGeneric("dNA", function(x) {
  standardGeneric("dNA")
})

#' @rdname spadesTimeUnits
setMethod("dNA",
          signature(x="ANY"),
          definition=function(x){
            lubridate::new_duration(0)
})
