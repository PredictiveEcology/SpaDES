##############################################################
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
#' of modules to be loaded for the simulation. **Note:** the module name
#' should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file \code{caribou.R},
#' located at the specified \code{path} (see below).
#'
#' @param path  An optional character string specifying the location of the module source files.
#'              If no path is specified, it defaults to the current working directory.
#'
#' @return A \code{simList} simulation object, pre-initialized from values specified
#' in the arguments supplied.
#'
#' @seealso \code{\link{spades}}.
#'
#' @include module-dependencies.R
#' @include simList.R
#' @export
#' @docType methods
#' @rdname simInit-method
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
setGeneric("simInit", function(times, params, modules, path) {
    standardGeneric("simInit")
})

#' @rdname simInit-method
#'
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="character"),
          definition=function(times, params, modules, path) {

            path <- checkPath(path, create=TRUE)

            # default modules
            defaults <- list("checkpoint", "save", "progress", "load")

            # parameters for default modules
            dotParamsReal = list(".saveInterval", ".saveInitialTime",
                                 ".plotInterval", ".plotInitialTime")
            dotParamsChar = list(".savePath", ".saveObjects")
            dotParams = append(dotParamsChar, dotParamsReal)

            # create new simDeps object
            assignSpaDES(".simDeps", new("simDeps"))

            # create new simList object in .spadesEnv
            sim <- new("simList", simtimes=list(current=times$start,
                                                start=times$start,
                                                stop=times$stop))
            simModules(sim) <- modules[!sapply(modules, is.null)]
            simParams(sim) <- params

            # load "default" modules
            for (d in defaults) {
              ### sourcing the code in each default module is already done
              ### because they are loaded with the package

              # add default module name to the loaded list:
              ### add module name to the loaded list
              simModulesLoaded(sim) <- append(simModulesLoaded(sim), d)

              # schedule each module's init event:
              sim <- scheduleEvent(sim, simStartTime(sim), d, "init")
            }

            # load user-defined modules
            for (m in simModules(sim)) {
                # source the code from each module's R file
                source(paste(path, "/", m, "/", m, ".R", sep=""), local=.GlobalEnv)

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

            simModules(sim) <- append(defaults, simModules(sim))

            # load files in the filelist
            if (is.null(simFileList(sim))) {
              sim <- loadFiles(sim, usedFileList=TRUE)
            } else {
              sim <- loadFiles(sim)
            }

            # check the parameters supplied by the user
            checkParams(sim, defaults, dotParams, path) # returns invisible TRUE/FALSE

            # summary of the simulation: modules, deps, etc.
            #data.frame()

            return(invisible(sim))
})

#' @rdname simInit-method
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="missing"),
          definition=function(times, params, modules) {
            simInit(times=times, params=params, modules=modules, path="./")
            return(invisible(sim))
})

##############################################################
#' Load modules for simulation.
#'
#' Checks the dependencies of the current module on other modules.
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
            return(FALSE)
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
          signature(sim="simList", depends="character"),
          definition=function(sim, depends) {
            return(!all(depends %in% simModulesLoaded(sim)))
})

##############################################################
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
#' @export
#' @keywords internal
#' @docType methods
#' @rdname doEvent-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setGeneric("doEvent", function(sim, debug) {
    standardGeneric("doEvent")
})

#' @rdname doEvent-method
setMethod("doEvent",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            # get next event
            nextEvent <- simEvents(sim)[1L, ] # extract the next event from queue

            # Catches the situation where no future event is scheduled, but StopTime is not reached
             if(any(is.na(nextEvent))) {
               simCurrentTime(sim) <- simStopTime(sim) + 1e-10
             } else {
              if (nextEvent$eventTime <= simStopTime(sim)) {
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
                    setkey(nextEvent, "eventTime")
                } else {
                  simCompleted(sim) <- if (debug) {
                    setkey(rbindlist(list(simCompleted(sim), nextEvent)), "eventTime")
                  } else {
                    head(setkey(rbindlist(list(simCompleted(sim), nextEvent)), "eventTime"), n=10)
                  }
                }
              } else {
                # update current simulated time to
                simCurrentTime(sim) <- simStopTime(sim) + 1e-10 # .Machine$double.eps
              }
            }
          return(invisible(sim))
})

#' @rdname doEvent-method
setMethod("doEvent",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            return(doEvent(sim, debug=FALSE))
})

##############################################################
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
#' @rdname scheduleEvent-method
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

#' @rdname scheduleEvent-method
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="numeric",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            if (length(eventTime)) {
              if (!is.na(eventTime)) {
                  newEvent <- as.data.table(list(eventTime=eventTime,
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

#' @rdname scheduleEvent-method
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="NULL",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            warning(paste("Invalid or missing eventTime. This is usually",
                          "caused by an attempt to scheduleEvent at time NULL",
                          "or by using an undefined parameter."))
            return(invisible(sim))
})

##############################################################
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
#' @seealso \code{\link{simInit}}.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Will print additional outputs informing the user of updates
#' to the values of various simList slot components.
#'
#' @export
#' @docType methods
#' @rdname spades-method
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

#' @rdname spades-method
setMethod("spades",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            while(simCurrentTime(sim) <= simStopTime(sim)) {
              sim <- doEvent(sim, debug)  # process the next event

              # print debugging info
              #  this can, and should, be more sophisticated;
              #  i.e., don't simply print the entire object
              if (debug) {
                  print(sim)
              }
            }
          return(invisible(sim))
})

#' @rdname spades-method
setMethod("spades",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            return(spades(sim, debug=FALSE))
})
