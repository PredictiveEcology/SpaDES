if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

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

    cur <- sim@current
    if (NROW(cur) == 0) {
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
    cur <- sim@current
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
                evnts1[1L, ] <- format(evnts1) %>%
                  stri_pad_right(., .spadesEnv[[".spadesDebugWidth"]])

                if (.spadesEnv[[".spadesDebugFirst"]]) {
                  evnts2 <- evnts1
                  evnts2[1L:2L, ] <- names(evnts1) %>%
                    stri_pad(., .spadesEnv[[".spadesDebugWidth"]]) %>%
                    rbind(., evnts1)
                  cat("This is the current event, printed as it is happening:\n")
                  write.table(evnts2, quote = FALSE, row.names = FALSE, col.names = FALSE)
                  .spadesEnv[[".spadesDebugFirst"]] <- FALSE
                } else {
                  colnames(evnts1) <- NULL
                  write.table(evnts1, quote = FALSE, row.names = FALSE)
                }
              }
            } else if (debug[[i]] == "simList") {
              print(sim)
            } else if (grepl(debug[[i]], pattern = "\\(")) {
              print(eval(parse(text = debug[[i]])))
            } else if (any(debug[[i]] == unlist(sim@modules))) {
              if (debug[[i]] == cur[["moduleName"]]) {
                debugonce(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
                on.exit(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
              }
            } else if (!any(debug[[i]] == c("step", "browser"))) {
              print(do.call(debug[[i]], list(sim)))
            }

            if (debug[[i]] == "step") {
              readline("Press any key to continue...")
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
                  } else if (is(a, "POSIXt")) {
                    cacheIt <- TRUE
                    notOlderThan <- a
                  }
                } else {
                  cacheIt <- TRUE
                }
              }
            }

            # This is to create a namespaced module call
            .modifySearchPath(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs,
                              removeOthers = FALSE)

            if (cacheIt) { # means that a module or event is to be cached
              objNam <- sim@depends@dependencies[[cur[["moduleName"]]]]@outputObjects$objectName
              moduleSpecificObjects <- c(grep(ls(sim@.envir, all.names = TRUE),
                                              pattern = cur[["moduleName"]], value = TRUE),
                                         na.omit(objNam))
              moduleSpecificOutputObjects <- objNam
              classOptions <- list(events = FALSE, current=FALSE, completed=FALSE, simtimes=FALSE,
                                   params = sim@params[[cur[["moduleName"]]]],
                                   modules = cur[["moduleName"]])
              
              sim <- Cache(FUN = get(moduleCall, envir = sim@.envir[[paste0("._", cur[["moduleName"]])]]),
                           sim = sim,
                           eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                           debug = debugDoEvent,
                           objects = moduleSpecificObjects,
                           notOlderThan = notOlderThan,
                           outputObjects = moduleSpecificOutputObjects,
                           classOptions = classOptions,
                           cacheRepo = sim@paths[["cachePath"]],
                           userTags = c("function:doEvent"))
            } else {
              sim <- get(moduleCall,
                         envir = sim@.envir[[paste0("._", cur[["moduleName"]])]])(sim, cur[["eventTime"]],
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
        if (NROW(sim@events)) {
          # i.e., if no more events
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
#'                       See \code{\link{priority}}.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @importFrom data.table setkey
#' @include priority.R
#' @export
#' @docType methods
#' @rdname scheduleEvent
#' @seealso \code{\link{priority}}
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
          "Invalid or missing eventTime. ",
          "This is usually caused by an attempt to scheduleEvent at an empty eventTime ",
          "or by using an undefined parameter."
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
#'              console at each event. See details. Default is \code{FALSE}.
#'
#' @param progress Logical (\code{TRUE} or \code{FALSE} show a graphical progress bar),
#'                 character (\code{"graphical"}, \code{"text"}) or numeric indicating
#'                 the number of update intervals to show in a graphical progress bar.
#'
#' @param cache Logical. If \code{TRUE}, then the \code{spades} call will be cached.
#'              This means that if the call is made again with the same simList,
#'              then `spades`` will return the return value from the previous run
#'              of that exact same simList. Default \code{FALSE}. See Details.
#'              See also the vignette on caching for examples.
#'
#' @param .plotInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param notOlderThan Date or time. Passed to \code{reproducible::Cache} to update the cache.
#'                     Default is \code{NULL}, meaning don't update the cache.
#'                     If \code{Sys.time()} is provided, then it will force a recache,
#'                     i.e., remove old value and replace with new value.
#'                     Ignored if \code{cache} is \code{FALSE}.
#'
#' @param ... Any. Can be used to make a unique cache identity, such as "replicate = 1".
#'            This will be included in the \code{Cache} call, so will be unique
#'            and thus \code{spades} will not use a cached copy as long as
#'            anything passed in \code{...} is unique, i.e., not cached previously.
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}, \code{\link[reproducible]{Cache}}
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
#' a previous run, even if there is stochasticity internally.
#' Caching is only based on the input simList. See also \code{experiment} for
#' the same mechanism, but it can be used with replication.
#' See also the vignette on caching for examples.
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
#' @author Alex Chubaty and Eliot McIntire
#' @docType methods
#' @export
#' @rdname spades
#' @seealso \code{\link{experiment}} for using replication with \code{spades}.
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
setGeneric("spades", function(sim, debug = FALSE, progress = NA, cache,
                              .plotInitialTime = NULL, .saveInitialTime = NULL,
                              notOlderThan = NULL, ...) {
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
      .modifySearchPath(.spadesEnv$searchPath, removeOthers = TRUE)
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
                        notOlderThan = NULL,
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
          cacheRepo = sim@paths$cachePath,
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
