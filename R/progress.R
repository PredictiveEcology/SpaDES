doEvent.progress = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    if (interactive()) {
      defaults = list(graphical=FALSE,
                      interval=(simStopTime(sim)-simStartTime(sim))/10)

      # Check whether a .progress is specified in the simList
      if ( is.null(simParams(sim)$.progress$graphical) &&
             is.null(simParams(sim)$.progress$interval) ) {
        simParams(sim)[[".progress"]] = defaults
      } else {
        ids = na.omit(match(names(simParams(sim)$.progress),
                            c("graphical", "interval")))
        simParams(sim)[[".progress"]][names(defaults)[-ids]] = defaults[-ids]
      }
    } else {
      # don't use progress bar when non-interactive (this is already set during simInit)
      simParams(sim)[[".progress"]] <- list(graphical=NA, interval=NA_real_)
    }

    # if NA then don't use progress bar
    if (any(!is.na(simParams(sim)$.progress))) {
      newProgressBar(sim)
      sim <- scheduleEvent(sim, simStartTime(sim), "progress", "set")
      sim <- scheduleEvent(sim, simStopTime(sim), "progress", "set")
    }
  } else if (eventType=="set") {
      # update progress bar
      setProgressBar(sim)

      # schedule the next save
      timeNextUpdate <- simCurrentTime(sim) + simParams(sim)$.progress$interval

      sim <- scheduleEvent(sim, timeNextUpdate, "progress", "set")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

##############################################################
#' Progress bar
#'
#' Shows a progress bar that is scaled to simulation stop time.
#'
#' The progress bar object is stored in a separate environment,
#' \code{.spadesEnv}.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @author Alex Chubaty
#' @author Eliot McIntire
#' @importFrom tcltk tkProgressBar
#' @include environment.R
#' @export
#' @docType methods
#' @rdname newProgressBar
newProgressBar <- function(sim) {
            if (exists(".pb", envir=.spadesEnv)) {
              close(get(".pb", envir=.spadesEnv))
              # rm(.pb, envir=.spadeEnv)
            }
            OS <- tolower(Sys.info()["sysname"])
            if (simParams(sim)$.progress$graphical) {
              if (OS=="windows") {
                pb <- winProgressBar(min = simStartTime(sim),
                                     max = simStopTime(sim),
                                     initial = simStartTime(sim))
              } else {
                pb <- tkProgressBar(min = simStartTime(sim),
                                    max = simStopTime(sim),
                                    initial = simStartTime(sim))
              }
            } else {
              pb <- txtProgressBar(min = simStartTime(sim),
                                   max = simStopTime(sim),
                                   initial = simStartTime(sim),
                                   char = ".", style = 3)
            }
            assign(".pb", pb, envir=.spadesEnv)
}

#' @importFrom tcltk setTkProgressBar
setProgressBar <- function(sim) {
  OS <- tolower(Sys.info()["sysname"])

  pb <- get(".pb", envir=.spadesEnv)
  if (simParams(sim)$.progress$graphical) {
    if (OS=="windows") {
      setWinProgressBar(pb, simCurrentTime(sim),
                        title=paste("Current simulation time:",
                                    simTimestepUnit(sim),
                                    simCurrentTime(sim),
                                    "of total", simStopTime(sim)))
    } else {
      setTkProgressBar(pb, simCurrentTime(sim),
                       title=paste("Current simulation time:",
                                   simTimestepUnit(sim),
                                   simCurrentTime(sim),
                                   "of total", simStopTime(sim)))
    }
  } else {
    setTxtProgressBar(pb, simCurrentTime(sim))
  }
  assign(".pb", pb, envir=.spadesEnv)
}
