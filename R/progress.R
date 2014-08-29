if(getRversion() >= "3.1.0")  utils::globalVariables(".pb")

################################################
###
### A PROGRESS BAR MODULE
###
###############################################

doEvent.progress = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # Check whether a .progress is specified in the simList
    defaults = list(.graphical = FALSE, .progressInterval=(simStopTime(sim)-simStartTime(sim))/10)

    if( !(".progress" %in% names(simParams(sim))) ) {
      simParams(sim)[[".progress"]] = defaults
    } else {
      ids = na.omit(match(names(simParams(sim)$.progress),c(".graphical", ".progressInterval")))
      simParams(sim)[[".progress"]][names(defaults)[-ids]] = defaults[-ids]
    }

    # if NA then don't use progress bar
    if (any(!is.na(simParams(sim)$.progress))) {
      .pb <<- newProgressBar(sim)
      sim <- scheduleEvent(sim, 0.00, "progress", "set")
    }
  } else if (eventType=="set") {
      # update progress bar
      setProgressBar(sim, .pb)

      # schedule the next save
      timeNextUpdate <- simCurrentTime(sim) + simParams(sim)$.progress$.progressInterval

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
#' @param sim A \code{simList} simulation object.
#'
#' @author Alex Chubaty
#' @author Eliot McIntire
#'
#' @export
#' @docType methods
#' @rdname newProgressBar
#'
# @examples
# need examples
newProgressBar <- function(sim) {
            try(close(.pb),silent = TRUE)
            OS <- tolower(Sys.info()["sysname"])
            if (simParams(sim)$.progress$.graphical) {
              if (OS=="windows") {
                .pb <- winProgressBar(min = simStartTime(sim),
                                     max = simStopTime(sim),
                                     initial = simStartTime(sim))
              } else {
                .pb <- tkProgressBar(min = simStartTime(sim),
                                    max = simStopTime(sim),
                                    initial = simStartTime(sim))
              }
            } else {
              .pb <- txtProgressBar(min = simStartTime(sim),
                                   max = simStopTime(sim),
                                   initial = simStartTime(sim),
                                   char = ".", style = 3)
            }
            return(.pb)
}


setProgressBar <- function(sim, .pb) {
  OS <- tolower(Sys.info()["sysname"])
  if (simParams(sim)$.progress$.graphical) {
    if (OS=="windows") {
      setWinProgressBar(.pb, simCurrentTime(sim), title = paste("Current simulation time",simCurrentTime(sim),
                                                               "of total", simStopTime(sim)))
    } else {
      setTkProgressBar(.pb, simCurrentTime(sim), title = paste("Current simulation time",simCurrentTime(sim),
                                                              "of total", simStopTime(sim)))
    }
  } else {
    setTxtProgressBar(.pb, simCurrentTime(sim))
  }
}

