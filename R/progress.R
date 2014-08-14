################################################
###
### A PROGRESS BAR MODULE
###
###############################################

doEvent.progress = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # Check whether a .progress is specified in the simList
    if( !(".progress" %in% names(simParams(mySim))) ) {
      # default is to use text progress bar at 10% increments
      simParams(sim)[[".progress"]] = list(graphical = FALSE, interval=(simStopTime(sim)-simStartTime(sim))/10)
    }
    .pb <<- simProgress(sim)
    sim <- scheduleEvent(sim, 0.00, "progress", "set")
  } else if (eventType=="set") {
      # update progress bar
      setSimProgress(.pb, sim)

      # schedule the next save
      timeNextUpdate <- simCurrentTime(sim) + simParams(sim)$.progress$interval
      sim <- scheduleEvent(sim, timeNextUpdate, "progress", "set")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(sim)
}


##############################################################
#' Progress bar
#'
#' Shows a progress bar that is scaled to End simulation time.
#'
#' @author Alex Chubaty
#' @author Eliot McIntire
#'
#' @export
#' @docType methods
#' @rdname simProgress
#'
# @examples
# need examples
simProgress = function(sim) {
            try(close(.pb),silent = TRUE)
            OS <- tolower(Sys.info()["sysname"])
            if (simParams(sim)$.progress$graphical) {
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


setSimProgress <- function(.pb, sim) {
  OS <- tolower(Sys.info()["sysname"])
  if (simParams(sim)$.progress$graphical) {
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

