##############################################################
#' Progress bar.
#'
#' @param sim           A \code{SimList} simulation object.
#' 
#' @param eventTime    A numeric specifying the time of the next event.
#' 
#' @param eventType    A character string specifying the type of event:
#'                      one of either \code{"init"}, \code{"load"}, or \code{"save"}.
#' 
#' @param debug         Optional logical flag determines whether sim debug info
#'                      will be printed (default \code{debug=FALSE}.
#'
#' @return Returns the modified \code{SimList} object.
#' 
#' @author Alex Chubaty
#' @author Eliot McIntire
#' 
#' @export
#' @docType methods
#' @rdname progress
#'
# @examples
# need examples
doEvent.progress = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    if( !(".progress" %in% names(simParams(mySim))) ) {
      # default is to use graphical progress bar, so, need to set defaults
      simParams(sim)[[".progress"]] = list(graphical = TRUE, interval=(simStopTime(sim)-simStartTime(sim))/10)
    } 
    pb <<- simProgress(sim)
    sim <- scheduleEvent(sim, 0.00, "progress", "set")
  } else if (eventType=="set") {
      # update progress bar
      setSimProgress(pb, sim)
      
      # schedule the next save
      timeNextUpdate <- simCurrentTime(sim) + simParams(sim)$.progress$interval
      sim <- scheduleEvent(sim, timeNextUpdate, "progress", "set")
  } else {
    warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
  }
  return(sim)
}


#' @rdname simProgress
simProgress = function(sim) {
            close(pb)
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
            return(pb)
          }


setSimProgress <- function(pb, sim) {
  OS <- tolower(Sys.info()["sysname"])
  if (simParams(sim)$.progress$graphical) {
    if (OS=="windows") {
      setWinProgressBar(pb, simCurrentTime(sim), title = paste("Current simulation time",simCurrentTime(sim),
                                                               "of total", simStopTime(sim)))
    } else {
      setTkProgressBar(pb, simCurrentTime(sim), title = paste("Current simulation time",simCurrentTime(sim),
                                                              "of total", simStopTime(sim)))
    }
  } else {
    setTxtProgressBar(pb, simCurrentTime(sim))
  }
  
  
}

