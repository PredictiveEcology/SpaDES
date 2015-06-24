doEvent.progress = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    if (interactive()) {
       defaults = list(graphical=FALSE,
                       interval=(end(sim)-start(sim))/10)

      # Check whether a .progress is specified in the simList
      if ( is.null(params(sim)$.progress$graphical) &&
             is.null(params(sim)$.progress$interval) ) {
        params(sim)[[".progress"]] = defaults
      } else {
        ids = na.omit(match(names(params(sim)$.progress),
                            c("graphical", "interval")))
        params(sim)[[".progress"]][names(defaults)[-ids]] = defaults[-ids]
      }
    } else {
      # don't use progress bar when non-interactive (this is already set during simInit)
      params(sim)[[".progress"]] <- list(graphical=NA, interval=NA_real_)
    }

    # if NA then don't use progress bar
    if (any(!is.na(params(sim)$.progress))) {
      newProgressBar(sim)
      sim <- scheduleEvent(sim, start(sim, "seconds"), "progress", "set")
      sim <- scheduleEvent(sim, end(sim, "seconds"), "progress", "set")
    }
  } else if (eventType=="set") {
      # update progress bar
      setProgressBar(sim)

      # schedule the next save
      timeNextUpdate <- time(sim, timeunit(sim)) +
        params(sim)$.progress$interval

      sim <- scheduleEvent(sim, timeNextUpdate, "progress", "set")
  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
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
            if (params(sim)$.progress$graphical) {
              if (OS=="windows") {
                pb <- winProgressBar(min = start(sim, timeunit(sim)),
                                     max = end(sim, timeunit(sim)),
                                     initial = start(sim, timeunit(sim)))
              } else {
                pb <- tkProgressBar(min = start(sim, timeunit(sim)),
                                    max = end(sim, timeunit(sim)),
                                    initial = start(sim, timeunit(sim)))
              }
            } else {
              pb <- txtProgressBar(min = start(sim, timeunit(sim)),
                                   max = end(sim, timeunit(sim)),
                                   initial = start(sim, timeunit(sim)),
                                   char = ".", style = 3)
            }
            assign(".pb", pb, envir=.spadesEnv)
}

#' @importFrom tcltk setTkProgressBar
setProgressBar <- function(sim) {
  OS <- tolower(Sys.info()["sysname"])

  pb <- get(".pb", envir=.spadesEnv)
  if (params(sim)$.progress$graphical) {
    if (OS=="windows") {
      setWinProgressBar(pb, time(sim, timeunit(sim)),
                        title=paste("Current simulation time:",
                                    timeunit(sim),
                                    round(time(sim, timeunit(sim)), 3),
                                    "of total", end(sim, timeunit(sim))))
    } else {
      setTkProgressBar(pb, time(sim, timeunit(sim)),
                       title=paste("Current simulation time:",
                                   timeunit(sim),
                                   round(time(sim, timeunit(sim)), 3),
                                   "of total", end(sim, timeunit(sim))))
    }
  } else {
    setTxtProgressBar(pb, round(time(sim, timeunit(sim)), 3))
  }
  assign(".pb", pb, envir=.spadesEnv)
}
