if(getRversion() >= "3.1.0")  utils::globalVariables(c("rng.kind", "rng.state"))

##############################################################
#' Simulation checkpoints.
#'
#' Save and reload the current state of the simulation,
#' including the state of the random number generator,
#' by scheduling checkpoint events.
#'
#' \code{\link{checkpointLoad}} and \code{\link{checkpointSave}} code from:
#' https://raw.githubusercontent.com/achubaty/r-tools/master/checkpoint.R
#'
#' RNG save code adapted from:
#' http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
#' https://stackoverflow.com/questions/13997444/
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
#' @seealso \code{\link{.Random.seed}}.
#'
#' @author Alex Chubaty
#'
#' @export
#' @docType methods
#' @rdname checkpoint
#'
# @examples
# need examples
doEvent.checkpoint = function(sim, eventTime, eventType, debug=FALSE) {
  ### determine whether to use checkpointing
  ### default is not to use checkpointing if unspecified
  if ( !(".checkpoint" %in% names(simParams(sim))) ) {
    simParams(sim)$.checkpoint = list(interval=NA_real_, file=NULL)
  }
  useChkpnt = !any(is.na(simParams(sim)$.checkpoint))

  ### determine checkpoint file location, for use in events below
  if (useChkpnt) {
    if (is.null(simCheckpointFile(sim))) {
      checkpointFile <- "checkpoint.RData"
    } else {
      checkpointFile <- simCheckpointFile(sim)
    }
    if (!is.null(simGlobalsOutputPath(sim))) {
      checkpointDir <- checkPath(simGlobalsOutputPath(sim), create=TRUE)
      checkpointFile <- file.path(checkpointDir, simCheckpointFile(sim))
    }
  }

  ### event definitions
  if (eventType=="init") {
    if (useChkpnt) {
      sim <- scheduleEvent(sim, 0.00, "checkpoint", "load")
    }
  } else if (eventType=="load") {
    if (useChkpnt) {
      # load user-specified checkpoint options
      checkpointLoad(checkpointFile)

      # schedule the next save
      timeNextSave <- simCurrentTime(sim) + simCheckpointInterval(sim)
      sim <- scheduleEvent(sim, timeNextSave, "checkpoint", "save")
    }
  } else if (eventType=="save") {
    if (useChkpnt) {
      checkpointSave(checkpointFile)

      # schedule the next save
      timeNextSave <- simCurrentTime(sim) + simCheckpointInterval(sim)
      sim <- scheduleEvent(sim, timeNextSave, "checkpoint", "save")
    }
  } else {
    warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))

  }
  return(invisible(sim))
}

#' @param file The checkpoint file.
#' @rdname checkpoint
checkpointLoad = function(file) {
  # check for previous checkpoint file
  if (file.exists(file)) {
    load(file)
    if (exists(".Random.seed")) {
      do.call("RNGkind", as.list(rng.kind))
      assign(".Random.seed", rng.state, .GlobalEnv)
    }
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}

#' @rdname checkpoint
checkpointSave = function(file) {
  if (exists(".Random.seed"))  {
    assign("rng.state", get(".Random.seed", .GlobalEnv), .GlobalEnv)
    assign("rng.kind", RNGkind(), .GlobalEnv)
  }
  save.image(file) # saves entire workspace
  invisible(TRUE) # return "success" invisibly
}
