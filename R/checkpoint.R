################################################################################
#' Simulation checkpoints.
#'
#' Save and reload the current state of the simulation,
#' including the state of the random number generator,
#' by scheduling checkpoint events.
#'
#' \code{\link{checkpointLoad}} and \code{\link{.checkpointSave}} code based on:
#' \url{https://raw.githubusercontent.com/achubaty/r-tools/master/checkpoint.R}
#'
#' RNG save code adapted from:
#' \url{http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/}
#' and \url{https://stackoverflow.com/questions/13997444/}
#'
#' @param sim           A \code{simList} simulation object.
#'
#' @param eventTime    A numeric specifying the time of the next event.
#'
#' @param eventType      A character string specifying the type of event: one of
#'                       either \code{"init"}, \code{"load"}, or \code{"save"}.
#'
#' @param debug         Optional logical flag determines whether sim debug info
#'                      will be printed (default \code{debug = FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @seealso \code{\link{.Random.seed}}.
#'
#' @author Alex Chubaty
#'
#' @include environment.R
#' @include priority.R
#' @importFrom R.utils isAbsolutePath
#' @importFrom reproducible checkPath
#' @export
#' @docType methods
#' @rdname checkpoint
#'
doEvent.checkpoint <- function(sim, eventTime, eventType, debug = FALSE) {
  ### determine whether to use checkpointing
  ### default is not to use checkpointing if unspecified
  ### - this default is set when a new simList object is initialized

  useChkpnt <- !any(is.na(P(sim, ".checkpoint")))

  ### determine checkpoint file location, for use in events below
  if (useChkpnt) {
    if (is.null(checkpointFile(sim))) {
      checkpointFile <- "checkpoint.RData"
    } else {
      checkpointFile <- checkpointFile(sim)
    }

    if (isAbsolutePath(checkpointFile(sim))) {
      checkpointDir <- checkPath(dirname(checkpointFile(sim)), create = TRUE)
    } else {
      checkpointDir <- checkPath(outputPath(sim), create = TRUE)
    }

    checkpointFile <- file.path(checkpointDir, basename(checkpointFile(sim)))
  }

  ### event definitions
  if (eventType == "init") {
    if (useChkpnt) {
      sim <- scheduleEvent(sim, 0.00, "checkpoint", "save", .last())
    }
  } else if (eventType == "save") {
    if (useChkpnt) {
      .checkpointSave(sim, checkpointFile)

      # schedule the next save
      timeNextSave <- time(sim, timeunit(sim)) + checkpointInterval(sim)
      sim <- scheduleEvent(sim, timeNextSave, "checkpoint", "save", .last())
    }
  } else {
    warning(paste(
      "Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  }
  return(invisible(sim))
}

#' @param file The checkpoint file.
#' @rdname checkpoint
#' @export
checkpointLoad <- function(file) {
  #f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  #fobj <- paste0(f, "_objs", ".RData")

  # check for previous checkpoint files
  if (file.exists(file)) { # && file.exists(fobj)) {
    simListName <- load(file, envir = .GlobalEnv)
    sim <- get(simListName, envir = .GlobalEnv)
    #load(fobj, envir = sim@.envir)

    do.call("RNGkind", as.list(sim$.rng.kind))
    assign(".Random.seed", sim$.rng.state, envir = .GlobalEnv)
    rm(list = c(".rng.kind", ".rng.state", ".timestamp"), envir = sim@.envir)
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}

#' @rdname checkpoint
.checkpointSave <- function(sim, file) {
  sim$.timestamp <- Sys.time()
  sim$.rng.state <- get(".Random.seed", envir = .GlobalEnv)
  sim$.rng.kind <- RNGkind()

  #f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  #fobj <- paste0(f, "_objs", ".RData")

  tmpEnv <- new.env()
  assign(objectNames("spades", "simList", "sim")[[1]]$objs, sim, envir = tmpEnv)

  saveSimList(objectNames("spades", "simList", "sim")[[1]]$objs,
              filename = file, keepFileBackedAsIs = TRUE, envir = tmpEnv)

  #save(list = ls(tmpEnv, all.names = TRUE), file = file, envir = tmpEnv)
  #save(list = ls(sim@.envir, all.names = TRUE), file = fobj, envir = sim@.envir)
  invisible(TRUE) # return "success" invisibly
}
