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
#' @param eventType    A character string specifying the type of event:
#'                      one of either \code{"init"}, \code{"load"}, or \code{"save"}.
#'
#' @param debug         Optional logical flag determines whether sim debug info
#'                      will be printed (default \code{debug=FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @seealso \code{\link{.Random.seed}}.
#'
#' @author Alex Chubaty
#'
#' @include environment.R
#' @importFrom R.utils isAbsolutePath
#' @export
#' @docType methods
#' @rdname checkpoint
#'
doEvent.checkpoint = function(sim, eventTime, eventType, debug=FALSE) {
  ### determine whether to use checkpointing
  ### default is not to use checkpointing if unspecified
  ### - this default is set when a new simList object is initialized

  useChkpnt = !any(is.na(params(sim)$.checkpoint))

  ### determine checkpoint file location, for use in events below
  if (useChkpnt) {
    if (is.null(checkpointFile(sim))) {
      checkpointFile <- "checkpoint.RData"
    } else {
      checkpointFile <- checkpointFile(sim)
    }

    if(isAbsolutePath(checkpointFile(sim))) {
      checkpointDir <- checkPath(dirname(checkpointFile(sim)), create=TRUE)
    } else {
      checkpointDir <- checkPath(outputPath(sim), create=TRUE)
    }

    checkpointFile <- file.path(checkpointDir, basename(checkpointFile(sim)))
  }

  ### event definitions
  if (eventType=="init") {
    if (useChkpnt) {
      sim <- scheduleEvent(sim, 0.00, "checkpoint", "save")
    }
  } else if (eventType=="save") {
    if (useChkpnt) {
      .checkpointSave(sim, checkpointFile)

      # schedule the next save
      timeNextSave <- time(sim) + checkpointInterval(sim)
      sim <- scheduleEvent(sim, timeNextSave, "checkpoint", "save")
    }
  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1,"moduleName",with=FALSE],"\'",sep=""))

  }
  return(invisible(sim))
}

#' @param file The checkpoint file.
#' @rdname checkpoint
#' @export
checkpointLoad = function(file) {
  f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  fobj <- paste0(f, "_objs", ".RData")

  # check for previous checkpoint files
  if (file.exists(file) && file.exists(fobj)) {
    simListName = load(file, envir=.GlobalEnv)
    sim <- get(simListName, envir=.GlobalEnv)
    load(fobj, envir=envir(sim))

    do.call("RNGkind", as.list(sim$.rng.kind))
    assign(".Random.seed", sim$.rng.state, envir=.GlobalEnv)
    rm(list=c(".rng.kind", ".rng.state", ".timestamp"), envir=envir(sim))
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}

#' @rdname checkpoint
.checkpointSave = function(sim, file) {
  sim$.timestamp <- Sys.time()
  sim$.rng.state <- get(".Random.seed", envir=.GlobalEnv)
  sim$.rng.kind <- RNGkind()

  f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  fobj <- paste0(f, "_objs", ".RData")

  tmpEnv <- new.env()
  assign(.objectNames("spades","simList","sim")[[1]]$objs, sim, envir=tmpEnv)

  save(list=ls(tmpEnv, all.names=TRUE), file=file, envir=tmpEnv)
  save(list=ls(envir(sim), all.names=TRUE), file=fobj, envir=envir(sim))
  invisible(TRUE) # return "success" invisibly
}



################################################################################
#' Cache method for simList class objects
#'
#' Because the \code{simList} has an environment as one of its slots, the caching mechanism
#' of the archivist package does not work. Here, we make a slight tweak to the
#' \code{cache} function. Specifically, we remove all elements that have an environment
#' as part of their attributes. This is generally functions that are loaded from the modules,
#' but also the \code{.envir} slot in the \code{simList}. Thus, only non-function objects are
#' used as part of the \code{digest} call in the \code{digest} package (used internally in
#' the \code{cache} function).
#'
#' @inheritParams archivist::cache
#'
#' @return Identical to \code{\link[archivist]{cache}}
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @export
#' @importFrom archivist cache showLocalRepo loadFromLocalRepo saveToRepo
#' @importFrom digest digest
#' @include simList-class.R
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
setGeneric("cache", signature="...", function(cacheRepo=NULL, FUN, ..., notOlderThan=NULL) {
  archivist::cache(cacheRepo, FUN, ..., notOlderThan)
})

#' @export
#' @rdname cache
setMethod(
  "cache",
  signature="simList",
  definition=function(cacheRepo, FUN, ..., notOlderThan) {
    tmpl <- list(...)
    wh <- which(sapply(tmpl, function(x) is(x, "simList")))
    tmpl$.FUN <- FUN
    tmpl$envirHash <- (sapply(sort(ls(tmpl[[wh]]@.envir, all.names=TRUE)), function(x) {
      if(!is(get(x, envir=envir(tmpl[[wh]])), "function")) {
        digest::digest(get(x, envir=envir(tmpl[[wh]])))
      } else {
        NULL
      }
      }))
    tmpl$envirHash$.sessionInfo <- NULL

    tmpl$envirHash <- tmpl$envirHash[!sapply(tmpl$envirHash, is.null)]
    tmpl$envirHash <- tmpl$envirHash[order(names(tmpl$envirHash))]
    envirHash <- tmpl$envirHash
    tmpl[[wh]]@.envir <- getNamespace("SpaDES")
    outputHash <- digest::digest(tmpl)
    
    localTags <- showLocalRepo(cacheRepo, "tags")
    isInRepo <- localTags[localTags$tag == paste0("cacheId:",
                                                  outputHash), , drop = FALSE]
    if (nrow(isInRepo) > 0) {
      lastEntry <- max(isInRepo$createdDate)
      if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
        lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
        return(loadFromLocalRepo(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo, value = TRUE))
      }
    }
    output <- do.call(FUN, list(...))
    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""
    saveToRepo(output, repoDir = cacheRepo, archiveData = TRUE,
               archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE)
    output
  }
)
