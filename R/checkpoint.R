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
#' @export
#' @docType methods
#' @rdname checkpoint
#'
doEvent.checkpoint = function(sim, eventTime, eventType, debug = FALSE) {
  ### determine whether to use checkpointing
  ### default is not to use checkpointing if unspecified
  ### - this default is set when a new simList object is initialized

  useChkpnt <- !any(is.na(params(sim)$.checkpoint))

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
      timeNextSave <- time(sim) + checkpointInterval(sim)
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
  f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  fobj <- paste0(f, "_objs", ".RData")

  # check for previous checkpoint files
  if (file.exists(file) && file.exists(fobj)) {
    simListName <- load(file, envir = .GlobalEnv)
    sim <- get(simListName, envir = .GlobalEnv)
    load(fobj, envir = envir(sim))

    do.call("RNGkind", as.list(sim$.rng.kind))
    assign(".Random.seed", sim$.rng.state, envir = .GlobalEnv)
    rm(list = c(".rng.kind", ".rng.state", ".timestamp"), envir = envir(sim))
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

  f <- strsplit(file, split = "[.][R|r][D|d]ata$")
  fobj <- paste0(f, "_objs", ".RData")

  tmpEnv <- new.env()
  assign(objectNames("spades", "simList", "sim")[[1]]$objs, sim, envir = tmpEnv)

  save(list = ls(tmpEnv, all.names = TRUE), file = file, envir = tmpEnv)
  save(list = ls(envir(sim), all.names = TRUE), file = fobj, envir = envir(sim))
  invisible(TRUE) # return "success" invisibly
}

################################################################################
#' Cache method for simList class objects
#'
#' Because the \code{simList} has an environment as one of its slots,
#' the caching mechanism of the archivist package does not work.
#' Here, we make a slight tweak to the \code{cache} function.
#' Specifically, we remove all elements that have an environment as part of
#' their attributes.
#' This is generally functions that are loaded from the modules,
#' but also the \code{.envir} slot in the \code{simList}.
#' Thus, only non-function objects are used as part of the \code{digest} call
#' in the \code{digest} package (used internally in the \code{cache} function).
#'
#' @inheritParams archivist::cache
#'
#' @return Identical to \code{\link[archivist]{cache}}
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @export
#' @importFrom archivist cache loadFromLocalRepo saveToRepo showLocalRepo
#' @importFrom digest digest
#' @include simList-class.R
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
setGeneric("cache", signature = "...",
           function(cacheRepo = NULL, FUN, ..., notOlderThan = NULL) {
  archivist::cache(cacheRepo, FUN, ..., notOlderThan)
})

#' @export
#' @rdname cache
setMethod(
  "cache",
  definition = function(cacheRepo, FUN, ..., notOlderThan) {
    tmpl <- list(...)
    # These three lines added to original version of cache in archive package
    wh <- which(sapply(tmpl, function(x) is(x, "simList")))
    whFun <- which(sapply(tmpl, function(x) is.function(x)))
    tmpl$.FUN <- format(FUN) # This is changed to allow copying between computers
    if (length(wh) > 0)
      tmpl[wh] <- lapply(tmpl[wh], makeDigestible)
    if (length(whFun) > 0)
      tmpl[whFun] <- lapply(tmpl[whFun], format)

    outputHash <- digest::digest(tmpl)
    localTags <- showLocalRepo(cacheRepo, "tags")
    isInRepo <- localTags[localTags$tag ==
                            paste0("cacheId:", outputHash), , drop = FALSE]
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

################################################################################
#' Remove any reference to environments in a \code{simList}
#'
#' Internal use only. Used when caching a SpaDES run a \code{simList}.
#'
#' This is a derivative of the class \code{simList}, except that all references
#' to local environments are removed.
#' Specifically, all functions (which are contained within environments) are
#' converted to a text representation via a call to \code{format(fn)}.
#' Also the objects that were contained within the \code{.envir} slot are hashed
#' using \code{digest::digest}.
#' The \code{paths} slot is not used (to allow comparison across platforms); it's
#' not relevant where the objects are gotten from, so long as they are the same.
#' The \code{.envir} slot is emptied (\code{NULL}).
#' The object is then converted to a \code{simList_} which has a \code{.list} slot.
#' The hashes of the objects are then placed in that \code{.list} slot.
#'
#' @param simList an object of class \code{simList}
#'
#' @return A simplified version of the \code{simList} object, but with no
#'         reference to any environments
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @seealso \code{\link[digest]{digest}}.
#' @include simList-class.R
#' @include misc-methods.R
#' @importFrom digest digest
#' @docType methods
#' @rdname makeDigestible
#' @author Eliot McIntire
setGeneric("makeDigestible", function(simList) {
  standardGeneric("makeDigestible")
})

#' @rdname makeDigestible
setMethod(
  "makeDigestible",
  signature = "simList",
  definition = function(simList) {
    envirHash <- (sapply(sort(ls(simList@.envir, all.names = TRUE)), function(x) {
      if (!(x == ".sessionInfo")) {
        obj <- get(x, envir = envir(simList))
        if (!is(obj, "function")) {
          if (is(obj, "Raster")) {
            # convert Rasters in the simList to some of their metadata.
            dig <- list(dim(obj), res(obj), crs(obj), extent(obj), obj@data)
            if (nchar(obj@file@name) > 0) {
              # if the Raster is on disk, has the first 1e6 characters;
              # uses SpaDES:::digest on the file
              dig <- append(dig, digest(file = obj@file@name, length = 1e6))
            }
            dig <- digest::digest(dig)
          } else {
            # convert functions in the simList to their digest.
            #  functions have environments so are always unique
            dig <- digest::digest(obj)
          }
        } else {
          # for functions, use a character representation via format
          dig <- digest::digest(format(obj))
        }
      } else {
        # for .sessionInfo, just keep the major and minor R version
        dig <- digest::digest(get(x, envir = envir(simList))[[1]] %>%
                                .[c("major", "minor")])
      }
      return(dig)
    }))

    # Remove the NULL entries in the @.list
    envirHash <- envirHash[!sapply(envirHash, is.null)]
    envirHash <- sortDotsFirst(envirHash)

    # Convert to a simList_ to remove the .envir slot
    simList <- as(simList, "simList_")
    # Replace the .list slot with the hashes of the slots
    simList@.list <- list(envirHash)

    # Remove paths as they are system dependent and not relevant for digest
    #  i.e., if the same file is located in a different place, that is ok
    simList@paths <- list()

    # Sort the params and .list with dots first, to allow Linux and Windows to be compatible
    simList@params <- lapply(simList@params, function(x) sortDotsFirst(x))

    simList
})



################################################################################
#' Clear erroneous archivist artifacts
#'
#' When an archive object is being saved, if this is occurring at the same time
#' as another process doing the same thing, a stub of a artifact occurs. This
#' function will clear those stubs.
#'
#' @return Done for its side effect on the repoDir
#'
#' @param repoDir A character denoting an existing directory of the Repository for
#' which metadata will be returned. If it is set to NULL (by default), it
#' will use the repoDir specified in \code{archivist::setLocalRepo}.
#'
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @docType methods
#' @rdname clearStubArtifacts
#' @author Eliot McIntire
setGeneric("clearStubArtifacts", function(repoDir = NULL) {
  standardGeneric("clearStubArtifacts")
})

#' @export
#' @rdname clearStubArtifacts
setMethod(
  "clearStubArtifacts",
  definition = function(repoDir) {
    md5hashInBackpack = showLocalRepo(repoDir = repoDir)$md5hash
    listFiles <- dir(file.path(repoDir, "gallery")) %>% strsplit(".rda") %>% unlist()
    toRemove <- !(md5hashInBackpack %in% listFiles)
    md5hashInBackpack[toRemove] %>%
      sapply(., rmFromLocalRepo, repoDir = repoDir)
    return(invisible(md5hashInBackpack[toRemove]))
  }
)

