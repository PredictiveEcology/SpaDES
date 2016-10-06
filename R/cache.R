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
#' Normally, a user will access this functionality as an argument in \code{\link{spades}}.
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
#' @examples
#' \dontrun{
#' mySim <- simInit(times=list(start=0.0, end=5.0),
#'                  params=list(.globals=list(stackName="landscape", burnStats = "testStats")),
#'                  modules=list("randomLandscapes", "fireSpread"),
#'                  paths=list(modulePath=system.file("sampleModules", package="SpaDES")))
#' if (require(archivist)) {
#'   # Call cache function directly
#'   archivist::createLocalRepo(paths(mySim)$cachePath)
#'   system.time(outSim <- cache(paths(mySim)$cachePath,
#'               spades, sim = copy(mySim), .plotInitialTime = NA, notOlderThan = Sys.time()))
#'   system.time(outSim <- cache(paths(mySim)$cachePath,
#'               spades, sim = copy(mySim), .plotInitialTime = NA))
#'
#'   # This functionality can be achieved within a spades call
#'   # compare caching ... run once to create cache
#'   system.time(outSim <- spades(copy(mySim), cache = TRUE, notOlderThan = Sys.time(),
#'                                .plotInitialTime = NA))
#'   # compare... second time is fast
#'   system.time(outSimCached <- spades(copy(mySim), cache = TRUE, .plotInitialTime = NA))
#'   all.equal(outSim, outSimCached)
#' }
#' }
#'
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
    if (missing(notOlderThan)) notOlderThan <- NULL
    # These three lines added to original version of cache in archive package
    wh <- which(sapply(tmpl, function(x) is(x, "simList")))
    whFun <- which(sapply(tmpl, function(x) is.function(x)))
    tmpl$.FUN <- format(FUN) # This is changed to allow copying between computers
    if (length(wh) > 0) tmpl[wh] <- lapply(tmpl[wh], makeDigestible)
    if (length(whFun) > 0) tmpl[whFun] <- lapply(tmpl[whFun], format)
    if (!is.null(tmpl$progress)) if (!is.na(tmpl$progress)) tmpl$progress <- NULL

    outputHash <- digest::digest(tmpl)
    localTags <- showLocalRepo(cacheRepo, "tags")
    isInRepo <- localTags[localTags$tag == paste0("cacheId:", outputHash), , drop = FALSE]
    if (nrow(isInRepo) > 0) {
      lastEntry <- max(isInRepo$createdDate)
      lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
      if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
        out <- loadFromLocalRepo(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo, value = TRUE)
        return(out)
      }
      if ((notOlderThan >= lastEntry)) { # flush it if notOlderThan is violated
        rmFromLocalRepo(isInRepo$artifact[lastOne], repoDir = cacheRepo)
      }
    }
    output <- do.call(FUN, list(...))
    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""

    written <- FALSE
    while (!written) {
      saved <- try(saveToRepo(output, repoDir = cacheRepo, archiveData = TRUE,
                              archiveSessionInfo = FALSE,
                              archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE),
                   silent = TRUE)
      written <- if (is(saved, "try-error")) {
        Sys.sleep(0.05)
        FALSE
      } else {
        TRUE
      }
    }
    output
  })


#' @inheritParams spades
#' @inheritParams cache
#' @param afterDate Objects cached after this date will be deleted, formatted YYYY-MM-DD.
#' @param beforeDate Objects cached before this date will be deleted, formatted as YYYY-MM-DD.
#' @param ... Other arguments passed to
#'
#' If neither \code{afterDate} or \code{beforeDate} are provided, then all objects will be removed.
#' If both \code{afterDate} and \code{beforeDate} are specified, then all objects between \code{afterDate} and
#' \code{beforeDate} will be deleted.
#'
#' @return Will clear all objects from the \code{cachePath} of the sim object
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @export
#' @importFrom archivist rmFromLocalRepo searchInLocalRepo
#' @include simList-class.R
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
#' @examples
#' \dontrun{
#' clearCache(mySim)
#' }
setGeneric("clearCache", function(sim, afterDate, beforeDate, cacheRepo, ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname cache
setMethod(
  "clearCache",
  definition = function(sim, afterDate, beforeDate, cacheRepo, ...) {

    if(missing(sim) & missing(cacheRepo)) stop("Must provide either sim or cacheRepo")
    if(missing(cacheRepo)) cacheRepo <- cachePath(sim)
    if(missing(afterDate)) afterDate = "1970-01-01"
    if(missing(beforeDate)) beforeDate = Sys.Date() + 1

    objs <- searchInLocalRepo(pattern = list(dateFrom = afterDate, dateTo = beforeDate),
                              repoDir = cacheRepo)
    lapply(objs,
           function(hash) rmFromLocalRepo(hash, cacheRepo))
  })

#' \code{showCache} and \code{clearCache} are wrappers around \code{archivist} package
#' functions, specific to simList objects.
#' They allow the user a bit of control over what is being cached.
#'
#' @inheritParams clearCache
#'
#' @seealso \code{\link[archivist]{splitTagsLocal}}.
#' @export
#' @importFrom archivist splitTagsLocal
#' @include simList-class.R
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
#' @examples
#' \dontrun{
#' showCache(mySim)
#' }
setGeneric("showCache", function(sim, cacheRepo, ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname cache
setMethod(
  "showCache",
  definition = function(sim, cacheRepo, ...) {

    if(missing(sim) & missing(cacheRepo)) stop("Must provide either sim or cacheRepo")
    if(missing(cacheRepo)) cacheRepo <- cachePath(sim)

    tryCatch(splitTagsLocal(cacheRepo), error = function(x)
      data.frame(artifact = character(), tagKey = character(), tagValue = character(),
                 createdDate = character()))
  })

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
#' @keywords internal
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
            if (is(obj, "RasterStack") | is(obj, "RasterBrick")) {
              dig <- list(dim(obj), res(obj), crs(obj), extent(obj),
                          lapply(obj@layers, function(yy) yy@data))
              if (nchar(obj@filename) > 0) {
                # if the Raster is on disk, has the first 1e6 characters;
                # uses SpaDES:::digest on the file
                dig <- append(dig, digest(file = obj@filename, length = 1e6))
              }
            } else {
              dig <- list(dim(obj), res(obj), crs(obj), extent(obj), obj@data)
              if (nchar(obj@file@name) > 0) {
                # if the Raster is on disk, has the first 1e6 characters;
                # uses SpaDES:::digest on the file
                dig <- append(dig, digest(file = obj@file@name, length = 1e6))
              }
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

    return(simList)
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
  })
