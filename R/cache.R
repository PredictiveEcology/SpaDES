################################################################################
#' Cache method that accomodates environments, S4 methods, Rasters
#'
#' This function is largely copied from \code{\link[archivist]{cache}}, with
#' three very critical modifications:
#' 1) the \code{archivist} package detects different environments as different;
#' 2) it also does not detect S4 methods correctly due to method inheritance;
#' 3) it does not detect objects that have file-base storage of information
#' (like \code{\link[raster]{RasterLayer-class}} objects and
#' \code{\link[ff]{ff}} objects).
#' This version of the \code{cache} function accomodates those three special,
#' though quite common, cases by:
#' 1) converting any environments into list equivalents;
#' 2) identifying the dispatched S4 method (including those made through
#' inheritance) before \code{\link[digest]{digest}} is called so the correct
#' method is being cached;
#' and 3) by running \code{\link[digest]{digest}} on the linked file.
#' In the \code{SpaDES} context, the \code{simList} has an environment as one of
#' its slots, thus using \code{archivist::cache} will not work correctly.
#'
#' Some of the details of the changes include:
#' We remove all elements that have an environment as part of their attributes.
#' This is generally functions that are loaded from the modules,
#' but also the \code{.envir} slot in the \code{simList}.
#' Functions are formatted to text before running digest.
#'
#' Cache (capital C) is a short cut to using SpaDES::cache as it
#' can be called from inside a SpaDES module without
#' specifying the \code{cacheRepo}. SpaDES will use the cacheRepo from a call
#' to \code{cachePath(sim)}, taking the sim from the call stack. Similarly, if no
#' \code{cacheRepo} is specified, then it will use \code{getPaths()$cachePath}, which
#' will, by default, be a temporary location with no persistence between R sessions!
#' To persist between sessions, use \code{SpaDES::setPaths()} every session.
#'
#' \code{Cache} (uppercase C) is also defined so that it is not confused with the
#' \code{archivist::cache} function which will not work in a SpaDES context.
#' If a user would like to use \code{cache} (lowercase c), then it must be
#' always prefixed with \code{SpaDES::cache(  )} so that it does not accidentally
#' call the achivist package version of cache.
#'
#' @section Caching as part of SpaDES:
#'
#' SpaDES has several levels of caching. Each level can be used to a modelers
#' advantage; and, each can be used simultaneously.
#'
#' @section \code{spades} or \code{experiment}:
#'
#' And entire call
#' to \code{spades} or \code{experiment} can be cached. This will have the effect
#' of eliminating any stochasticity in the model as the output will simply be
#' the cached version of the \code{simList}.
#'
#' @section Module-level caching:
#'
#' If the parameter \code{.useCache} is set to TRUE, then the \code{doEvent.moduleName}
#' will be cached. That means that every time that module
#' is called from within a spades or experiment call, cache will be used. Only
#' the objects inside the \code{simList} that correspond to the inputObjects of the
#' module and the outputObjects to the module will be assessed for caching
#' inputs or output, respectively.
#'
#' In general use, module level caching would be mostly useful for modules that have
#' no stochasticity, such as data-preparation modules, GIS modules etc.
#'
#' @section Event-level caching:
#'
#' If the parameter \code{.useCache} is set to a character or character vector,
#' then that or those event(s) will be cached. That means that every time the event
#' is called from within a spades or experiment call, cache will be used. Only
#' the objects inside the \code{simList} that correspond to the inputObjects of the
#' module and the outputObjects to the module will be assessed for caching
#' inputs or output, respectively. The fact that all and only the named inputObjects
#' and outputObjects are cached and returned may be inefficient (i.e., it may
#' cache more objects than are necessary) for individual events.
#'
#' In general use, event-level caching would be mostly useful for events that have
#' no stochasticity, such as data-preparation events, GIS events etc.
#'
#' @section Function-level caching:
#'
#' Any function can be cached using:
#' \code{Cache(FUN = functionName, ...)}
#' or
#' \code{cache(cacheRepo = cacheDirectory, FUN = functionName, ...)}. This will
#' be a slight change to a function call, such as:
#' \code{projectRaster(raster, crs = crs(newRaster))}
#' to
#' \code{Cache(projectRaster, raster, crs = crs(newRaster))}
#'
#' @inheritParams archivist::cache
#'
#' @param objects Character vector of objects within the simList that should
#'                be considered for caching. i.e., only use a subset of
#'                the simList objects. Only used if ... includes a \code{simList}.
#'
#' @param outputObjects Optional character vector indicating which objects to
#'                      return. This is only relevant for \code{simList} objects
#'
#' @param cacheRepo A repository used for storing cached objects.
#'                  This is optional if \code{Cache} is used inside a SpaDES module.
#'
#' @param compareRasterFileLength Numeric. Optional. When there are Rasters, that
#'        have file-backed storage, this is passed to the length arg in \code{digest}
#'        when determining if the Raster file is already in the database.
#'        Default 1e6. Passed to \code{prepareFileBackedRaster}.
#'
#' @param makeCopy A logical argument. Only effective when cache operates on dwdData. It creates a copy of the downloaded
#'                 files in the cacheRepo/gallery directory to optimize recovering. Only work when cache run for the first time for now.
#'
#' @param sideEffect A logical argument. Only effective when cache operates on dwdData.If TRUE,the argument check if files to be downloaded
#'                   are found locally prior to download. When the files are absent, they are recovered from a copy (the argument
#'                   makeCopy must have been set as TRUE the first time chache was runned).
#'
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'              If FALSE, cheksum is compiled using the object. Default is FALSE.
#'
#' @inheritParams digest::digest
#'
#' @return As with \code{\link[archivist]{cache}}, the return is either the return
#' value of the function call or the cached version (i.e., the result from a previous
#' call to this same cached function with identical arguments).
#'
#' @note In general, it is expected that caching will only be used when stochasticity
#' is not relevant, or if a user has achieved sufficient stochasticity (e.g., via
#' sufficient number of calls to \code{experiment}) such that no new explorations
#' of stochastic outcomes are required. It will also be very useful in a
#' reproducible work flow
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @export
#' @importFrom archivist cache loadFromLocalRepo saveToLocalRepo showLocalRepo
#' @importFrom digest digest
#' @importFrom R.utils isAbsolutePath
#' @importFrom utils unzip
#' @importFrom tools file_ext
#' @include simList-class.R
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
#' @examples
#' \dontrun{
#' mySim <- simInit(times = list(start = 0.0, end = 5.0),
#'                  params = list(.globals = list(stackName = "landscape", burnStats = "testStats")),
#'                  modules = list("randomLandscapes", "fireSpread"),
#'                  paths = list(modulePath = system.file("sampleModules", package = "SpaDES")))
#'
#'   # This functionality can be achieved within a spades call
#'   # compare caching ... run once to create cache
#'   system.time(outSim <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time(),
#'                                .plotInitialTime = NA))
#'   # compare... second time is fast
#'   system.time(outSimCached <- spades(Copy(mySim), cache = TRUE, .plotInitialTime = NA))
#'   all.equal(outSim, outSimCached)
#'
#'   # Function caching
#'   ras <- raster(extent(0,1e3,0,1e3),res = 1)
#'   system.time(map <- Cache(gaussMap, ras, cacheRepo = cachePath(mySim),
#'                            notOlderThan = Sys.time()))
#'   # second time much faster
#'   system.time(mapCached <- Cache(gaussMap, ras, cacheRepo = cachePath(mySim)))
#'
#'   # They are the same
#'   all.equal(map, mapCached)
#'
#'   # Module-level
#'   # In this example, we will use the cache on the randomLandscapes module
#'   # This means that each subsequent call to spades will result in identical
#'   # outputs from the randomLandscapes module (only!).
#'   # This would be useful when only one random landscape is needed
#'   # simply for trying something out, or putting into production code
#'   # (e.g., publication, decision support, etc.)
#'   params(mySim)$randomLandscapes$.useCache <- TRUE
#'   system.time(randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
#'                                  notOlderThan = Sys.time(), debug = TRUE))
#'
#'   # user  system elapsed
#'   # 1.26    0.25    7.00
#'   # Vastly faster
#'   system.time(randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA,
#'                                  debug = TRUE))
#'    # user  system elapsed
#'    # 0.22    0.00    0.24
#'    # Test that only layers produced in randomLandscapes are identical, not fireSpread
#'    layers <- list("DEM","forestAge", "habitatQuality", "percentPine","Fires")
#'    same <- lapply(layers, function(l) identical(randomSim$landscape[[l]],
#'                                         randomSimCached$landscape[[l]]))
#'    names(same) <- layers
#'    print(same)
#'
#' }
#'
setGeneric("Cache", signature = "...",
           function(FUN, ..., notOlderThan = NULL,
                    objects = NULL, outputObjects = NULL, algo = "xxhash32",
                    cacheRepo = NULL, compareRasterFileLength = 1e6, sideEffect= FALSE,
                    makeCopy = FALSE, quick = FALSE) {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo)
})

#' @export
#' @rdname cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,
                        algo, cacheRepo, compareRasterFileLength, sideEffect= FALSE, makeCopy = FALSE, quick = FALSE) {
    tmpl <- list(...)
    if (missing(notOlderThan)) notOlderThan <- NULL
    if (missing(quick)) quick <- TRUE
    # These three lines added to original version of cache in archive package
    wh <- which(sapply(tmpl, function(x) is(x, "simList")))
    if (is.null(cacheRepo)) {
      if (length(wh) > 0) {
        cacheRepo <- tmpl[wh][[1]]@paths$cachePath # just take the first simList, if there are >1
      } else {
        doEventFrameNum <- grep(sys.calls(), pattern = "(^doEvent)|(^.parseModule)")[2]
        if (!is.na(doEventFrameNum)) {
          sim <- get("sim", envir = sys.frame(doEventFrameNum))
          cacheRepo <- sim@paths$cachePath
        } else {
          cacheRepo <- SpaDES::getPaths()$cachePath
        }
      }
    }

    if (is(try(archivist::showLocalRepo(cacheRepo), silent = TRUE), "try-error")) {
      archivist::createLocalRepo(cacheRepo)
    }

    whRasOrSpatial <- which(sapply(tmpl, function(x) {is(x, "Raster")| is(x, "Spatial")}))
    whSpatial <- which(sapply(tmpl, function(x) is(x, "Spatial")))
    whFun <- which(sapply(tmpl, function(x) is.function(x) | is(x, "expression")))
    whFilename <- which(sapply(tmpl, function(x) is.character(x)))
    if(length(whFilename)>0) {
      tmpl[whFilename] <- lapply(whFilename, function(xx) {
        if(any(unlist(lapply(tmpl[[xx]], file.exists))))
          basename(tmpl[[xx]])
        else
          tmpl[[xx]]
      })
    }

    whCluster <- which(sapply(tmpl, function(x) is(x, "cluster")))
    if (length(wh) > 0 | exists("sim")) {
      if (length(wh) > 0) {
        cur <- current(tmpl[[wh]])
      } else {
        cur <- current(sim)
      }
    }

    if (is.null(objects)) {
      if (length(wh) > 0) tmpl[wh] <- lapply(tmpl[wh], makeDigestible,
                                             compareRasterFileLength = compareRasterFileLength)
    } else {
      if (length(wh) > 0) tmpl[wh] <- lapply(tmpl[wh], function(xx) {
        makeDigestible(xx, objects, compareRasterFileLength = compareRasterFileLength)
      })
    }


    if (isS4(FUN)) {
      # Have to extract the correct dispatched method
      firstElems <- strsplit(showMethods(FUN, inherited = TRUE, printTo = FALSE), split = ", ")
      firstElems <- lapply(firstElems, function(x) {
        y <- strsplit(x, split = "=")
        unlist(lapply(y, function(z) z[1]))
      })
      sigArgs <- lapply(unique(firstElems), function(x) {
        FUN@signature %in% x
      })
      signat <- unlist(sigArgs[unlist(lapply(sigArgs, function(y) any(y)))])

      methodUsed <- selectMethod(FUN, optional = TRUE,
                                 signature = sapply(as.list(
                                   match.call(FUN, do.call(call, append(list(name = FUN@generic),
                                                                        tmpl))))[FUN@signature[signat]],
                                   class)) ## TO DO: need to get the method the dispatch correct
      tmpl$.FUN <- format(methodUsed@.Data)
    } else {
      tmpl$.FUN <- format(FUN) # This is changed to allow copying between computers
    }

    if (length(whRasOrSpatial) > 0) tmpl[whRasOrSpatial] <- lapply(tmpl[whRasOrSpatial], makeDigestible,
                                                 compareRasterFileLength = compareRasterFileLength)
    if (length(whCluster) > 0) tmpl[whCluster] <- NULL
    if (length(whFun) > 0) tmpl[whFun] <- lapply(tmpl[whFun], format)
    if (!is.null(tmpl$progress)) if (!is.na(tmpl$progress)) tmpl$progress <- NULL

    #0
    # Extract list of files to download
    if(tmpl$dfile=="all"){
      datasetName <-tmpl$datasetName
      currentFileList <-paste0(datasetName,"/",unlist(urls[dataset==datasetName,files]))

    }else{
      currentFileList<-paste0(datasetName,"/",tmpl$dfile)
    }
    # List existing files
    listfile <- list.files(file.path(cacheRepo,datasetName))
    existingFileList<- currentFileList[basename(currentFileList) %in% listfile]

    # Extract checksum
    if(length(existingFileList)==0){
      cacheCurrentFiles <- lapply(currentFileList, function(x) {x=NA})
      names(cacheCurrentFiles)<-currentFileList
    }else{
      if (quick){
        sizeCurrentFiles <- lapply(existingFileList, function(x) {if(file.exists(file.path(cacheRepo,x[1])))
                                                               list(x, file.size(file.path(cacheRepo,x)))})
        cacheCurrentFiles <- lapply(sizeCurrentFiles, function(x) {if(file.exists(file.path(cacheRepo,x[1])))
                                                                 digest::digest(x, algo = algo)})
      }else{
        cacheCurrentFiles <- lapply(existingFileList, function(x) {if(file.exists(file.path(cacheRepo,x)))
                                                               digest::digest(file = file.path(cacheRepo,x), algo = algo)})
      }
      names(cacheCurrentFiles)<-existingFileList
    }
    #1

    outputHash <- digest::digest(tmpl, algo = algo)
    localTags <- showLocalRepo(cacheRepo, "tags")
    isInRepo <- localTags[localTags$tag == paste0("cacheId:", outputHash), , drop = FALSE]
    if (nrow(isInRepo) > 0) {
      lastEntry <- max(isInRepo$createdDate)
      lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
      if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
        if (grepl(format(FUN)[1], pattern = "function \\(sim, eventTime")) {
          # very coarse way of determining doEvent call
          message("Using cached copy of ", cur$eventType, " event in ", cur$moduleName, " module")
        }

        out <- loadFromLocalRepo(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo, value = TRUE)
        #0
        if (sideEffect){
          # List cached files
          needDwd<- NULL
          # Extracted checksum from previous download
          cachedFileNames <- attributes(out)$cacheFileList
          # Format checksum from current file as cached checksum
          currentFilesize <- paste0(names(cacheCurrentFiles), ":", cacheCurrentFiles)
          # List current files with divergent checksum (or checksum missing)
          setlist <- currentFilesize[!(currentFilesize %in% cachedFileNames)]
          fname <- sub(":.*", "", setlist)
          repoFrom = file.path(cacheRepo, "gallery")
          if(!length(fname)==0){
            # Process files having divergent checksum values
            for (i in 1:length(fname)){
              if(file.exists(file.path(repoFrom, fname[i]))){
                # extract checksum from backup files to compare with cached checksum
                if(quick){
                  sizeRepoFiles <- file.size(file.path(repoFrom,fname[i]))
                  cacheRepoFiles <- digest::digest(sizeRepoFiles, algo = algo)
                }else{
                  cacheRepoFiles <- digest::digest(file = file.path(repoFrom,fname[i]), algo = algo)
                }
                repoFiles <- paste0(fname[i], ":", cacheRepoFiles)

                if (repoFiles %in% cachedFileNames){
                  # checksum fit cached checksum. Copy will occur
                  dir.create(file.path(cacheRepo, datasetName), showWarnings=FALSE)
                  file.copy(file.path(repoFrom, fname[i]), file.path(cacheRepo, datasetName), overwrite = TRUE)
                  dwdrequiered <- FALSE
                  needDwd <- c(needDwd, dwdrequiered)
                }else{
                  # checksum don't match cached checksum. Download will occur
                  dwdrequiered <- TRUE
                  needDwd <- c(needDwd, dwdrequiered)
                }
              }else{
                # No copy are found locally. Download will occur
                dwdrequiered <- TRUE
                needDwd <- c(needDwd, dwdrequiered)
              }
            }
          }else{
            # Files exist locally and checksum match cached checksum
            dwdrequiered <- FALSE
            needDwd <- c(needDwd, dwdrequiered)
          }
          if(any(needDwd)){
            do.call(FUN, list(...))
          }
        }
        if (makeCopy){
          repoTo <- file.path(cacheRepo, "gallery")
          dir.create(file.path(repoTo, datasetName), showWarnings=FALSE)
          lapply(currentFileList, function(x) if(!file.exists(file.path(repoTo,x))){
                                                          file.copy(file.path(cacheRepo,x),
                                                          file.path(repoTo, datasetName),
                                                          recursive=TRUE)})
        }
        #1

        #if(!is(out, "simList")) {
          if (length(wh) > 0) {
            simListOut <- out # gets all items except objects in list(...)
            origEnv <- list(...)[[wh]]@.envir
            isListOfSimLists <- if (is.list(out)) if (is(out[[1]], "simList")) TRUE else FALSE else FALSE

            if (isListOfSimLists) {
              for (i in seq_along(out)) {
                keepFromOrig <- !(ls(origEnv) %fin% ls(out[[i]]))
                list2env(mget(ls(origEnv)[keepFromOrig], envir = origEnv),
                         envir = simListOut[[i]]@.envir)
              }
            } else {
              keepFromOrig <- !(ls(origEnv) %fin% ls(out))
              list2env(mget(ls(origEnv)[keepFromOrig], envir = origEnv),
                       envir = simListOut@.envir)
            }
            return(simListOut)
          }
        return(out)
      }
      if (notOlderThan >= lastEntry) {
        # flush it if notOlderThan is violated
        rmFromLocalRepo(isInRepo$artifact[lastOne], repoDir = cacheRepo)
      }
    }
    output <- do.call(FUN, list(...))
    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""

    #0
    if (makeCopy){
      repoTo <- file.path(cacheRepo, "gallery")
      dir.create(file.path(repoTo, datasetName), showWarnings=FALSE)
      lapply(currentFileList, function(x) if(!file.exists(file.path(repoTo,x))){
                                                 file.copy(file.path(cacheRepo,x),
                                                           file.path(repoTo, datasetName),
                                                           recursive=TRUE)})
    }

    if (quick){
      sizeCurrentFiles <- lapply(currentFileList, function(x) {list(x, file.size(file.path(cacheRepo,x)))})
      cacheCurrentFiles <- lapply(sizeCurrentFiles, function(x) {digest::digest(x, algo = algo)})
    }else{
      cacheCurrentFiles <- lapply(currentFileList, function(x) {digest::digest(file = file.path(cacheRepo,x), algo = algo)})
    }
    names(cacheCurrentFiles)<-currentFileList
    attr(output, "cacheFileList") <- paste0(names(cacheCurrentFiles), ":", cacheCurrentFiles)
    #1

    if (isS4(FUN)) attr(output, "function") <- FUN@generic

    if (is(output, "simList")) {
      if (!is.null(outputObjects)) {
        outputToSave <- output
        outputToSave@.envir <- new.env()
        list2env(mget(outputObjects, envir = output@.envir), envir = outputToSave@.envir)
        attr(outputToSave, "tags") <- attr(output, "tags")
        attr(outputToSave, "call") <- attr(output, "call")
        #0
        attr(outputToSave, "endFileList") <- attr(output, "endFileList")
        #1
        if (isS4(FUN))
          attr(outputToSave, "function") <- attr(output, "function")
      } else {
        outputToSave <- output
      }
    } else {
      outputToSave <- output
    }

    # This is for write conflicts to the SQLite database, i.e., keep trying until it is
    # written
    written <- FALSE
    if (is(outputToSave, "Raster")) {
      outputToSave <- prepareFileBackedRaster(outputToSave, repoDir = cacheRepo)#, archiveData = TRUE,
      output <- outputToSave
      #archiveSessionInfo = FALSE,
      #archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE)
    }
    while (!written) {
      saved <- try(saveToLocalRepo(outputToSave, repoDir = cacheRepo,
                                   archiveData = TRUE, archiveSessionInfo = FALSE,
                                   archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE),
                   silent = TRUE)

      # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
      written <- if (is(saved, "try-error")) {
        Sys.sleep(0.05)
        FALSE
      } else {
        TRUE
      }
    }

    return(output)
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
#' @export
#' @importFrom archivist rmFromLocalRepo searchInLocalRepo
#' @include simList-class.R
#' @docType methods
#' @rdname cache
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
    if (missing(sim) & missing(cacheRepo)) stop("Must provide either sim or cacheRepo")
    if (missing(cacheRepo)) cacheRepo <- sim@paths$cachePath
    if (missing(afterDate)) afterDate <- "1970-01-01"
    if (missing(beforeDate)) beforeDate <- Sys.Date() + 1

    objs <- searchInLocalRepo(pattern = list(dateFrom = afterDate, dateTo = beforeDate),
                              repoDir = cacheRepo)
    rmFromLocalRepo(objs, cacheRepo, many = TRUE)
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
    if (missing(sim) & missing(cacheRepo)) stop("Must provide either sim or cacheRepo")
    if (missing(cacheRepo)) cacheRepo <- sim@paths$cachePath

    tryCatch(splitTagsLocal(cacheRepo), error = function(x) {
      data.frame(artifact = character(0), tagKey = character(0),
                 tagValue = character(0), createdDate = character(0))
    })
})

################################################################################
#' Remove any reference to environments or filepaths in objects
#'
#' Using digest::digest will include every detail of an object, including
#' environments of functions, including those that are session-specific. Since
#' the goal of using digest::digest is not session specific, this function
#' attempts to strip all session specific information so that the digest
#' works between sessions and operating systems. It is tested under many
#' conditions and object types, there are bound to be others that don't
#' work correctly.
#'
#' This is primarily for internal use only. Especially when
#' caching a \code{simList}.
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
#' @param object an object to convert to a 'digestible' state
#'
#' @param objects Optional character vector indicating which objects are to
#'                be considered while making digestible.
#' @inheritParams Cache
#'
#' @return A simplified version of the \code{simList} object, but with no
#'         reference to any environments, or other session-specific information.
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @seealso \code{\link[digest]{digest}}.
#' @include simList-class.R
#' @include misc-methods.R
#' @importFrom broom tidy
#' @importFrom digest digest
#' @docType methods
#' @keywords internal
#' @rdname makeDigestible
#' @author Eliot McIntire
setGeneric("makeDigestible", function(object, objects,
                                      compareRasterFileLength = 1e6) {
          standardGeneric("makeDigestible")
})

#' @rdname makeDigestible
setMethod(
  "makeDigestible",
  signature = "simList",
    definition = function(object, objects, compareRasterFileLength) {

      objectsToDigest <- sort(ls(object@.envir, all.names = TRUE))
      if (!missing(objects)) {
        objectsToDigest <- objectsToDigest[objectsToDigest %in% objects]
      }
      envirHash <- (sapply(objectsToDigest, function(x) {
          if (!(x == ".sessionInfo")) {
            obj <- get(x, envir = envir(object))
            if (!is(obj, "function") & !is(obj, "expression")) {
              if (is(obj, "Raster")) {
                # convert Rasters in the simList to some of their metadata.
                obj <- makeDigestible(obj,
                                      compareRasterFileLength = compareRasterFileLength)
                dig <- digest::digest(obj)
              } else if (is(obj, "Spatial")) {
                dig <- makeDigestible(obj)
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
            dig <- digest::digest(get(x, envir = envir(object))[[1]] %>%
                                    .[c("major", "minor")])
          }
        return(dig)
      }))

      # Remove the NULL entries in the @.list

      envirHash <- envirHash[!sapply(envirHash, is.null)]
      envirHash <- sortDotsUnderscoreFirst(envirHash)

      # Convert to a simList_ to remove the .envir slot
      object <- as(object, "simList_")
      # Replace the .list slot with the hashes of the slots
      object@.list <- list(envirHash)

      # Remove paths as they are system dependent and not relevant for digest
      #  i.e., if the same file is located in a different place, that is ok
      object@paths <- list()
      object@outputs$file <- basename(object@outputs$file)
      object@inputs$file <- basename(object@inputs$file)
      deps <- object@depends@dependencies
      for (i in seq_along(deps)) {
        if(!is.null(deps[[i]])) {
          object@depends@dependencies[[i]] <- lapply(slotNames(object@depends@dependencies[[i]]), function(x) slot(object@depends@dependencies[[i]],x))
          names(object@depends@dependencies[[i]]) <- slotNames(deps[[i]])
          object@depends@dependencies[[i]][["timeframe"]] <- as.Date(deps[[i]]@timeframe)
        }
      }

      # Sort the params and .list with dots first, to allow Linux and Windows to be compatible
      object@params <- lapply(object@params, function(x) sortDotsUnderscoreFirst(x))

      return(object)
})

setMethod(
  "makeDigestible",
  signature = "Raster",
  definition = function(object, compareRasterFileLength) {

    if (is(object, "RasterStack") | is(object, "RasterBrick")) {
      dig <- list(dim(object), res(object), crs(object), extent(object),
                  lapply(object@layers, function(yy) yy@data))
      if (nchar(object@filename) > 0) {
        # if the Raster is on disk, has the first compareRasterFileLength characters;
        # uses SpaDES:::digest on the file
        dig <- append(dig, digest(file = object@filename, length = compareRasterFileLength))
      }
    } else {
      dig <- list(dim(object), res(object), crs(object), extent(object), object@data)
      if (nchar(object@file@name) > 0) {
        # if the Raster is on disk, has the first compareRasterFileLength characters;
        # uses SpaDES:::digest on the file
        dig <- append(dig, digest(file = object@file@name, length = compareRasterFileLength))
      }
    }
    return(dig)
})

setMethod(
  "makeDigestible",
  signature = "Spatial",
  definition = function(object, compareRasterFileLength) {

    #if (is(object, "SpatialPolygonsDataFrame") ) {

    aaa <- suppressMessages(broom::tidy(object))

    # The following Rounding is necessary to make digest equal on linux and windows
    bbb <- as.data.frame(lapply(aaa, function(x) if(is(x,"numeric")) round(x, 4) else x))
    dig <- digest::digest(bbb)
    return(dig)
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
    md5hashInBackpack <- showLocalRepo(repoDir = repoDir)$md5hash
    listFiles <- dir(file.path(repoDir, "gallery")) %>% strsplit(".rda") %>% unlist()
    toRemove <- !(md5hashInBackpack %in% listFiles)
    md5hashInBackpack[toRemove] %>%
      sapply(., rmFromLocalRepo, repoDir = repoDir)
    return(invisible(md5hashInBackpack[toRemove]))
})

#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @docType methods
#' @rdname cache
setGeneric("cache", signature = "...",
           function(cacheRepo = NULL, FUN, ..., notOlderThan = NULL,
                    objects = NULL, outputObjects = NULL, algo = "xxhash32") {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo)
})

#' @export
#' @rdname cache
setMethod(
  "cache",
  definition = function(cacheRepo, FUN, ..., notOlderThan, objects,
                        outputObjects, algo) {
    Cache(FUN = FUN, ..., notOlderThan = notOlderThan, objects = objects,
          outputObjects = outputObjects, algo = algo, cacheRepo = cacheRepo)
})

#' Alternative to \code{archivist::saveToRepo} for rasters
#'
#' Rasters are sometimes file-based, so the normal save mechanism doesn't work.
#' This function creates an explicit save of the file that is backing the raster,
#' in addition to saving the object metadata in the \code{archivist} repository database.
#'
#' @param obj The raster object to save to the repository.
#'
#' @inheritParams Cache
#'
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param ... pass to \code{archivist::saveToRepo}
#'
#' @return A raster object and its file backing will be passed to the archivist repository.
#'
#' @importFrom digest digest
#' @importFrom raster filename
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname prepareFileBackedRaster
#'
prepareFileBackedRaster <- function(obj, repoDir = NULL, compareRasterFileLength = 1e6, ...) {
  if (!inMemory(obj)) {
    curFilename <- normalizePath(filename(obj), winslash = "/")

    saveFilename <- file.path(repoDir, "rasters", basename(curFilename)) %>%
      normalizePath(., winslash = "/", mustWork = FALSE)

    if (saveFilename != curFilename) {
      shouldCopy <- TRUE
      if (file.exists(saveFilename)) {
        if (!(compareRasterFileLength == Inf)) {
          if (digest(file = saveFilename, length = compareRasterFileLength) ==
              digest(file = curFilename, length = compareRasterFileLength)) {
            shouldCopy <- FALSE
          }
        } else {
          shouldCopy = TRUE
        }
      }
      if (shouldCopy) {
        pathExists <- file.exists(dirname(saveFilename))
        if (!pathExists) dir.create(dirname(saveFilename))
       if (saveFilename %>% grepl(., pattern = ".grd$")) {
          copyFile(to = saveFilename, overwrite = TRUE, from = curFilename, silent = TRUE)
          griFilename <- sub(saveFilename, pattern = ".grd$", replacement = ".gri")
          curGriFilename <- sub(curFilename, pattern = ".grd$", replacement = ".gri")
          copyFile(to = griFilename, overwrite = TRUE, from = curGriFilename, silent = TRUE)
          #           file.copy(to = griFilename, overwrite = TRUE,
          #                     recursive = FALSE, copy.mode = TRUE,
          #                     from = curGriFilename)
        } else {
          suppressWarnings(copyFile(to = saveFilename, overwrite = TRUE,
                                    from = curFilename, silent = TRUE))
        }
      }
      slot(slot(obj, "file"), "name") <- saveFilename
    }
  } else {
    saveFilename <- slot(slot(obj, "file"), "name")
  }

  return(obj)
}

#' Copy a file using Robocopy on Windows and rsync on Linux/macOS
#'
#' This will copy an individual file faster using \code{Robocopy} on Windows,
#' and using \code{rsync} on macOS and Linux.
#'
#' @param from The source file.
#'
#' @param to The new file.
#'
#' @param useRobocopy For Windows, this will use a system call to \code{Robocopy}
#'        which appears to be much faster than the internal \code{file.copy} function.
#'        Uses \code{/MIR} flag. Default \code{TRUE}.
#'
#' @param overwrite Passed to \code{file.copy}
#'
#' @param delDestination Logical, whether the destination should have any files deleted,
#' if they don't exist in the source. This is \code{/purge}.
#'
#' @param create Passed to \code{checkLazyDir}.
#'
#' @param silent Should a progress be printed.
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname copyFile
#'
copyFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                     overwrite = TRUE, delDestination = FALSE,
                     #copyRasterFile=TRUE, clearRepo=TRUE,
                     create = TRUE, silent = FALSE) {

  origDir <- getwd()
  if (!dir.exists(to)) to <- dirname(to) # extract just the directory part
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    if (useRobocopy) {
      if (silent) {
        system(paste0("robocopy ","/purge"[delDestination]," /ETA /NDL /NFL /NJH /NJS ",
                      normalizePath(dirname(from), winslash = "\\"),
                      "\\ ", normalizePath(to, winslash = "\\"),
                      " ", basename(from)))
      } else {
        system(paste0("robocopy ", "/purge"[delDestination], " /ETA ",
                      normalizePath(dirname(from), winslash = "\\"), "\\ ",
                      normalizePath(to, winslash = "\\"), " ", basename(from)))
        #         system(paste0("robocopy /E ","/purge"[delDestination]," /ETA ", normalizePath(fromDir, winslash = "\\"),
        #                       "\\ ", normalizePath(toDir, winslash = "\\"), "\\"))
      }
    } else {
      file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)
    }
  } else if (os == "linux" | os == "darwin") {
    if (silent) {
      system(paste0("rsync -a ","--delete "[delDestination], from, " ", to,"/"))
    } else {
      system(paste0("rsync -avP ","--delete "[delDestination], from, " ", to, "/"))
    }
  }
  setwd(origDir)
  return(invisible(to))
}
