if (!isGeneric("robustDigest")) {
    setGeneric("robustDigest", function(object, objects,
                                          compareRasterFileLength = 1e6,
                                          algo = "xxhash64") {
    standardGeneric("robustDigest")
  })
}

#' robustDigest for simList class objects
#'
#' See \code{\link[reproducible]{robustDigest}}. This method strips out stuff
#' from a simList class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems,
#' or machines. This will likely still not allow identical digest
#' results across R versions.
#'
#' @importFrom reproducible robustDigest
#' @importMethodsFrom reproducible robustDigest
#' @inheritParams reproducible::robustDigest
#' @rdname robustDigest
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{robustDigest}}
#' @exportMethod robustDigest
#' @export
setMethod(
  "robustDigest",
  signature = "simList",
  definition = function(object, objects, compareRasterFileLength, algo) {
    allObjs <- ls(object@.envir, all.names = TRUE)
    objectsToDigest <- sort(allObjs, method = "radix")
    if (!missing(objects)) {
      objectsToDigest <- objectsToDigest[objectsToDigest %in% objects]
    }

    envirHash <- robustDigest(mget(objectsToDigest, envir = object@.envir))

    envirHash <- sortDotsUnderscoreFirst(envirHash)
    object <- Copy(object, objects = FALSE, queues = FALSE)
    object@.envir <- new.env()

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
      if (!is.null(deps[[i]])) {
        object@depends@dependencies[[i]] <- lapply(slotNames(object@depends@dependencies[[i]]), function(x)
          slot(object@depends@dependencies[[i]],x))
        names(object@depends@dependencies[[i]]) <- slotNames(deps[[i]])
        object@depends@dependencies[[i]][["timeframe"]] <- as.Date(deps[[i]]@timeframe)
      }
    }

    # Sort the params and .list with dots first, to allow Linux and Windows to be compatible
    object@params <- lapply(object@params, function(x) sortDotsUnderscoreFirst(x))

    return(object)
})

if (!isGeneric(".tagsByClass")) {
  setGeneric(".tagsByClass", function(object) {
    standardGeneric(".tagsByClass")
  })
}

#' tagsByClass for simList class objects
#'
#' See \code{\link[reproducible]{.tagsByClass}}. Adds current \code{moduleName},
#' \code{eventType}, \code{eventTime}, and \code{function:spades} as userTags
#'
#' @importFrom reproducible .tagsByClass
#' @importMethodsFrom reproducible .tagsByClass
#' @inheritParams reproducible::.tagsByClass
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.tagsByClass}}
#' @exportMethod .tagsByClass
#' @export
#' @rdname tagsByClass
setMethod(
  ".tagsByClass",
  signature = "simList",
  definition = function(object) {
    cur <- object@current
    if (NROW(cur)) {
      userTags <- c(
                    paste0("module:",cur$moduleName),
                    paste0("eventType:",cur$eventType),
                    paste0("eventTime:",cur$eventTime),
                    paste0("function:spades")) # add this because it will be an
                                               # outer function, if there are
                                               # events occurring
    } else {
      userTags <- NULL
    }
    userTags
})

if (!isGeneric(".cacheMessage")) {
  setGeneric(".cacheMessage", function(object, functionName) {
    standardGeneric(".cacheMessage")
  })
}

#' cacheMessage for simList class objects
#'
#' See \code{\link[reproducible]{.cacheMessage}}.
#'
#' @importFrom reproducible .cacheMessage
#' @importMethodsFrom reproducible .cacheMessage
#' @inheritParams reproducible::.cacheMessage
#' @rdname cacheMessage
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.cacheMessage}}
#' @exportMethod .cacheMessage
setMethod(
  ".cacheMessage",
  signature = "simList",
  definition = function(object, functionName) {
    cur <- current(object)
    if (NROW(cur)) {
      message("Using cached copy of ", cur$eventType, " event in ", cur$moduleName, " module")
    } else {
      .cacheMessage(NULL, functionName)
    }
})


#########################################################
if (!isGeneric(".checkCacheRepo")) {
  setGeneric(".checkCacheRepo", function(object) {
    standardGeneric(".checkCacheRepo")
  })
}

#' checkCacheRepo for simList class objects
#'
#' See \code{\link[reproducible]{.checkCacheRepo}}.
#'
#' @importFrom reproducible .checkCacheRepo
#' @importMethodsFrom reproducible .checkCacheRepo
#' @inheritParams reproducible::.checkCacheRepo
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.checkCacheRepo}}
#' @exportMethod .checkCacheRepo
#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "list",
  definition = function(object) {
    whSimList <- unlist(lapply(object, is, "simList"))
    if (length(whSimList) > 0) {
      cacheRepo <- object[whSimList][[1]]@paths$cachePath # just take the first simList, if there are >1
    } else {
      doEventFrameNum <- grep(sys.calls(), pattern = "(^doEvent)|(^.parseModule)")[2]
      if (!is.na(doEventFrameNum)) {
        sim <- get("sim", envir = sys.frame(doEventFrameNum))
        cacheRepo <- sim@paths$cachePath
      } else {
        cacheRepo <- getOption("spades.cachePath")
        #checkPath(cacheRepo, create = TRUE) #SpaDES dependency
      }
    }
    cacheRepo
})

if (!isGeneric(".prepareOutput")) {
  setGeneric(".prepareOutput", function(object) {
    standardGeneric(".prepareOutput")
  })
}

##########################################
#' prepareOutput for simList class objects
#'
#' See \code{\link[reproducible]{.prepareOutput}}.
#'
#' @importFrom reproducible .prepareOutput
#' @importMethodsFrom reproducible .prepareOutput
#' @inheritParams reproducible::.prepareOutput
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.prepareOutput}}
#' @exportMethod .prepareOutput
#' @export
#' @rdname prepareOutput
setMethod(
  ".prepareOutput",
  signature = "simList",
  definition = function(object, cacheRepo, ...) {
      simListOut <- object # gets all items except objects in list(...)
      tmpl <- list(...)
      whSimList <- which(unlist(lapply(tmpl, is, "simList")))
      origEnv <- tmpl[[whSimList[1]]]@.envir
      isListOfSimLists <-
        if (is.list(object)) if (is(object[[1]], "simList")) TRUE else FALSE else FALSE

      if (isListOfSimLists) {
        for (i in seq_along(object)) {
          keepFromOrig <- !(ls(origEnv) %in% ls(object[[i]]@.envir))
          list2env(mget(ls(origEnv)[keepFromOrig], envir = origEnv),
                   envir = simListOut[[i]]@.envir)
        }
      } else {
        keepFromOrig <- !(ls(origEnv) %in% ls(object@.envir))
        list2env(mget(ls(origEnv)[keepFromOrig], envir = origEnv),
                 envir = simListOut@.envir)
      }
    return(simListOut)
})

if (!isGeneric(".addTagsToOutput")) {
  setGeneric(".addTagsToOutput", function(object, outputObjects, FUN) {
    standardGeneric(".addTagsToOutput")
  })
}

#' addTagsToOutput for simList class objects
#'
#' See \code{\link[reproducible]{.addTagsToOutput}}.
#'
#' @inheritParams reproducible::.addTagsToOutput
#'
#' @author Eliot MciIntire
#' @docType methods
#' @exportMethod .addTagsToOutput
#' @export
#' @importFrom reproducible .addTagsToOutput
#' @importMethodsFrom reproducible .addTagsToOutput
#' @include simList-class.R
#' @rdname addTagsToOutput
#' @seealso \code{\link[reproducible]{.addTagsToOutput}}
#'
setMethod(
  ".addTagsToOutput",
  signature = "simList",
  definition = function(object, outputObjects, FUN) {
    if (!is.null(outputObjects)) {
      outputToSave <- object
      outputToSave@.envir <- new.env()
      list2env(mget(outputObjects, envir = object@.envir), envir = outputToSave@.envir)
      attr(outputToSave, "tags") <- attr(object, "tags")
      attr(outputToSave, "call") <- attr(object, "call")
      if (isS4(FUN))
        attr(outputToSave, "function") <- attr(object, "function")
    } else {
      outputToSave <- object
    }
    outputToSave
})

if (!isGeneric(".objSizeInclEnviros")) {
  setGeneric(".objSizeInclEnviros", function(object) {
    standardGeneric(".objSizeInclEnviros")
  })
}

#' objSizeInclEnviros for simList class objects
#'
#' See \code{\link[reproducible]{.objSizeInclEnviros}}.
#'
#' @importFrom reproducible .objSizeInclEnviros
#' @importMethodsFrom reproducible .objSizeInclEnviros
#' @inheritParams reproducible::.objSizeInclEnviros
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.objSizeInclEnviros}}
#' @exportMethod .objSizeInclEnviros
#' @export
#' @rdname objSizeInclEnviros
setMethod(
  ".objSizeInclEnviros",
  signature = "simList",
  definition = function(object) {
    object.size(as.list(object@.envir, all.names = TRUE)) + object.size(object)
})
