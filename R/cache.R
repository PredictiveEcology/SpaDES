if (!isGeneric("robustDigest")) {
    setGeneric("robustDigest", function(object, objects,
                                          compareRasterFileLength = 1e6,
                                          algo = "xxhash64") {
    standardGeneric("robustDigest")
  })
}

#' robustDigest for simList class objects
#'
#' See \code{\link[reproducible]{robustDigest}}. This strips out stuff
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
#' @rdname robustDigest
setMethod(
  "robustDigest",
  signature = "simList",
  definition = function(object, objects, compareRasterFileLength, algo) {
    allObjs <- ls(object@.envir, all.names = TRUE)
    objectsToDigest <- sort(allObjs, method = "radix")
    if (!missing(objects)) {
      objectsToDigest <- objectsToDigest[objectsToDigest %in% objects]
    }

    envirHash <- robustDigest(mget(objectsToDigest, envir=object@.envir))

    #envirHash <- envirHash[!sapply(envirHash, is.null)]
    envirHash <- sortDotsUnderscoreFirst(envirHash)
    object <- Copy(object, objects = FALSE, queues = FALSE)
    object@.envir <- new.env()
    #rm(list = allObjs, envir=object@.envir)

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
        object@depends@dependencies[[i]] <- lapply(slotNames(object@depends@dependencies[[i]]), function(x) slot(object@depends@dependencies[[i]],x))
        names(object@depends@dependencies[[i]]) <- slotNames(deps[[i]])
        object@depends@dependencies[[i]][["timeframe"]] <- as.Date(deps[[i]]@timeframe)
      }
    }

    # Sort the params and .list with dots first, to allow Linux and Windows to be compatible
    object@params <- lapply(object@params, function(x) sortDotsUnderscoreFirst(x))

    return(object)
  })

