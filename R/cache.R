#' makeDigestible for simList class objects
#'
#' See \code{\link[reproducible]{makeDigestible}}. This strips out stuff
#' from a simList class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems,
#' or machines. This will likely still not allow identical digest
#' results across R versions.
#'
#' @importFrom reproducible makeDigestible
#' @importMethodsFrom reproducible makeDigestible
#' @inheritParams reproducible makeDigestible
#' @rdname makeDigestible
#' @include simList-class.R
#' @export
setGeneric("makeDigestible", function(object, objects,
                                      compareRasterFileLength = 1e6,
                                      algo = "xxhash64") {
  standardGeneric("makeDigestible")
})

#' @exportMethod makeDigestible
setMethod(
  "makeDigestible",
  signature = "simList",
  definition = function(object, objects, compareRasterFileLength, algo) {
    objectsToDigest <- sort(ls(object@.envir, all.names = TRUE), method = "radix")
    if (!missing(objects)) {
      objectsToDigest <- objectsToDigest[objectsToDigest %in% objects]
    }
    envirHash <- (sapply(objectsToDigest, function(x) {
      if (!(x == ".sessionInfo")) {
        obj <- get(x, envir = envir(object))
        if (is(obj, "cluster")) {
          dig <- NULL
        } else {
          if (!is(obj, "function") & !is(obj, "expression")) {
            if (is(obj, "Raster")) {
              # convert Rasters in the simList to some of their metadata.
              dig <- makeDigestible(obj,
                                    compareRasterFileLength = compareRasterFileLength,
                                    algo = algo)
            } else if (is(obj, "Spatial")) {
              dig <- makeDigestible(obj, algo = algo)
            } else {
              if (is.character(obj)) {
                if (any(grepl("\\/", obj))) { # test for paths
                  obj <- basename(obj)
                }
              }
              # convert functions in the simList to their digest.
              #  functions have environments so are always unique
              dig <- fastdigest::fastdigest(obj)
            }
          } else {
            # for functions, use a character representation via format
            dig <- fastdigest::fastdigest(format(obj))
          }
        }
      } else {
        # for .sessionInfo, just keep the major and minor R version
        dig <- fastdigest::fastdigest(get(x, envir = envir(object))[[1]] %>%
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

