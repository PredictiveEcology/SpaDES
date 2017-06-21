#' Copy a simList object
#'
#' Because a simList works with an environment to hold all objects,
#' all objects within that slot are pass-by-reference. That means
#' it is not possible to simply copy an object with an assignment operator:
#' the two objects will share the same objects. As one simList object changes
#' so will the other. when this is not the desired behaviour, use this function.
#' NOTE: use capital C, to limit confusion with \code{data.table::copy()}
#'
#'
#'
#' @param sim  A \code{simList} object.
#' @param objects  Whether the objects contained within the simList environment
#'                 should be copied. Default = TRUE, which may be slow.
#' @param queues Logical. Should the events queues (\code{events},
#'               \code{current}, \code{completed}) be deep copied via
#'               \code{data.table::copy}
#' @export
#' @docType methods
#' @importFrom reproducible prepareFileBackedRaster
#' @rdname Copy
setGeneric("Copy", function(sim, objects, queues) {
  standardGeneric("Copy")
})

#' @rdname Copy
setMethod("Copy",
          signature(sim = "simList", objects = "logical", queues = "logical"),
          definition = function(sim, objects, queues) {
            sim_ <- sim
            if (queues) {
              sim_@events <- data.table::copy(sim@events)
              sim_@completed <- data.table::copy(sim@completed)
              sim_@current <- data.table::copy(sim@current)
            }
            if (objects) {
              sim_@.envir <- new.env(parent = parent.env(sim@.envir))
              objs <- mget(ls(sim@.envir, all.names = TRUE),
                           envir = sim@.envir)
              isDataTable <- unlist(lapply(objs, function(x) is(x, "data.table")))
              objs[isDataTable] <- lapply(objs[isDataTable], function(y) data.table::copy(y))
              isRaster <- unlist(lapply(objs, function(x) is(x, "Raster")))
              objs[isRaster] <- lapply(objs[isRaster], function(y)
                prepareFileBackedRaster(y, repoDir = cachePath(sim_)))
              sim_@.envir <- list2env(objs,
                                      envir = sim_@.envir)
            }
            return(sim_)
})

#' @rdname Copy
setMethod("Copy",
          signature(sim = "simList", objects = "missing", queues = "missing"),
          definition = function(sim) {
            sim_ <- Copy(sim, objects = TRUE, queues = TRUE)
            return(sim_)
})

#' @rdname Copy
#' @export
#' @inheritParams Copy
copy <- function(sim, objects = TRUE) {
   #.Deprecated("Copy", old = "copy")
   Copy(sim = sim, objects = objects)
}
