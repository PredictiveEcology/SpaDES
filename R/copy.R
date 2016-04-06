### Copy a simList object
#'
#' Because a simList works with an environment to hold all objects,
#' all objects within that slot are pass-by-reference. That means
#' it is not possible to simply copy an object with an assignment operator:
#' the two objects will share the same objects. As one simList object changes
#' so will the other. when this is not the desired behaviour, use this function.
#'
#' @param sim  A \code{simList} object.
#' @export
#' @docType methods
#' @rdname copy
setGeneric("copy", function(sim) {
  standardGeneric("copy")
})

setMethod("copy",
          signature(sim = "simList"),
          definition=function(sim) {
            sim_ <- sim
            sim_@.envir <- new.env(parent = parent.env(envir(sim)))
            sim_@.envir <- list2env(mget(ls(sim@.envir, all.names=TRUE),
                                         envir=sim@.envir),
                                    envir=sim_@.envir)
            return(sim_)
          })

