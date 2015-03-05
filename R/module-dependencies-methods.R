################################################################################
#' Build a module dependency graph
#'
#' @param sim A \code{\link{simList}} object.
#'
#' @inheritParams depsGraph-method
#'
#' @return An \code{\link{igraph}} object.
#'
#' @include simList.R
#'
#' @import igraph
#' @export
#' @docType methods
#' @rdname depsGraph-method
#'
#' @author Alex Chubaty
#'
setGeneric("depsGraph", function(sim) {
  standardGeneric("depsGraph")
})

#' @rdname depsGraph-method
#'
setMethod("depsGraph",
          signature(sim="simList"),
          definition=function(sim) {
            deps <- simDepends(sim)
            g <-
              return(g)
          })
