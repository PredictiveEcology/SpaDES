# register the S3 `igraph` class for use with S4 methods.
setOldClass("igraph")

################################################################################
#' Build edge list for module dependency graph
#'
#' @param sim A \code{simList} object.
#'
#' @param plot  Logical indicating whether the edgelist (and subsequent graph)
#'              will be used for plotting. If \code{TRUE}, duplicated rows
#'              (i.e., multiple object dependencies between modules) are removed
#'              so that only a single arrow is drawn connecting the modules.
#'              Default is \code{FALSE}.
#'
#' @return A \code{data.frame} whose first two columns give a list of edges
#'          and remaining columns the attributes of the dependency objects
#'          (object name, class, etc.).
#'
#' @include simList.R
#'
#' @import dplyr
#' @export
#' @docType methods
#' @rdname depsEdgeList-method
#'
#' @author Alex Chubaty
#'
setGeneric("depsEdgeList", function(sim, plot) {
  standardGeneric("depsEdgeList")
})

#' @rdname depsEdgeList-method
#'
setMethod("depsEdgeList",
          signature(sim="simList", plot="logical"),
          definition=function(sim, plot) {
            deps <- simDepends(sim)
            sim.in <- sim.out <- data.frame(objName=character(),
                                            objClass=character(),
                                            module=character())

            lapply(deps@dependencies, function(x) {
              z.in <- x@inputObjects
              z.out <- x@outputObjects
              z.in$module <- z.out$module <- x@name
              if (!all(is.na(z.in[,1:2]))) sim.in <<- rbind(sim.in, z.in)
              if (!all(is.na(z.out[,1:2]))) sim.out <<- rbind(sim.out, z.out)
              return(invisible(NULL))
            })

            dx <- left_join(sim.in, sim.out, by="name") %>%
              mutate(module.y=replace(module.y, is.na(module.y), "_IN_"))

            df <- with(dx, data.frame(from=module.y, to=module.x, objName=name,
                                     objClass=class.x, stringsAsFactors=FALSE))
            if (plot) df <- df[!duplicated(df[,1:2]),]
            return(df)
})

#' @rdname depsEdgeList-method
#'
setMethod("depsEdgeList",
          signature(sim="simList", plot="missing"),
          definition=function(sim, plot) {
            el <- depsEdgeList(sim, plot=FALSE)
})

################################################################################
#' Build a module dependency graph
#'
#' @inheritParams depsEdgeList
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
setGeneric("depsGraph", function(sim, plot) {
  standardGeneric("depsGraph")
})

#' @rdname depsGraph-method
#'
setMethod("depsGraph",
          signature(sim="simList", plot="logical"),
          definition=function(sim, plot) {
            edgeList <- depsEdgeList(sim, plot)
            if (!plot) {
              edgeList <- depsPruneCycles(edgeList)
            }
            return(graph.data.frame(edgeList))
})

#' @rdname depsGraph-method
#'
setMethod("depsGraph",
          signature(sim="simList", plot="missing"),
          definition=function(sim) {
            edgeList <- depsEdgeList(sim, plot=FALSE)
            return(graph.data.frame(edgeList))
})

################################################################################
#' Determine module load order
#'
#' Checks module dependencies and attempts to enusure that cyclic dependencies
#' can be resolved, checking objects in the global environment, and finally,
#' attempts to determine the load order for modules in the simulation.
#'
#' Uses \code{\link{topological.sort}} to try to find a load order satisfying
#' all module object dependencies.
#'
#' @inheritParams depsEdgeList
#'
#' @return Character vector of module names, sorted in correct load order.
#'
#' @include simList.R
#'
#' @import igraph
#' @export
#' @docType methods
#' @rdname depsLoadOrder-method
#'
#' @author Alex Chubaty
#'
setGeneric("depsLoadOrder", function(simGraph) {
  standardGeneric("depsLoadOrder")
})

#' @rdname depsLoadOrder-method
#'
setMethod("depsLoadOrder",
          signature(simGraph="igraph"),
          definition=function(simGraph) {
            # only works if simGraph is acyclic!
            tsort <- topological.sort(simGraph)
            loadOrder <- names(simGraph[[tsort,]])
            return(loadOrder)
})
