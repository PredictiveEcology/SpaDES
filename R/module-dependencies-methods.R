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
              mutate(module.y=replace(module.y, is.na(module.y), "_INPUT_"))

            df <- with(dx, data.frame(from=module.y, to=module.x, objName=name,
                                     objClass=class.x, stringsAsFactors=FALSE))
            if (plot) df <- df[!duplicated(df[,1:2]),]
            return(df)
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
            if (plot) {
              el <- depsEdgeList(sim, plot)
            } else {
              el <- depsEdgeList(sim, plot) %>% depsPruneEdges(.)
            }
            return(graph.data.frame(el))
})

################################################################################
#' Prune edges to remove cycles in module dependencies
#'
#' Attempts to identify cycles in the dependency graph and remove edges representing
#' object dependencies which are provided by other modules in the simulation.
#'
#' @param simEdgeList An edge list produced by \code{\link{depsEdgeList}}.
#'
#' @return An updated edge list object.
#'
#' @include simList.R
#'
#' @import igraph
#' @export
#' @docType methods
#' @rdname depsPruneEdges-method
#'
#' @author Alex Chubaty
#'
setGeneric("depsPruneEdges", function(simEdgeList) {
  standardGeneric("depsPruneEdges")
})

#' @rdname depsPruneEdges-method
#'
setMethod("depsPruneEdges",
          signature(simEdgeList="data.frame"),
          definition=function(simEdgeList) {
            simGraph <- graph.data.frame(simEdgeList)
            M <- shortest.paths(simGraph, mode="out")
            pth <- data.frame(from=character(), to=character())
            for (row in 1L:(nrow(M)-1)) {
              for (col in (row+1L):ncol(M)) {
                current <- M[row,col]
                partner <- M[col,row]
                if (all((current>0), !is.infinite(current), (partner>0), !is.infinite(partner))) {
                  pth1 <- get.shortest.paths(simGraph, from=rownames(M)[row], to=colnames(M)[col])$vpath[[1]]
                  pth1 <- data.frame(from=rownames(M)[pth1],to=rownames(M)[lead(pth1,1)], stringsAsFactors = FALSE) %>% na.omit

                  pth2 <- get.shortest.paths(simGraph, from=colnames(M)[col], to=rownames(M)[row])$vpath[[1]]
                  pth2 <- data.frame(from=rownames(M)[pth2],to=rownames(M)[lead(pth2,1)], stringsAsFactors = FALSE) %>% na.omit

                  pth <- bind_rows(pth,bind_rows(pth1, pth2))
                }
              }
            }
            pth <- pth %>% inner_join(simEdgeList)

            # What is not provided in modules, but needed
            missingObjects <- simEdgeList %>% filter(from!=to) %>% anti_join(pth,.)
            if (NROW(missingObjects)) {
              warning("Problem resolving the module dependencies:\n",
                      missingObjects)
            }

            # What is provided in modules, and can be omitted from simEdgeList object
            newEdgeList <- simEdgeList %>% filter(from!=to) %>% anti_join(pth)

            return(newEdgeList)
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
#' @inheritParams depsPruneEdges
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
            # -- ensure depsDetectCycles is called before this function
            tsort <- topological.sort(simGraph)
            loadOrder <- names(simGraph[[tsort,]]) %>% .[!(. %in% "_INPUT_" )]
            return(loadOrder)
})
