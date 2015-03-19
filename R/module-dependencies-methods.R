if(getRversion() >= "3.1.0") utils::globalVariables(c(".", "module.x", "module.y", "from", "to", "name"))

# register the S3 `igraph` class for use with S4 methods.
setOldClass("igraph")
selectMethod("show", "igraph")

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
#' @return A \code{data.table} whose first two columns give a list of edges
#'          and remaining columns the attributes of the dependency objects
#'          (object name, class, etc.).
#'
#' @include simList.R
#'
#' @export
#' @import data.table
#' @importFrom magrittr '%>%'
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
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
            sim.in <- sim.out <- data.frame(name=character(),
                                            class=character(),
                                            module=character(),
                                            stringsAsFactors=FALSE) %>%
                                 as.data.table

            lapply(deps@dependencies, function(x) {
              if (!is.null(x)) {
                z.in <- as.data.table(x@inputObjects)
                z.out <- as.data.table(x@outputObjects)
                z.in$module <- z.out$module <- x@name
                if (!all(is.na(z.in[,name]), is.na(z.in[,class]))) {
                  sim.in <<- rbindlist(list(sim.in, z.in), use.names=TRUE)
                }
                if (!all(is.na(z.out[,1:2]), is.na(z.out[,class]))) {
                  sim.out <<- rbindlist(list(sim.out, z.out), use.names=TRUE)
                }
              }
              return(invisible(NULL)) # return from the lapply
            })

            if ((nrow(sim.in)) && (nrow(sim.out))) {
              dx <- left_join(sim.in, sim.out, by="name") %>%
                mutate(module.y=replace(module.y, is.na(module.y), "_INPUT_"))

              dt <- with(dx, data.frame(from=module.y, to=module.x,
                                        objName=name, objClass=class.x,
                                        stringsAsFactors=FALSE)) %>%
                    as.data.table
              if (plot) dt <- dt[!duplicated(dt[,1:2]),]
            } else {
              dt <- data.frame(from=character(), to=character(), objName=character(),
                               objClass=character(), stringsAsFactors=FALSE) %>%
                    as.data.table
            }
            return(dt)
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
#' @importFrom magrittr '%>%'
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
              el <- depsEdgeList(sim, plot) %>% depsPruneEdges
            }
            return(graph.data.frame(el))
})

################################################################################
#' Prune edges to remove cycles in module dependencies
#'
#' Attempts to identify cycles in the dependency graph and remove edges representing
#' object dependencies which are provided by other modules in the simulation.
#'
#' @param simEdgeList An edge list (\code{data.table}) produced by \code{\link{depsEdgeList}}.
#'
#' @return An updated edge list object.
#'
#' @include simList.R
#'
#' @import data.table
#' @import igraph
#' @importFrom magrittr '%>%'
#' @importFrom dplyr anti_join
#' @importFrom dplyr lead
#' @importFrom dplyr inner_join
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
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
          signature(simEdgeList="data.table"),
          definition=function(simEdgeList) {
            simGraph <- graph.data.frame(simEdgeList)
            M <- shortest.paths(simGraph, mode="out")
            if (nrow(M)>1) {
              pth <- data.frame(from=character(), to=character()) %>% as.data.table()
              for (row in 1L:(nrow(M)-1L)) {
                for (col in (row+1L):ncol(M)) {
                  current <- M[row,col]
                  partner <- M[col,row]
                  if (all((current>0), !is.infinite(current), (partner>0), !is.infinite(partner))) {
                    pth1 <- get.shortest.paths(simGraph,
                                               from=rownames(M)[row],
                                               to=colnames(M)[col])$vpath[[1]]
                    pth1 <- data.frame(from=rownames(M)[pth1],
                                       to=rownames(M)[lead(pth1, 1)],
                                       stringsAsFactors = FALSE) %>%
                            na.omit %>% as.data.table

                    pth2 <- get.shortest.paths(simGraph,
                                               from=colnames(M)[col],
                                               to=rownames(M)[row])$vpath[[1]]
                    pth2 <- data.frame(from=rownames(M)[pth2],
                                       to=rownames(M)[lead(pth2, 1)],
                                       stringsAsFactors = FALSE) %>%
                            na.omit %>% as.data.table

                    pth <- rbindlist(list(pth, rbindlist(list(pth1, pth2))))
                  }
                }
              }
              pth <- pth %>% inner_join(simEdgeList, by=c("from","to"))

              # What is not provided in modules, but needed
              missingObjects <- simEdgeList %>% filter(from!=to) %>%
                anti_join(pth,., by=c("from","to"))
              if (nrow(missingObjects)) {
                warning("Problem resolving the module dependencies:\n",
                        missingObjects)
              }

              # What is provided in modules, and can be omitted from simEdgeList object
              newEdgeList <- simEdgeList %>% filter(from!=to) %>% anti_join(pth, by=c("from","to"))
            } else {
              newEdgeList <- simEdgeList
            }
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
#' @param sim         A \code{simList} object.
#'
#' @param simGraph    An \code{\link{igraph}} object produced by \code{\link{depsGraph}}.
#'
#' @return Character vector of module names, sorted in correct load order.
#'
#' @include simList.R
#'
#' @importFrom magrittr '%>%'
#' @import igraph
#' @export
#' @docType methods
#' @rdname depsLoadOrder-method
#'
#' @author Alex Chubaty
#'
setGeneric("depsLoadOrder", function(sim, simGraph) {
  standardGeneric("depsLoadOrder")
})

#' @rdname depsLoadOrder-method
#'
setMethod("depsLoadOrder",
          signature(sim="simList", simGraph="igraph"),
          definition=function(sim, simGraph) {
            # only works if simGraph is acyclic!
            tsort <- topological.sort(simGraph)
            if (length(tsort)) {
              loadOrder <- names(simGraph[[tsort,]]) %>% .[!(. %in% "_INPUT_" )]
            } else {
              loadOrder <- unlist(simModules(sim))
            }
            return(loadOrder)
})
