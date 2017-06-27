### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "module.x", "module.y", "from", "to", "name",
                           "objectName", "objectClass", "other", "module",
                           "i.objectClass", "i.module", "sourceURL"))
}

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
#' @include simList-class.R
#'
#' @export
#' @importFrom data.table ':=' data.table rbindlist setkey setorder
#' @docType methods
#' @rdname depsEdgeList
#'
#' @author Alex Chubaty
#'
setGeneric("depsEdgeList", function(sim, plot) {
  standardGeneric("depsEdgeList")
})

#' @rdname depsEdgeList
setMethod(
  "depsEdgeList",
  signature(sim = "simList", plot = "logical"),
  definition = function(sim, plot) {
    deps <- depends(sim)
    sim.in <- sim.out <- data.table(objectName = character(0),
                                    objectClass = character(0),
                                    module = character(0))

    lapply(deps@dependencies, function(x) {
      if (!is.null(x)) {
        z.in <- as.data.table(x@inputObjects)[, .(objectName, objectClass)]
        z.out <- as.data.table(x@outputObjects)[, .(objectName, objectClass)]
        z.in$module <- z.out$module <- x@name
        if (!all(is.na(z.in[, objectName]), is.na(z.in[, objectClass]))) {
          sim.in <<- rbindlist(list(sim.in, z.in), use.names = TRUE)
        }
        if (!all(is.na(z.out[, 1:2]), is.na(z.out[, objectClass]))) {
          sim.out <<- rbindlist(list(sim.out, z.out), use.names = TRUE)
        }
      }
      return(invisible(NULL)) # return from the lapply
    })

    setkey(sim.in, "objectName")
    setkey(sim.out, "objectName")

    if ((nrow(sim.in)) && (nrow(sim.out))) {
      dx <- sim.out[sim.in, nomatch = NA_character_, allow.cartesian = TRUE]
      dx[is.na(module), module := "_INPUT_"]
      DT <- dx[, list(from = module, to = i.module,
                      objName = objectName, objClass = i.objectClass)]

      if (plot) DT <- DT[!duplicated(DT[, 1:2, with = FALSE]), ]
    } else {
      DT <- data.table(from = character(0), to = character(0),
                       objName = character(0), objClass = character(0))
    }
    setorder(DT, "from", "to", "objName")
    return(DT)
})

#' @rdname depsEdgeList
setMethod("depsEdgeList",
          signature(sim = "simList", plot = "missing"),
          definition = function(sim, plot) {
            depsEdgeList(sim, plot = FALSE)
})

################################################################################
#' Build a module dependency graph
#'
#' @inheritParams depsEdgeList
#'
#' @return An \code{\link{igraph}} object.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname depsGraph
#'
#' @author Alex Chubaty
# igraph is being imported in spades-package.R
# igraph exports %>% from magrittr
setGeneric("depsGraph", function(sim, plot) {
  standardGeneric("depsGraph")
})

#' @export
#' @rdname depsGraph
setMethod("depsGraph",
          signature(sim = "simList", plot = "logical"),
          definition = function(sim, plot) {
            if (plot) {
              el <- depsEdgeList(sim, plot)
            } else {
              el <- depsEdgeList(sim, plot) %>% .depsPruneEdges()
            }
            core <- .coreModules() %>% unname() %>% unlist()
            m <- sim@modules %>% unlist()
            v <- unique(c(el$to, el$from, m[-which(m %in% core)]))
            return(graph_from_data_frame(el, vertices = v, directed = TRUE))
})

#' @export
#' @rdname depsGraph
setMethod("depsGraph",
          signature(sim = "simList", plot = "missing"),
          definition = function(sim) {
            return(depsGraph(sim, FALSE))
})

################################################################################
#' Prune edges to remove cycles in module dependencies
#'
#' Internal function.
#' Attempts to identify cycles in the dependency graph and remove edges representing
#' object dependencies which are provided by other modules in the simulation.
#'
#' @param simEdgeList An edge list (\code{data.table}) produced by \code{\link{depsEdgeList}}.
#'
#' @return An updated edge list object.
#'
#' @include simList-class.R
#'
#' @importFrom data.table as.data.table data.table rbindlist
#' @importFrom dplyr anti_join bind_rows filter inner_join lead
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @keywords internal
#' @rdname depsPruneEdges
#'
#' @author Alex Chubaty
# igraph is being imported in spades-package.R
# igraph exports %>% from magrittr
setGeneric(".depsPruneEdges", function(simEdgeList) {
  standardGeneric(".depsPruneEdges")
})

#' @rdname depsPruneEdges
setMethod(
  ".depsPruneEdges",
  signature(simEdgeList = "data.table"),
  definition = function(simEdgeList) {
    simGraph <- graph_from_data_frame(simEdgeList)
    M <- distances(simGraph, mode = "out")
    if (nrow(M) > 1) {
      pth <- data.table(from = character(0), to = character(0))
      for (row in 1L:(nrow(M) - 1L)) {
        for (col in (row + 1L):ncol(M)) {
          current <- M[row, col]
          partner <- M[col, row]
          if (all((current > 0), !is.infinite(current), (partner > 0),
                  !is.infinite(partner))) {
            pth1 <- shortest_paths(simGraph,
                                   from = rownames(M)[row],
                                   to = colnames(M)[col])$vpath[[1]]
            pth1 <- data.frame(from = rownames(M)[pth1],
                               to = rownames(M)[lead(match(names(pth1), rownames(M)), 1)],
                               stringsAsFactors = FALSE) %>%
                    na.omit %>% as.data.table()

            pth2 <- shortest_paths(simGraph,
                                   from = colnames(M)[col],
                                   to = rownames(M)[row])$vpath[[1]]
            pth2 <- data.frame(from = rownames(M)[pth2],
                               to = rownames(M)[lead(match(names(pth2), rownames(M)), 1)],
                               stringsAsFactors = FALSE) %>%
                    na.omit %>% as.data.table()

            pth <- rbindlist(list(pth, rbindlist(list(pth1, pth2))))
          }
        }
      }
      pth <- pth %>% inner_join(simEdgeList, by = c("from", "to"))

      # what is not provided in modules, but needed
      missingObjects <- simEdgeList %>% filter(from != to) %>%
        anti_join(pth, ., by = c("from", "to"))
      if (nrow(missingObjects)) {
        warning("Problem resolving the module dependencies:\n",
                paste(missingObjects), collapse = "\n")
      }

      # what is provided in modules, and can be omitted from simEdgeList object
      newEdgeList <- simEdgeList %>%
        filter(from != to) %>%
        anti_join(pth, by = c("from", "to"))
    } else {
      newEdgeList <- simEdgeList
    }
    return(newEdgeList %>% data.table() %>% setorder("from", "to", "objName"))
})

################################################################################
#' Determine module load order
#'
#' Internal function.
#' Checks module dependencies and attempts to ensure that cyclic dependencies
#' can be resolved, checking objects in the global environment, and finally,
#' attempts to determine the load order for modules in the simulation.
#'
#' Uses \code{\link[igraph]{topo_sort}} to try to find a load order satisfying
#' all module object dependencies.
#'
#' @param sim         A \code{simList} object.
#'
#' @param simGraph    An \code{\link{igraph}} object produced by \code{\link{depsGraph}}.
#'
#' @return Character vector of module names, sorted in correct load order.
#'
#' @include simList-class.R
#' @export
#' @keywords internal
#' @docType methods
#' @rdname depsLoadOrder
#'
#' @author Alex Chubaty
# igraph is being imported in spades-package.R
# igraph exports %>% from magrittr
setGeneric(".depsLoadOrder", function(sim, simGraph) {
  standardGeneric(".depsLoadOrder")
})

#' @rdname depsLoadOrder
setMethod(".depsLoadOrder",
          signature(sim = "simList", simGraph = "igraph"),
          definition = function(sim, simGraph) {
            # only works if simGraph is acyclic!
            tsort <- topo_sort(simGraph, "out")
            if (length(tsort)) {
              loadOrder <- names(simGraph[[tsort, ]]) %>% .[!(. %in% "_INPUT_" )]
            } else {
              modules <- unlist(sim@modules)
              if (length(sim@modules)) {
                loadOrder <- modules
              } else {
                loadOrder <- character()
              }
            }
            # make sure modules with no deps get added
            if (!all(sim@modules %in% loadOrder)) {
              ids <- which(sim@modules %in% loadOrder)
              noDeps <- unlist(sim@modules)[-ids]
              loadOrder <- c(loadOrder, noDeps)
            }
            return(loadOrder)
})
