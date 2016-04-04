################################################################################
#' Run an experiment using spades
#'
#' This is essentially a wrapper around the \code{spades} call that allows for
#' parameters or modules to vary. This function will create a fully factorial experiment
#' among all levels of the variables passed into the function. The function
#' requires a \code{simList} object, plus optional params and/or modules and/or replications.
#'
#' @inheritParams spades
#'
#' @inheritParams simInit
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}
#'
#' @importFrom raster getCluster returnCluster
#' @export
#' @docType methods
#' @rdname spades
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  experiment(mySim, params=list(fireSpread = list(spreadprob = c(0.2, 0.23),
#'                                                  nFires=c(20, 10)),
#'                                caribouMovement = list(N = c(100, 1000))))
#' }
#'
setGeneric("experiment", function(sim, params, modules, objects, inputs, outputs, ...) {
  standardGeneric("experiment")
})

#' @rdname spades
setMethod(
  "experiment",
  signature(sim = "simList"),
  definition = function(sim, params, modules, objects, inputs, outputs, ...) {

    cl <- tryCatch(getCluster(), error=function(x) NULL)
    on.exit(returnCluster())

    parFun <- if(!is.null(cl)) { "clusterApplyLB" } else {"lapply"}

    paramsSimple <- unlist(params, recursive = FALSE)

    factorialExp <- expand.grid(paramsSimple)

    mod <- strsplit(names(paramsSimple), split="\\.") %>% sapply(function(x) x[1])
    param<- strsplit(names(paramsSimple), split="\\.") %>% sapply(function(x) x[2])

    browser()
    #sims <- get(parFun)(cl=cl, 1:NROW(factorialExp), function(ind) {
    sims <- get(parFun)(1:NROW(factorialExp), function(ind) {
        sim_ <- sim
      for(x in 1:length(mod)) {
        params(sim_)[[mod[x]]][[param[[x]]]] <- factorialExp[ind,x]
      }
        paths(sim_)$outputPath <- file.path(paths(sim_)$outputPath,
                                            paste(collapse = "-",substr(mod,1,4),substr(param, 1,6),factorialExp[ind,], sep="_"))

      sim_ <- spades(sim_, ...)
      return(sim_)
    })



    return(invisible(sim))
  })

#' @rdname spades
setMethod("spades",
          signature(sim = "simList", debug = "missing"),
          definition = function(sim) {
            stopifnot(class(sim) == "simList")
            return(spades(sim, debug = FALSE))
          })
