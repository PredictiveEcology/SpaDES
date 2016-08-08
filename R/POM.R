################################################################################
#' Use Pattern Oriented Modeling to fit unknown parameters
#'
#' This is very much in alpha condition. It has been tested on simple problems,
#' as shown in the examples, with up to 2 parameters.
#' It appears that DEoptim is the superior package for these stochastic problems.
#' This should be used with caution as with all optimization routines.
#'
#' @inheritParams spades
#' @inheritParams splitRaster
#'
#' @param params Character vector of parameter names that can be changed by the optimizer. These
#'               must be accessible with params(sim) internally.
#' @param objects A named list. The names of each list element must correspond to an object in the
#'                .GlobalEnv and the list elements must be objects that can be accessed in the
#'                envir(sim). Each of these pairs will be assessed against one another using
#'                the \code{objFnCompare}. Each pair will be standardized from 0 to 1. This can
#'                also be a function of objects found in envir(sim). See examples.
#'
#' @param objFn An objective function to be passed into \code{optimizer}
#'
#' @param optimizer The function to use to optimize. Default is
#'                  "DEoptim". Currently it can also be "optim" or "rgenoud", which
#'                  use stats::optim or rgenoud::genoud, respectively.
#'
#' @param sterr Logical. If using \code{optimizer = "optim"}, the hessian can be calculated.
#'              If this is TRUE, then the standard errors can be estimated using
#'              that hessian, assuming normality.
#'
#' @param optimControl List of control arguments passed into the control of each
#'                     optimization routine. Currently, only passed to
#'                     \code{\link{DEoptim.control}} when \code{optimizer} is \code{"DEoptim"}
#'
#' @param ... All objects needed in objFn
#'
#' @param objFnCompare Character string. Either, "MAD" or "RMSE" indicating that inside the objective
#'                     function, data and prediction will be compared by Mean Absolute Deviation or
#'                     Root Mean Squared Error. Default is "MAD".
#'
#' @return The values for parameters used in objFn that minimize the objFn.
#'
#' @seealso \code{\link{spades}}, \code{\link[parallel]{makeCluster}},
#' \code{\link{simInit}}
#'
#' @include module-dependencies-class.R
#' @include helpers.R
#' @include simList-class.R
#' @include environment.R
#' @include priority.R
#' @importFrom DEoptim DEoptim DEoptim.control
#' @importFrom stats optim
#' @importFrom raster getCluster returnCluster
#' @importFrom parallel clusterExport
#' @export
#' @docType methods
#' @rdname POM
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{
#'  set.seed(89462)
#'  library(parallel)
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      fireSpread = list(nFires = 5),
#'      randomLandscapes = list(nx = 300, ny = 300)
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  out <- spades(copy(mySim), .plotInitialTime = NA)
#'  fireData <- sum(getValues(out$landscape$Fires))/ncell(out$landscape$Fires)
#'  clearPlot()
#'  Plot(out$landscape$Fires)
#'
#'  #cl <- makeCluster(8)
#'  fireFn <- function(landscape) {
#'               sum(getValues(landscape$Fires))/ncell(landscape$Fires)
#'             }
#'  out <- POM(mySim, "spreadprob",
#'             list(fireData = fireFn),
#'             hessian = TRUE) # using optim, can get Hessian
#'  #    cl = cl)
#'  out <- POM(mySim, "spreadprob", list(fireData = fireFn),
#'             optimizer = "DEoptim")#, cl = cl)
#'
#'  # Two parameters
#'  N <- length(out$caribou)/1000
#'  caribouFn <- function(caribou) length(caribou)/1000
#'  aTime <- Sys.time()
#'  out2 <- POM(mySim, c("spreadprob", "N"),
#'     list(fireData = fireFn,
#'          N = caribouFn), optimizer = "DEoptim",
#'      cl = cl)
#'  bTime <- Sys.time()
#'  out3 <- POM(mySim, c("spreadprob", "N"),
#'     list(fireData = fireFn,
#'          N = caribouFn), hessian = TRUE)#,
#'      #cl = cl)
#'  cTime <- Sys.time()
#'  out4 <- POM(mySim, c("spreadprob", "N"),
#'     list(fireData = fireFn,
#'          N = caribouFn), optimizer = "genoud",
#'      cl = cl3)
#'  dTime <- Sys.time()
#'  print(paste("DEoptim", format(bTime - aTime)))
#'  print(paste("optim", format(cTime - bTime)))
#'  print(paste("genoud", format(dTime - cTime)))
#'  #stopCluster(cl)
#'
#'  )
#'  }
setGeneric(
  "POM",
  function(sim, params, objects, objFn, cl, optimizer = "optim",
           sterr = FALSE, ..., objFnCompare = "MAD", optimControl = NULL) {
    standardGeneric("POM")
})

#' @rdname POM
setMethod(
  "POM",
  signature(sim = "simList", params = "character", objects = "ANY"),
  definition = function(sim, params, objects, objFn, cl, optimizer,
                        sterr, ..., objFnCompare, optimControl) {

    if (missing(cl)) {
      cl <- tryCatch(getCluster(), error = function(x) NULL)
      on.exit(if (!is.null(cl)) returnCluster())
      clProvided <- FALSE
    } else {
      clProvided <- TRUE
    }

    paramNames <- lapply(SpaDES::params(sim), names)
    whParams <- lapply(paramNames, function(pn) match(params, pn))
    whModules <- unlist(lapply(whParams, function(mod) any(!is.na(mod))))

    whParamsByMod <- unlist(lapply(whParams, na.omit))
    #whParamsList1 <- match(params, unlist(lapply(SpaDES::params(sim), names)))

    range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

    if (missing(objFn)) {
      objFn <- function(par, objects, simList, whModules, whParams, whParamsByMod) {
        whP <- 0
        for (wh in seq_along(whParamsByMod)) {
          whP <- whP + 1
          params(simList)[[names(whParamsByMod)[wh]]][[whParamsByMod[wh]]] <-
            par[whP]
        }

        out <- spades(SpaDES::copy(simList), .plotInitialTime = NA)

        outputObjects <- lapply(objects, function(objs) {
          if (is.function(objs)) {
            dat <- mget(names(formals(objs)), envir = envir(out))
            do.call(objs, dat)
          } else {
            eval(parse(text = objs[[1]])[[1]], envir = envir(out))
          }
        })

        objectiveRes <- unlist(lapply(seq_along(outputObjects), function(x) {
          if (is(outputObjects[[x]], "Raster")) {
            outObj <- getValues(outputObjects[[x]])
            dataObj <- getValues(get(names(outputObjects)[x]))
          } else {
            outObj <- outputObjects[[x]]
            dataObj <- get(names(outputObjects)[[x]])
          }

          if (objFnCompare == "MAD") {
            if (length(outObj) == 1) {
              mean(abs((outObj - dataObj)))
            } else {
              mean(abs(range01(outObj - dataObj)))
            }
          } else if (objFnCompare == "RMSE") {
            sqrt(mean((outObj - dataObj)^2))
          } else {
            stop("objFnCompare must be either MAD or RMSE, see help")
          }

        }))
        sum(objectiveRes)
    }}

    if (!is.null(cl)) {
      clusterExport(cl, c("sim", names(objects)), envir = sys.frame(1))
      clusterEvalQ(cl, {
        library(SpaDES)
        library(RColorBrewer)
        library(raster)
      })
    }

    deps <- depends(sim)@dependencies
    whP <- 0
    par <- numeric(length(whParamsByMod))
    lowerRange <- numeric(length(whParamsByMod))
    upperRange <- numeric(length(whParamsByMod))
    for (wh in seq_along(whParamsByMod)) {
      whP <- whP + 1
      modName <- names(whParamsByMod)[whP]
      par[whP] <- unlist(deps[[modName]]@parameters$default[deps[[modName]]@parameters$paramName == names(p(sim, modName)[whParamsByMod[whP]])])
      upperRange[whP] <- unlist(deps[[modName]]@parameters$max[deps[[modName]]@parameters$paramName == names(p(sim, modName)[whParamsByMod[whP]])])
      lowerRange[whP] <- unlist(deps[[modName]]@parameters$min[deps[[modName]]@parameters$paramName == names(p(sim, modName)[whParamsByMod[whP]])])
    }
    deoptimArgs <- list(fn = objFn, lower = lowerRange, upper = upperRange,
                          simList = sim, objects = objects,
                          whModules = whModules, whParams = whParams,
                          whParamsByMod = whParamsByMod)


    if (optimizer == "DEoptim") {
      if (!is.null(cl)) {
        deoptimArgs <- append(deoptimArgs,
                              list(control = DEoptim.control(parallelType = 3),
                                   cl = cl))
      }
      deoptimArgs <- append(deoptimArgs,
                            list(control = DEoptim.control(NP = 20*length(lowerRange),
                                                           itermax = 20)))
      if (!is.null(optimControl)) {
        deoptimArgs$control[names(optimControl)] <- optimControl
      }
      output <- do.call("DEoptim", deoptimArgs)
    } else {
      if (!is.null(list(...)$hessian) | sterr)
        deoptimArgs <- append(deoptimArgs,
                              list(hessian = TRUE))

      if (optimizer == "genoud") {
        if (!is.null(cl)) {
          #do.call(DEoptim.control, list(parallelType=3))
          deoptimArgs <- append(deoptimArgs,
                                list(control = DEoptim.control(parallelType = 3),
                                     cl = cl))
        }
        deoptimArgs <- append(deoptimArgs,
                              list(nvars = length(lowerRange)))
        deoptimArgs$Domains <- cbind(lowerRange, upperRange)
        deoptimArgs$boundary.enforcement <- 2
        deoptimArgs$lower <- NULL
        deoptimArgs$upper <- NULL
        if (requireNamespace("rgenoud")) {
          output <- do.call("genoud", deoptimArgs)
        } else {
          stop("rgenoud package is not installed. Please install, install.packages(\"rgenoud\")")
        }
      } else {
        deoptimArgs <- append(deoptimArgs,
                              list(par = par, method = "L-BFGS-B",
                                   control = list(trace = 3, REPORT = 3)))
        #if(!is.null(list(...)$hessian) | sterr)
        #  deoptimArgs <- append(deoptimArgs,
        #                        list(hessian = TRUE))

        output <- do.call("optim", deoptimArgs)
      }
    }
    #if(!clProvided) stopCluster(cl)
    if (sterr) {
      output$sterr <- try(sqrt(abs(diag(solve(output$hessian)))))
    }
    output$args <- deoptimArgs
    return(output)
})
