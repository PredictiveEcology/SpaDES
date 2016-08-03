################################################################################
#' Use Pattern Oriented Modeling to fit unknown parameters
#'
#'
#' @inheritParams spades
#' @inheritParams splitRaster
#' @param objFn An objective function to be passed into
#'              \code{optimizer}
#' @param optimizer The function to use to optimize. Default is
#'                  optim. Can also to DEoptim.
#' @param sterr Logical. If using optim, the hessian can be calculated,
#'              and standard errors can be estimated, assuming normality.
#' @param ... All objects needed in objFn
#'
#' @param objFnCompare Character string. Either, "MAD" or "RMSE" indicating that inside the objective
#'                     function, data and prediction will be compared by Mean Absolute Deviation or
#'                     Root Mean Squared Error. Default is "MAD".
#'
#' @return The values for parameters used in objFn that minimize
#' the objFn.
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
#' @export
#' @docType methods
#' @rdname POM
#'
#' @author Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  set.seed(52461)
#'  library(parallel)
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      fireSpread = list(nFires = 1),
#'      randomLandscapes = list(nx = 200, ny = 200)
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'  out <- spades(copy(mySim), .plotInitialTime = NA)
#'  fireData <- out$landscape$Fires
#'  Plot(fireData, new=TRUE)
#'
#'  #cl <- makeCluster(6)
#'  out <- POM(mySim, "spreadprob", list(fireData = "landscape$Fires"),
#'             hessian = TRUE) # using optim, can get Hessian
#'  #    cl = cl)
#'
#'  N <- length(out$caribou)
#'  out2 <- POM(mySim, c("spreadprob", "N"),
#'     list(fireData = "landscape$Fires",
#'          N = function(caribou) length(caribou)), optimizer = "DEoptim")#,
#'      #cl = cl)
#'  out3 <- POM(mySim, c("spreadprob", "N"),
#'     list(fireData = "landscape$Fires",
#'          N = function(caribou) length(caribou)), hessian = TRUE)#,
#'      #cl = cl)
#'  #stopCluster(cl)
#'
#'  )
#'  }
setGeneric(
  "POM",
  function(sim, params, objects, objFn, cl, optimizer = "optim",
           sterr, ..., objFnCompare = "MAD") {
    standardGeneric("POM")
  })

#' @rdname POM
setMethod(
  "POM",
  signature(sim = "simList", params = "character", objects = "ANY"),
  definition = function(sim, params, objects, objFn, cl, optimizer,
                        sterr, ..., objFnCompare) {

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

    if(missing(objFn)) {
      objFn <- function(par, objects, simList, whModules, whParams, whParamsByMod) {
        #browser()
        whP <- 0
        for(wh in seq_along(whParamsByMod)) {
          whP <- whP + 1
          params(simList)[[names(whParamsByMod)[wh]]][[whParamsByMod[wh]]] <-
            par[whP]
        }

        out <- spades(SpaDES::copy(simList), .plotInitialTime = NA)

        outputObjects <- lapply(objects, function(objs) {
          if(is.function(objs)) {
            dat <- mget(names(formals(objs)), envir = envir(out))
            do.call(objs, dat)
          } else {
            eval(parse(text = objs[[1]])[[1]], envir = envir(out))
          }
        })

        objectiveRes <- unlist(lapply(seq_along(outputObjects), function(x) {
          if(is(outputObjects[[x]], "Raster")) {
            outObj <- getValues(outputObjects[[x]])
            dataObj <- getValues(get(names(outputObjects)[x]))
          } else {
            outObj <- outputObjects[[x]]
            dataObj <- get(names(outputObjects)[[x]])
          }

          if(objFnCompare=="MAD") {
            if(length(outObj)==1) {
              mean(abs(x - lowerRange[x])/(upperRange[x] - lowerRange[x]))
            } else {
              mean(abs(range01(outObj - dataObj)))
            }
          } else if(objFnCompare=="RMSE"){
            sqrt(mean((outObj - dataObj)^2))
          } else {
            stop("objFnCompare must be either MAD or RMSE, see help")
          }

        }))
        sum(objectiveRes)
    } }

    if(!is.null(cl)) {
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
    for(wh in seq_along(whParamsByMod)) {
      whP <- whP + 1
      modName <- names(whParamsByMod)[whP]
      par[whP] <- unlist(deps[[modName]]@parameters$default[deps[[modName]]@parameters$paramName==names(p(sim, modName)[whParamsByMod[whP]])])
      upperRange[whP] <- unlist(deps[[modName]]@parameters$max[deps[[modName]]@parameters$paramName==names(p(sim, modName)[whParamsByMod[whP]])])
      lowerRange[whP] <- unlist(deps[[modName]]@parameters$min[deps[[modName]]@parameters$paramName==names(p(sim, modName)[whParamsByMod[whP]])])
    }
    deoptimArgs <- list(fn = objFn, lower = lowerRange, upper = upperRange,
                          simList = sim, objects = objects,
                          whModules = whModules, whParams = whParams,
                          whParamsByMod = whParamsByMod)
    if(optimizer=="DEoptim") {
      if(!is.null(cl)) {
        #do.call(DEoptim.control, list(parallelType=3))
        deoptimArgs <- append(deoptimArgs,
                              list(control = DEoptim.control(parallelType=3),
                                                  cl = cl))
      }
      output <- do.call(DEoptim, deoptimArgs)
    } else {
      deoptimArgs <- append(deoptimArgs,
                            list(par = par, method = "L-BFGS-B",
                                 control = list(trace = 3, REPORT = 3)))
      if(!is.null(list(...)$hessian) | sterr)
        deoptimArgs <- append(deoptimArgs,
                              list(hessian = TRUE))

      output <- do.call(optim, deoptimArgs)
    }
    #if(!clProvided) stopCluster(cl)
    if(sterr) {
      output$sterr <- try(sqrt(abs(diag(solve(out1$hessian)))))

    }
    return(output)

  })

