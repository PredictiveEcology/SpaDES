################################################################################
#' Use Pattern Oriented Modeling to fit unknown parameters
#'
#' This is very much in alpha condition. It has been tested on simple problems,
#' as shown in the examples, with up to 2 parameters.
#' It appears that \code{DEoptim} is the superior package for the stochastic problems.
#' This should be used with caution as with all optimization routines. This function
#' can nevertheless take \code{optim} or \code{genoud} as optimizers, using
#' \code{stats::optim} or \code{rgenoud::genoud}, respectively.
#' However, these latter approaches do not seem appropriate for stochastic problems,
#' and have not been widely tested and are not supported within \code{POM}.
#'
#' There are two ways to use this function, via 1) \code{objFn} or 2) \code{objects}.
#'
#' \enumerate{
#'   \item The user can pass the entire objective function to the \code{objFn}
#'   argument that will be passed directly to the \code{optimizer}.
#'   For this, the user will likely need to pass named objects as part of the \code{...}.
#'
#'   \item The slightly simpler approach is to pass a list of 'actual data--simulated data'
#'   pairs as a named list in \code{objects} and specify how these objects should be
#'   compared via \code{objFnCompare} (whose default is Mean Absolute Deviation or "MAD").
#' }
#'
#' Option 1 offers more control to the user, but may require more knowledge.
#' Option 1 should likely contain a call to \code{simInit(Copy(simList))} and
#' \code{spades} internally.
#' See examples that show simple examples of each type, option 1 and option 2.
#' In both cases, \code{params} is required to indicate which parameters can be
#' varied in order to achieve the fit.
#'
#' Currently, option 1 only exists when optimizer is \code{"DEoptim"}, the default.
#'
#' The upper and lower limits for parameter values are taken from the
#' metadata in the module. Thus, if the module metadata does not define the
#' upper and lower limits, or these are very wide, then the optimization
#' may have troubles. Currently, there is no way to override these upper
#' and lower limits; the module metadata should be changed if there needs
#' to be different parameter limits for optimization.
#'
#' \code{objects} is a named list of data--pattern pairs.
#' Each of these pairs will be assessed against one another using
#' the \code{objFnCompare}, after standardizing each independently. The
#' standardization, which only occurs if the abs(data value < 1),
#' is: \code{mean(abs(derived value - data value))/mean(data value)}. If
#' the data value is between -1 and 1, then there is no standardization.
#' If there is more than one data--pattern
#' pair, then they will simply be added together in the objective
#' function. This gives equal weight to each pair. If the user wishes to
#' put different weight on each pattern, a \code{weights} vector can be
#' provided. This will be used to multiply the standardized values described above.
#' Alternatively, the user
#' may wish to weight them differently, in which case, their relative
#' scales can be adjusted.
#'
#' There are many options that can be passed to \code{\link[DEoptim]{DEoptim}},
#' (the details of which are in the help), using \code{optimControl}. The defaults
#' sent from \code{POM} to \code{DEoptim} are: steptol = 3 (meaning it will start
#' assessing convergence after 3 iterations (WHICH MAY NOT BE SUFFICIENT FOR YOUR PROBLEM),
#' \code{NP = 10 * length(params)}
#' (meaning the population size is 10 x the number of parameters) and itermax =
#' 200 (meaning it won't go past 200 iterations). These and others may need to be adjusted to
#' obtain good values.
#' NOTE: \code{DEoptim} does not provide a direct estimate of confidence intervals.
#' Also, convergence may be unreliable, and may occur because \code{itermax} is reached.
#' Even when convergence is indicated, the estimates are not guaranteed to be global
#' optima. This is different than other optimizers that will normally indicate
#' if convergence was not achieved at termination of the optimization.
#'
#' Using this function with a parallel cluster currently requires that you pass
#' \code{optimControl = list(parallelType = 1)}, and possibly package and variable names
#'  (and does not yet accept the \code{cl} argument). See examples.
#' This setting will use all available threads on your computer.
#' Future versions of this will allow passing of a custom cluster object via \code{cl} argument.
#' \code{POM} will automatically determine packages to load in the spawned cluster
#' (via \code{SpaDES::packages}) and it will load all objects in the cluster that are
#' necessary, by sending \code{names(objects)} to \code{parVar} in \code{DEoptim.control}.
#'
#' Setting \code{logObjFnVals} to \code{TRUE} may help diagnosing some problems.
#' Using the POM derived objective function, essentially all patterns are treated equally.
#' This may not give the correct behavior for the objective function.
#' Because \code{POM} weighs the patterns equally, it may be useful to use the
#' log files to examine the behaviour of the pattern--data pairs.
#' The first file, ObjectiveFnValues.txt, shows the result of each of the
#' (possibly logged), pattern--data deviations, standardized, and weighted.
#' The second file, \file{ObjectiveFnValues_RawPatterns.txt}, shows the actual
#' value of the pattern (unstandardized, unweighted, unlogged).
#' If \code{weights} is passed, then these weighted values will be reflected
#' in the \file{ObjectiveFnValues.txt} file.
#'
#' @inheritParams spades
#' @inheritParams splitRaster
#'
#' @param params Character vector of parameter names that can be changed by the optimizer. These
#'               must be accessible with \code{params(sim)} internally.
#'
#' @param objects A optional named list (must be specified if objFn is not).
#'                The names of each list element must correspond to an object in the
#'                \code{.GlobalEnv} and the list elements must be objects or
#'                functions of objects that can be accessed in
#'                the ls(sim) internally. These will be used to create the
#'                objective function passed to the optimizer. See details and examples.
#'
#' @param objFn An optional objective function to be passed into \code{optimizer}.
#'              If missing, then \code{POM} will use \code{objFnCompare} and
#'              \code{objects} instead. If using \code{POM} with a SpaDES
#'              simulation, this objFn must contain a spades call internally,
#'              followed by a derivation of a value that can be minimized
#'              but the \code{optimizer}. It must have, as first argument, the
#'              values for the parameters. See example.
#'
#' @param optimizer The function to use to optimize. Default is \code{"DEoptim"}.
#'                  Currently it can also be \code{"optim"} or \code{"rgenoud"},
#'                  which use \code{stats::optim} or \code{rgenoud::genoud}, respectively.
#'                  The latter two do not seem optimal for stochastic problems and have
#'                  not been widely tested.
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
#' @param NaNRetries Numeric. If greater than 1, then the function will retry
#'                   the objective function for a total of that number of times
#'                   if it results in an \code{NaN}. In general
#'                   this should not be used as the objective function should be
#'                   made so that it doesn't produce \code{NaN}. But, sometimes
#'                   it is difficult to diagnose stochastic results.
#'
#' @param logObjFnVals Logical or Character string indicating a filename. Ignored if
#'                       \code{objFn} is supplied.
#'                       If TRUE (and there is no \code{objFn} supplied), then the
#'                       value of the individual patterns will be output the console
#'                       if being run interactively or to a tab delimited
#'                       text file named \code{ObjectiveFnValues.txt} (or that passed by
#'                       the user here) at each evaluation of the
#'                       POM created objective function. See details.
#'
#' @param weights Numeric. If provided, this vector will be multiplied by the standardized
#'                deviations (possibly MAD or RMSE) as described in \code{objects}. This has
#'                the effect of weighing
#'                each standardized deviation (pattern--data pair) to a user
#'                specified amount in the objective function.
#'
#' @param useLog Logical. Should the data patterns and output patterns be logged (\code{log})
#'               before calculating the \code{objFnCompare}. i.e.,
#'               \code{mean(abs(log(output) - log(data)))}.
#'               This should be length 1 or length \code{objects}.
#'               It will be recycled if length >1, less than \code{objects}.
#'
#' @return A list with at least 2 elements. The first (or first several) will
#' be the returned object from the optimizer. The second (or last if there are
#' more than 2), named \code{args} is the set of arguments that were passed
#' into the control of the optimizer.
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
#' @importFrom parallel clusterEvalQ clusterExport
#' @export
#' @docType methods
#' @rdname POM
#'
#' @author Eliot McIntire
#'
#' @example inst/examples/example_POM.R
#'
setGeneric(
  "POM",
  function(sim, params, objects = NULL, objFn, cl, optimizer = "DEoptim",
           sterr = FALSE, ..., objFnCompare = "MAD", optimControl = NULL,
           NaNRetries = NA, logObjFnVals = FALSE, weights, useLog = FALSE) {
    standardGeneric("POM")
  })

#' @rdname POM
setMethod(
  "POM",
  signature(sim = "simList", params = "character", objects = "ANY", objFn = "ANY"),
  definition = function(sim, params, objects, objFn, cl, optimizer,
                        sterr, ..., objFnCompare, optimControl,
                        NaNRetries, logObjFnVals, weights, useLog) {

    if (missing(cl)) {
      cl <- tryCatch(getCluster(), error = function(x) NULL)
      on.exit(if (!is.null(cl)) returnCluster(), add = TRUE)
      clProvided <- FALSE
    } else {
      clProvided <- TRUE
    }
    if (!is.null(list(...)$weight)) message("Did you mean to pass 'weight' instead of 'weights'?")

    on.exit(while (sink.number() > 0) sink(), add = TRUE)

    if (missing(weights)) weights <- rep(1, length(objects))
    if (!missing(objects))
      if (length(useLog) < length(objects)) useLog <- rep_len(useLog, length(objects))

    if (is.na(NaNRetries)) NaNRetries <- 1
    paramNames <- lapply(SpaDES::params(sim), names)
    whParams <- lapply(paramNames, function(pn) match(params, pn))
    whModules <- unlist(lapply(whParams, function(mod) any(!is.na(mod))))

    whParamsByMod <- unlist(lapply(whParams, na.omit))
    names(whParamsByMod) <- unlist(lapply(names(whModules), function(nam) {
      rep(nam, sum(grepl(pattern = nam, names(whParamsByMod))))
    }))

    if (missing(objects)) {
      objects <- NULL
    }

    range01 <- function(x, ...) {
      (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    }

    if (missing(objFn)) {
      objFn1 <- function(par, objects, sim, whModules, whParams,
                        whParamsByMod, parallelType, weights, useLog) {
        keepGoing <- TRUE
        tryNum <- 1
        while (keepGoing) {
          sim_ <- Copy(sim)
          whP <- 0
          for (wh in seq_along(whParamsByMod)) {
            whP <- whP + 1
            params(sim_)[[names(whParamsByMod)[wh]]][[whParamsByMod[wh]]] <- par[whP]
          }

          out <- spades(sim_, .plotInitialTime = NA)

          outputObjects <- lapply(objects, function(objs) {
            if (is.function(objs)) {
              dat <- mget(names(formals(objs)), envir = envir(out))
              do.call(objs, dat)
            } else {
              eval(parse(text = objs[[1]])[[1]], envir = envir(out))
            }
          })

          envPOMCalled <- sys.frame(min(grep("POM", sys.calls()))-1)

          objectiveRes <- lapply(seq_along(outputObjects), function(x) {
            if (is(outputObjects[[x]], "Raster")) {
              outObj <- getValues(outputObjects[[x]])
              dataObj <- getValues(get(names(outputObjects)[x]),
                                   envir = envPOMCalled)
            } else {
              outObj <- outputObjects[[x]]
              dataObj <- get(names(outputObjects)[[x]],
                             envir = envPOMCalled)
            }

            if (useLog[x]) {
              if ((outObj <= 0) | (dataObj <= 0)) {
                useLog[x] <- FALSE
                warning(paste0(names(outputObjects)[x],
                               " or its pattern is zero or negative; not using log"))
              }

            }
            if (objFnCompare == "MAD") {
              if (useLog[x]) {
                out <- mean(abs(log(outObj) - log(dataObj)), na.rm = TRUE)
              } else {
                out <- mean(abs(outObj - dataObj), na.rm = TRUE)
              }
            } else if (objFnCompare == "RMSE") {
              if (useLog[x]) {
                out <- sqrt(mean((log(outObj) - log(dataObj)) ^ 2))
              } else {
                out <- sqrt(mean((outObj - dataObj) ^ 2))
              }
            } else {
              stop("objFnCompare must be either MAD or RMSE, see help")
            }
            if (useLog[x]) {
              dataObjVal <- mean(abs(log(dataObj)), na.rm = TRUE)
            } else{
              dataObjVal <- mean(abs(dataObj), na.rm = TRUE)
            }
            if (abs(dataObjVal) < 1) dataObjVal <- 1
            outStandard <- out / dataObjVal
            out <- list(raw = out, standardized = outStandard, value = outObj)
            return(out)

          })
          objectiveResStd <- unlist(lapply(objectiveRes, function(x) x[["standardized"]]))
          objectiveResW <- objectiveResStd * weights
          sumObj <- sum(objectiveResW)
          if (is.nan(sumObj)) {
            if (tryNum < NaNRetries) keepGoing <- TRUE else keepGoing <- FALSE
            tryNum <- tryNum + 1
          } else {
            keepGoing <- FALSE
          }
        }
        if (!(identical(logObjFnVals, FALSE))) {
          if (deoptimArgs$control$parallelType > 0 | (logObjFnVals != "objectiveFnValues.txt"))
            sink(file = logObjFnVals, append = TRUE)
          cat(format(objectiveResW, digits = 4), sep = "\t")
          cat("\n")
          if (deoptimArgs$control$parallelType > 0  | (logObjFnVals != "objectiveFnValues.txt"))
            sink()
          if (deoptimArgs$control$parallelType > 0 | (logObjFnVals != "objectiveFnValues.txt"))
            sink(file = paste0(gsub(logObjFnVals, pattern = "[.]txt", replacement = ""),
                               "_RawPattern.txt"), append = TRUE)
          cat(format(unlist(lapply(objectiveRes, function(x) x[["value"]])), digits = 4), dep = "\t")
          cat("\n")
          if (deoptimArgs$control$parallelType > 0  | (logObjFnVals != "objectiveFnValues.txt"))
            sink()

        }
        return(sumObj)
      }
      objFn <- function(...) {
        keepGoing1 <- TRUE
        tryNum1 <- 1
        while (keepGoing1) {
          outTry <- try(objFn1(...))
          if (!is(outTry, "try-error")) {
            keepGoing1 <- FALSE
          } else {
            warning("objective function returned error on try #", tryNum1, "Consider changing.")
            if (tryNum1 < NaNRetries) keepGoing1 <- TRUE else keepGoing1 <- FALSE
            tryNum1 <- tryNum1 + 1
          }
        }
        return(outTry)

      }
      userSuppliedObjFn <- FALSE
    } else {
      userSuppliedObjFn <- TRUE
      dots <- list(...)
    }

    deps <- depends(sim)@dependencies
    whP <- 0
    par <- numeric(length(whParamsByMod))
    lowerRange <- numeric(length(whParamsByMod))
    upperRange <- numeric(length(whParamsByMod))
    for (wh in seq_along(whParamsByMod)) {
      whP <- whP + 1
      modName <- names(whParamsByMod)[whP]
      par[whP] <- unlist(deps[[modName]]@parameters$default[deps[[modName]]@parameters$paramName == names(P(sim, modName)[whParamsByMod[whP]])])
      upperRange[whP] <- unlist(deps[[modName]]@parameters$max[deps[[modName]]@parameters$paramName == names(P(sim, modName)[whParamsByMod[whP]])])
      lowerRange[whP] <- unlist(deps[[modName]]@parameters$min[deps[[modName]]@parameters$paramName == names(P(sim, modName)[whParamsByMod[whP]])])
    }
    deoptimArgs <- list(fn = objFn, lower = lowerRange, upper = upperRange,
                        sim = sim, objects = objects,
                        whModules = whModules, whParams = whParams,
                        whParamsByMod = whParamsByMod, weights = weights,
                        useLog = useLog)
    if (optimizer == "DEoptim") {
      deoptimArgs$control <- DEoptim.control()
      if (!is.null(cl)) {
        message(paste("cl argument not yet implemented in DEoptim and likely won't work. ",
                      "Until DEoptim package has parallelType = 3 implemented, use ",
                      "parallelType = 1. See examples."))
        deoptimArgs$control$parallelType <- 3
        deoptimArgs$cl <- cl
      }
      deoptimArgs$control$NP <- 10 * length(lowerRange)
      deoptimArgs$control$steptol <- 3
      if (!is.null(optimControl)) {
        deoptimArgs$control[names(optimControl)] <- optimControl
      }

      if (userSuppliedObjFn) {
        dots <- list(...)
        de1 <- deoptimArgs[na.omit(match(names(formals(DEoptim)), names(deoptimArgs)))]
        de2 <- dots[na.omit(match(names(formals(objFn)), names(dots)))]
        deoptimArgs <- list()
        deoptimArgs[names(de1)] <- de1
        deoptimArgs[names(de2)] <- de2
        deoptimArgs$sim <- sim
      }

      if (!is.null(cl)) {
        if (userSuppliedObjFn) {
          clusterExport(cl, c("sim", names(dots)), envir = sys.frame(1))
        } else {
          clusterExport(cl, c("sim", names(objects)), envir = sys.frame(-1))
        }
        clusterEvalQ(cl, {
          lapply(SpaDES::packages(sim), library, character.only = TRUE)
        })
      } else if (deoptimArgs$control$parallelType == 1) {
        deoptimArgs$control$parVar <- as.list(names(objects))
        deoptimArgs$control$packages <- SpaDES::packages(sim)
      }

      #deoptimArgs$control$parallelType <- deoptimArgs$control$parallelType

      deoptimArgs$control <- do.call(DEoptim.control, deoptimArgs$control)

      if (!(identical(logObjFnVals, FALSE))) {
        if (isTRUE(logObjFnVals)) logObjFnVals <- "objectiveFnValues.txt"

        if (deoptimArgs$control$parallelType > 0 | (logObjFnVals != "objectiveFnValues.txt"))
          sink(file = logObjFnVals, append = FALSE)
        cat(names(objects), sep = "\t")
        cat("\n")
        if (deoptimArgs$control$parallelType > 0)
          sink()
        if (deoptimArgs$control$parallelType > 0 | (logObjFnVals != "objectiveFnValues.txt"))
          sink(file = paste0(gsub(logObjFnVals, pattern = "[.]txt", replacement = ""),
                             "_RawPattern.txt"), append = FALSE)

        cat(names(objects), sep = "\t")
        cat("\n")
        if (deoptimArgs$control$parallelType > 0 | (logObjFnVals != "objectiveFnValues.txt"))
          sink()
      }

      print(params)
      output <- do.call("DEoptim", deoptimArgs)

    } else {
      if (!is.null(list(...)$hessian) | sterr)
        deoptimArgs <- append(deoptimArgs,
                              list(hessian = TRUE))

      if (optimizer == "genoud") {
        if (!is.null(cl)) {
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
          stop("rgenoud package is not installed. Please install using:\n",
               "  install.packages(\"rgenoud\")")
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
