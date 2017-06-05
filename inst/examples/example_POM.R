if (interactive()) {
  set.seed(89462)
  library(parallel)
  library(raster)
  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      fireSpread = list(nFires = 5),
      randomLandscapes = list(nx = 300, ny = 300)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
  )

  # Since this is a made up example, we don't have real data
  #  to run POM against. Instead, we will run the model once,
  #  take the values at the end of the simulation as if they
  #  are real data, then rerun the POM function next,
  #  comparing these "data" with the simulated values
  #  using Mean Absolute Deviation
  outData <- spades(Copy(mySim), .plotInitialTime = NA)

  # Extract the "true" data, in this case, the "proportion of cells burned"
  # Function defined that will use landscape$Fires map from simList,
  #  i.e., sim$landscape$Fires
  #  the return value being compared via MAD with propCellBurnedData
  propCellBurnedFn <- function(landscape) {
    sum(getValues(landscape$Fires) > 0) / ncell(landscape$Fires)
  }
  # visualize the burned maps of true "data"
  propCellBurnedData <- propCellBurnedFn(outData$landscape)
  clearPlot()
  if (interactive()) {
    Fires <- outData$landscape$Fires # Plot doesn't do well with many nested layers
    Plot(Fires)
  }

  # Example 1 - 1 parameter
  # In words, this says, "find the best value of spreadprob such that
  #  the proportion of the area burned in the simulation
  #  is as close as possible to the proportion area burned in
  #  the "data", using \code{DEoptim()}.

  # Can use cluster if computer is multi-threaded (but not yet via cl arg, which is not
  #                                                implemented yet in DEoptim)
  # This example uses parallelType = 1 in DEoptim. For this, you must manually
  #  pass all packages and variables as character strings.
  # cl <- makeCluster(detectCores() - 1) # not implemented yet in DEoptim
  out1 <- POM(mySim, "spreadprob",
              list(propCellBurnedData = propCellBurnedFn), # data = pattern pair
              #optimControl = list(parallelType = 1),
              logObjFnVals = TRUE)

  ## Once cl arg is available from DEoptim, this will work:
  # out1 <- POM(mySim, "spreadprob", cl = cl,
  #            list(propCellBurnedData = propCellBurnedFn)) # data = pattern pair

  # Example 2 - 2 parameters
  # Function defined that will use caribou from sim$caribou, with
  #  the return value being compared via MAD with NPattern
  #  module, parameter N, is from 10 to 1000)
  caribouFn <- function(caribou) length(caribou)

  # Extract "data" from simList object (normally, this would be actual data)
  NPattern <- caribouFn(outData$caribou)

  aTime <- Sys.time()
  parsToVary <- c("spreadprob", "N")
  out2 <- POM(mySim, parsToVary,
              list(propCellBurnedData = propCellBurnedFn,
                   NPattern = caribouFn), logObjFnVals = TRUE)
                   #optimControl = list(parallelType = 1))
                   #cl = cl) # not yet implemented, waiting for DEoptim
  bTime <- Sys.time()
  # check that population overlaps known values (0.225 and 100)
  apply(out2$member$pop, 2, quantile, c(0.025, 0.975))
  hists <- apply(out2$member$pop, 2, hist, plot = FALSE)
  clearPlot()
  for (i in seq_along(hists)) Plot(hists[[i]], addTo = parsToVary[i],
                                   title = parsToVary[i], axes = TRUE)

  print(paste("DEoptim", format(bTime - aTime)))
  #stopCluster(cl) # not yet implemented, waiting for DEoptim

  # Example 3 - using objFn instead of objects

  # list all the parameters in the simList, from these, we select to vary
  params(mySim)

  # Objective Function Example:
  #   objective function must have several elements
  #   - first argument must be parameter vector, passed to and used by DEoptim
  #   - likely needs to take sim object, likely needs a copy
  #      because of pass-by-reference semantics of sim objects
  #   - pass data that will be used internally for objective function
  objFnEx <- function(pars, # param values
                      sim, # simList object
                      NPattern, propCellBurnedData, caribouFn, propCellBurnedFn) {
    ### data

    # make a copy of simList because it will possibly be altered by spades call
    sim1 <- Copy(sim)

    # take the parameters and assign them to simList
    params(sim1)$fireSpread$spreadprob <- pars[1]
    params(sim1)$caribouMovement$N <- pars[2]

    # run spades, without plotting
    out <- spades(sim1, .plotInitialTime = NA)

    # calculate outputs
    propCellBurnedOut <- propCellBurnedFn(out$landscape)
    NPattern_Out <- caribouFn(out$caribou)

    minimizeFn <- abs(NPattern_Out - NPattern) +
                  abs(propCellBurnedOut - propCellBurnedData)

    # have more info reported to console, if desired
    # cat(minimizeFn)
    # cat(" ")
    # cat(pars)
    # cat("\n")

    return(minimizeFn)
  }

  # Run DEoptim with custom objFn, identifying 2 parameters to allow
  #  to vary, and pass all necessary objects required for the
  #  objFn

  # choose 2 of them to vary. Need to identify them in params & inside objFn
  # Change optimization parameters to alter how convergence is achieved
  out5 <- POM(mySim, params = c("spreadprob", "N"),
              objFn = objFnEx,
              NPattern = NPattern,
              propCellBurnedData = propCellBurnedData,
              caribouFn = caribouFn,
              propCellBurnedFn = propCellBurnedFn,
              #cl = cl, # uncomment for cluster # not yet implemented, waiting for DEoptim
              # see ?DEoptim.control for explanation of these options
              optimControl = list(
                NP = 100, # run 100 populations, allowing quantiles to be calculated
                initialpop = matrix(c(runif(100, 0.2, 0.24), runif(100, 80, 120)), ncol = 2),
                parallelType = 1
              )
            )

  # Can also use an optimizer directly -- miss automatic parameter bounds,
  #  and automatic objective function using option 2
  library(DEoptim)
  out7 <- DEoptim(fn = objFnEx,
                 sim = mySim,
                 NPattern = NPattern,
                 propCellBurnedData = propCellBurnedData,
                 caribouFn = caribouFn,
                 propCellBurnedFn = propCellBurnedFn,
                 # cl = cl, # uncomment for cluster
                 # see ?DEoptim.control for explanation of these options
                 control = DEoptim.control(steptol = 3, # parallelType = 3,
                                           parallelType = 1,
                                           packages = list("raster", "SpaDES", "RColorBrewer"),
                                           parVar = list("objFnEx")),
                 initialpop = matrix(c(runif(40, 0.2, 0.24), runif(40, 80, 120)), ncol = 2),
                 lower = c(0.2, 80), upper = c(0.24, 120))
}
