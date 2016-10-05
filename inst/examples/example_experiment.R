if (interactive()) {
  library(igraph) # use %>% in a few examples
  library(raster)

  tmpdir <- file.path(tempdir(), "examples")

  # Create a default simList object for use through these examples
  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  # Example 1 - test alternative parameter values
  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)

  # Here is a list of alternative values for each parameter. They are length one
  #   numerics here -- e.g., list(0.2, 0.23) for spreadprob in fireSpread module,
  #   but they can be anything, as long as it is a list.
  experimentParams <- list(fireSpread = list(spreadprob = list(0.2, 0.23),
                                             nFires = list(20, 10)),
                           caribouMovement = list(N = list(100, 1000)))

  sims <- experiment(mySim, params = experimentParams)

  # see experiment:
  attr(sims, "experiment")

  # Read in outputs from sims object
  FireMaps <- do.call(stack, lapply(1:NROW(attr(sims, "experiment")$expDesign),
                                    function(x) sims[[x]]$landscape$Fires))
  if (interactive()) Plot(FireMaps, new = TRUE)

  # Or reload objects from files, useful if sim objects too large to store in RAM
  caribouMaps <- lapply(sims, function(sim) {
    caribou <- readRDS(outputs(sim)$file[outputs(sim)$objectName == "caribou"])
  })
  names(caribouMaps) <- paste0("caribou", 1:8)
  # Plot whole named list
  if (interactive()) Plot(caribouMaps, size = 0.1)

  # Example 2 - test alternative modules
  # Example of changing modules, i.e., caribou with and without fires
  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)
  experimentModules <- list(
    c("randomLandscapes", "fireSpread", "caribouMovement"),
    c("randomLandscapes", "caribouMovement"))
  sims <- experiment(mySim, modules = experimentModules)
  attr(sims, "experiment")$expVals # shows 2 alternative experiment levels

  # Example 3 - test alternative parameter values and modules
  # Note, this isn't fully factorial because all parameters are not
  #   defined inside smaller module list
  sims <- experiment(mySim, modules = experimentModules, params = experimentParams)
  attr(sims, "experiment")$expVals # shows 10 alternative experiment levels

  # Example 4 - manipulate manipulate directory names -
  #  "simNum" is special value for dirPrefix, it is converted to 1, 2, ...
  sims <- experiment(mySim, params = experimentParams, dirPrefix = c("expt", "simNum"))
  attr(sims, "experiment")$expVals # shows 8 alternative experiment levels, 24 unique
                                   #   parameter values

  # Example 5 - doing replicate runs -
  sims <- experiment(mySim, replicates = 2)
  attr(sims, "experiment")$expDesign # shows 2 replicates of same experiment

  # Example 6 - doing replicate runs, but within a sub-directory
  sims <- experiment(mySim, replicates = 2, dirPrefix = c("expt"))
  lapply(sims, outputPath) # shows 2 replicates of same experiment, within a sub directory

  # Example 7 - doing replicate runs, of a complex, non factorial experiment.
  # Here we do replication, parameter variation, and module variation all together.
  # This creates 20 combinations.
  # The experiment function tries to make fully factorial, but won't
  # if all the levels don't make sense. Here, changing parameter values
  # in the fireSpread module won't affect the simulation when the fireSpread
  # module is not loaded:
  # library(raster)
  # beginCluster(20) # if you have multiple clusters available, use them here to save time
  sims <- experiment(mySim, replicates = 2, params = experimentParams,
                     modules = experimentModules,
                     dirPrefix = c("expt", "simNum"))
  # endCluster() # end the clusters
  attr(sims, "experiment")

  # Example 8 - Use replication to build a probability map.
  # For this to be meaningful, we need to provide a fixed input landscape,
  #   not a randomLandscape for each experiment level. So requires 2 steps.
  # Step 1 - run randomLandscapes module twice to get 2 randomly
  #  generated landscape maps. We will use 1 right away, and we will
  #  use the two further below
  mySimRL <- simInit(
    times = list(start = 0.0, end = 0.1, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape"),
      # Turn off interactive plotting
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = file.path(tmpdir, "landscapeMaps1")),
    outputs = data.frame(objectName = "landscape", saveTime = 0, stringsAsFactors = FALSE)
  )
  # Run it twice to get two copies of the randomly generated landscape
  mySimRLOut <- experiment(mySimRL, replicate = 2)

  #extract one of the random landscapes, which will be passed into next as an object
  landscape <- mySimRLOut[[1]]$landscape

  # here we don't run the randomLandscapes module; instead we pass in a landscape
  #  as an object, i.e., a fixed input
  mySimNoRL <- simInit(
    times = list(start = 0.0, end = 1, timeunit = "year"), # only 1 year to save time
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA)
    ),
    modules = list("fireSpread", "caribouMovement"), # No randomLandscapes modules
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir),
    objects = c("landscape"), # Pass in the object here
    # Save final state (the default if saveTime is not specified) of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  # Put outputs into a specific folder to keep them easy to find
  outputPath(mySimNoRL) <- file.path(tmpdir, "example8")
  sims <- experiment(mySimNoRL, replicates = 8) # Run experiment
  attr(sims, "experiment") # shows the experiment, which in this case is just replicates

  # list all files that were saved called 'landscape'
  landscapeFiles <- dir(outputPath(mySimNoRL), recursive = TRUE, pattern = "landscape",
                        full.names = TRUE)

  # Can read in Fires layers from disk since they were saved, or from the sims
  #  object
  # Fires <- lapply(sims, function(x) x$landscape$Fires) %>% stack
  Fires <- lapply(landscapeFiles, function(x) readRDS(x)$Fires) %>% stack()
  Fires[Fires > 0] <- 1 # convert to 1s and 0s
  fireProb <- sum(Fires)/nlayers(Fires) # sum them and convert to probability
  if (interactive()) Plot(fireProb, new = TRUE)

  # Example 9 - Pass in inputs, i.e., input data objects taken from disk
  #  Here, we, again, don't provide randomLandscapes module, so we need to
  #  provide an input stack called lanscape. We point to the 2 that we have
  #  saved to disk in Example 8
  mySimInputs <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA)
    ),
    modules = list("fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                 outputPath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )
  landscapeFiles <- dir(tmpdir, pattern = "landscape_year0", recursive = TRUE, full.names = TRUE)

  # Varying inputs files - This could be combined with params, modules, replicates also
  outputPath(mySimInputs) <- file.path(tmpdir, "example9")
  sims <- experiment(mySimInputs,
                     inputs = lapply(landscapeFiles,function(filenames) {
                       data.frame(file = filenames, loadTime = 0,
                                  objectName = "landscape",
                                  stringsAsFactors = FALSE) })
   )

  # load in experimental design object
  experiment <- load(file = file.path(tmpdir, "example9", "experiment.RData")) %>% get()
  print(experiment) # shows input files and details

  # Example 10 - Use a very simple output dir name using substrLength = 0,
  #   i.e., just the simNum is used for outputPath of each spades call
  outputPath(mySim) <- file.path(tmpdir, "example10")
  sims <- experiment(mySim, modules = experimentModules, replicates = 2,
                     substrLength = 0)
  lapply(sims, outputPath) # shows that the path is just the simNum
  experiment <- load(file = file.path(tmpdir, "example10", "experiment.RData")) %>% get()
  print(experiment) # shows input files and details

  # Example 11 - use clearSimEnv = TRUE to remove objects from simList
  # This will shrink size of return object, which may be useful because the
  #  return from experiment function may be a large object (it is a list of
  # simLists). To see size of a simList, you have to look at the objects
  #  contained in the  envir(simList).  These can be obtained via objs(sim)
  sapply(sims, function(x) object.size(objs(x))) %>% sum + object.size(sims)
  # around 3 MB
  # rerun with clearSimEnv = TRUE
  sims <- experiment(mySim, modules = experimentModules, replicates = 2,
                     substrLength = 0, clearSimEnv = TRUE)
  sapply(sims, function(x) object.size(objs(x))) %>% sum + object.size(sims)
  # around 250 kB, i.e., all the simList contents except the objects.

  # Example 12 - pass in objects
  experimentObj <- list(landscape = lapply(landscapeFiles, readRDS) %>%
                                    setNames(paste0("landscape",1:2)))
  # Pass in this list of landscape objects
  sims <- experiment(mySimNoRL, objects = experimentObj)

  # Remove all temp files
  unlink(tmpdir, recursive = TRUE)
}
