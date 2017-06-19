test_that("experiment does not work correctly", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(igraph); on.exit(detach("package:igraph"), add = TRUE)
  library(dplyr); on.exit(detach("package:dplyr"), add = TRUE)
  library(reproducible);

  tmpdir <- file.path(tempdir(), "testParallel") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # Example of changing parameter values
  mySimFull <- simInit(
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
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)
  caribouNums <- c(100, 1000)
  experimentParams <- list(
    fireSpread = list(spreadprob = c(0.2), nFires = c(20, 10)),
    caribouMovement = list(N = caribouNums)
  )

  sims <- experiment(mySimFull, params = experimentParams)
  expt <- load(file.path(tmpdir, "experiment.RData")) %>% get() # Loads an object named experiment
  exptDesign <- expt$expDesign
  exptVals <- expt$expVals

  expect_equal(NROW(exptDesign), 4)
  expect_equal(exptVals[exptVals$module == "caribouMovement", "val"] %>% unlist(),
               c(rep(caribouNums, 2)))
  expect_equal(exptVals$modules %>% unique(),
               "randomLandscapes,caribouMovement,fireSpread")
  expect_equal(NROW(attr(sims, "experiment")$expDesign), NROW(exptDesign))

  # test that experimental design object is indeed what is in the sims object
  mods <- sapply(strsplit(names(exptDesign)[-(4:5)], split = "\\."), function(x) x[[1]])
  params <- sapply(strsplit(names(exptDesign)[-(4:5)], split = "\\."), function(x) x[[2]])
  out2 <- lapply(seq_along(mods), function(y) {
    out <- lapply(seq_len(NROW(exptDesign)), function(x) {
      expect_equivalent(0, params(sims[[x]])[[mods[y]]][[params[[y]]]] -
                          exptVals %>% dplyr::filter(module == mods[[y]] &
                                                       param == params[[y]] &
                                                       expLevel == x) %>%
                          dplyr::select(val) %>% unlist())
    })
  })

  sims <- experiment(mySimFull, replicates = 3)
  expt <- load(file.path(tmpdir, "experiment.RData")) %>% get() # Loads an object named experiment
  exptDesign <- expt$expDesign
  exptVals <- expt$expVals
  out <- lapply(seq_along(sims), function(x) {
    expect_equal(outputs(sims[[x]])$saved, c(TRUE, TRUE))
    expect_equal(
      outputs(sims[[x]])$file,
      file.path(tmpdir, paste0("rep", x),
                paste0(c("landscape", "caribou"), "_year2.rds")) %>%
        normPath()
    )
  })

  ### Test inputs - first, have to make the input map
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
  sims2 <- experiment(mySimRL, replicate = 2)

  mySimNoRL <- simInit(
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
  landscapeFiles <- dir(outputPath(mySimRL), pattern = "landscape_year0", recursive = TRUE,
                        full.names = TRUE)
  set.seed(1232)
  sims <- experiment(mySimNoRL, replicates = 2,
                     inputs = lapply(landscapeFiles, function(filenames) {
                       data.frame(file = filenames, loadTime = 0,
                                  objectName = "landscape", stringsAsFactors = FALSE)
                     })
  )

  # Make sure these are using the same, identical input maps
  expect_true(identical(sims[[1]]$landscape$habitatQuality, sims[[3]]$landscape$habitatQuality))
  expect_true(identical(sims[[2]]$landscape$habitatQuality, sims[[4]]$landscape$habitatQuality))
  # Make sure there are two different input maps (i.e,. the inverse of the above test)
  expect_false(identical(sims[[2]]$landscape$habitatQuality, sims[[3]]$landscape$habitatQuality))

  # Make sure random number generator is working. These start with the same maps, but should end up different
  expect_false(identical(sims[[2]]$landscape$Fires, sims[[4]]$landscape$Fires))
  expect_false(identical(sims[[1]]$landscape$Fires, sims[[3]]$landscape$Fires))

  # Test clearSimEnv argument... i.e., clearing of the final objects
  expect_equal(length(ls(sims[[1]])), 10)
  set.seed(1232)
  sims2 <- experiment(mySimNoRL, replicates = 2, clearSimEnv = TRUE,
                      inputs = lapply(landscapeFiles, function(filenames) {
                        data.frame(file = filenames, loadTime = 0,
                                   objectName = "landscape", stringsAsFactors = FALSE)
                      })
  )
  # This version has no objects
  expect_equal(length(ls(sims2[[1]])), 0)

  # Test that the only difference is their objects, which we can pass back in manually
  list2env(mget(ls(sims[[1]]), envir = envir(sims[[1]])), envir = envir(sims2[[1]]))
  expect_equal(sims[[1]], sims2[[1]])

  # Test object passing in
  experimentObj <- list(landscape = lapply(landscapeFiles, readRDS) %>%
                          setNames(paste0("landscape", 1:2)))
  # Pass in this list of landscape objects
  set.seed(1232)
  sims3 <- experiment(mySimNoRL, objects = experimentObj)
  # Compare simulations that had objects read from disk with objects passed via objects arg
  expect_equal(sims3[[1]]$landscape, sims[[1]]$landscape)
  expect_equal(sims3[[2]]$landscape, sims[[2]]$landscape)
})

test_that("parallel does not work with experiment function", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  skip("Can't automatically test parallel processing - Run Manually")

  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(magrittr); on.exit(detach("package:magrittr"), add = TRUE)
  library(dplyr); on.exit(detach("package:dplyr"), add = TRUE)

  tmpdir <- file.path(tempdir(), "testParallel") %>% checkPath(create = TRUE)

  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # Example of changing parameter values
  mySimFull <- simInit(
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
                 cachePath = file.path(tmpdir, "cache"),
                 inputPath = tmpdir,
                 outputPath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)
  caribouNums <- c(100, 1000)
  experimentParams <- list(
    fireSpread = list(spreadprob = c(0.2), nFires = c(20, 10)),
    caribouMovement = list(N = caribouNums)
  )

  set.seed(2343)
  seqTime <- system.time(simsSeq <- experiment(mySimFull, params = experimentParams))

  if (interactive()) {
    n <- pmin(parallel::detectCores(), 4) # use up to 4 cores
    beginCluster(n)
    set.seed(2343)
    parTime <- system.time(simsPar <- experiment(mySimFull, params = experimentParams))
    endCluster()
    expect_equal(attr(simsPar, "experiment"), attr(simsSeq, "experiment"))
    expect_gt(as.numeric(seqTime)[3], as.numeric(parTime)[3])
  }
})
