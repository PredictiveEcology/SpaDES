usesSpaDESVersion <- "1.1.0"
if (packageVersion("SpaDES") < usesSpaDESVersion) {
  stop("This fireSpread module was built with SpaDES version", usesSpaDESVersion,
       "Please update SpaDES to use this module")
}
rm(usesSpaDESVersion)

defineModule(sim, list(
  name = "fireSpread",
  description = "Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `stackName` be set.",
  keywords = c("fire", "percolation model", "spread algorithm"),
  childModules = character(),
  authors = c(person(c("Alex", "M"), "Chubaty",
                     email = "alexander.chubaty@canada.ca",
                     role = c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire",
                   email = "eliot.mcintire@canada.ca",
                   role = c("aut", "cre")),
            person("Steve", "Cumming",
                   email = "Steve.Cumming@sbf.ulaval.ca",
                   role = c("aut"))),
  version = numeric_version("1.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("methods", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter("nFires", "numeric", 10L, NA, NA, "number of fires to initiate"),
    defineParameter("its", "numeric", 1e6, NA, NA, "number of iterations for fire spread"),
    defineParameter("persistprob", "numeric", 0.00, 0, 1, "probability of fire persisting in a pixel"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, "fire return interval"),
    defineParameter("spreadprob", "numeric", 0.225, 0, 1, "probability of fire spreading into a pixel"),
    defineParameter("startTime", "numeric", 1.0, 0, NA, "time of initial fire ignition"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time interval between save events")
  ),
  inputObjects = data.frame(
    objectName = c(globals(sim)$stackName, globals(sim)$burnStats),
    objectClass = c("RasterStack", "numeric"),
    sourceURL = c(NA_character_, NA_character_),
    other = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c(globals(sim)$stackName, globals(sim)$burnStats),
    objectClass = c("RasterStack", "numeric"),
    other = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE)
))

## event types
doEvent.fireSpread <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more object dependencies:
    ### (use `checkObject` or similar)
    checkObject(sim, globals(sim)$stackName, layer = "habitatQuality")
    #    checkObject(sim, globals(sim)$burnStats)

    if (is.null(sim[[globals(sim)$burnStats]])) {
      sim[[globals(sim)$burnStats]] <- numeric()
    } else {
      npix <- sim[[(globals(sim)$burnStats)]]
      stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
    }

    # do stuff for this event
    sim <- sim$fireSpreadInit(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, params(sim)$fireSpread$startTime,
                         "fireSpread", "burn")
    sim <- scheduleEvent(sim, params(sim)$fireSpread$.saveInterval,
                         "fireSpread", "save", .last())
    sim <- scheduleEvent(sim, params(sim)$fireSpread$.plotInitialTime,
                         "fireSpread", "plot.init", .last())

  } else if (eventType == "burn") {
    # do stuff for this event
    sim <- sim$fireSpreadBurn(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, time(sim), "fireSpread", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpread$returnInterval,
                         "fireSpread", "burn")
  } else if (eventType == "stats") {
    # do stuff for this event
    sim <- sim$fireSpreadStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType == "plot.init") {
    # do stuff for this event
    setColors(sim[[globals(sim)$stackName]], n = c(Fires=10)) <-
      list(
        DEM = grDevices::terrain.colors(10),
        forestAge = brewer.pal(9,"BuGn"),
        habitatQuality = brewer.pal(8,"Spectral"),
        percentPine = brewer.pal(9,"Greens"),
        Fires = c("white", rev(heat.colors(9)))
      )

    Plot(sim[[globals(sim)$stackName]], new = TRUE,
         legendRange = list(0:maxValue(sim[[globals(sim)$stackName]]$DEM), 0:100, c(0,1), 0:100, 0:10))

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpread$.plotInterval,
                         "fireSpread", "plot", .last())
  } else if (eventType == "plot") {
    # do stuff for this event
    Plot(sim[[globals(sim)$stackName]]$Fires, new = FALSE)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpread$.plotInterval,
                         "fireSpread", "plot", .last())
  } else if (eventType == "save") {
    # do stuff for this event
    sim <- saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpread$.saveInterval,
                         "fireSpread", "save", .last()+1)
  } else {
    warning(paste(
      "Undefined event type: \'", events(sim)[1, "eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE],"\'",
      sep = ""
    ))
  }
  return(invisible(sim))
}

## event functions
fireSpreadInit <- function(sim) {
  landscapes <- sim[[globals(sim)$stackName]]

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(landscapes), ncol = ncol(landscapes),
                  nrow = nrow(landscapes), vals = 0)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  Fires <- setValues(Fires, 0)

  # add Fires map to global$stackName stack
  landscapes$Fires <- Fires
  sim[[globals(sim)$stackName]] <- landscapes

  return(invisible(sim))
}


fireSpreadBurn <- function(sim) {
  landscapes <- sim[[globals(sim)$stackName]]

  Fires <- spread(landscapes[[1]],
                  loci = as.integer(sample(1:ncell(landscapes), params(sim)$fireSpread$nFires)),
                  spreadProb = params(sim)$fireSpread$spreadprob,
                  persistance = params(sim)$fireSpread$persistprob,
                  mask = NULL,
                  maxSize = 1e8,
                  directions = 8,
                  iterations = params(sim)$fireSpread$its,
                  plot.it = FALSE,
                  mapID = TRUE)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  sim[[globals(sim)$stackName]] <- landscapes

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- sim[[globals(sim)$burnStats]]

  landscapes <- sim[[globals(sim)$stackName]]

  sim[[globals(sim)$burnStats]] <- c(npix, length(which(values(landscapes$Fires)>0)))

  return(invisible(sim))
}
