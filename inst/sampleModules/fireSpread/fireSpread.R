usesSpaDESVersion <- "1.3.1.9044"
if (packageVersion("SpaDES") < usesSpaDESVersion) {
  stop("This randomLandscapes module was built with SpaDES version", usesSpaDESVersion,
       ". Please update SpaDES to use this module.")
}
rm(usesSpaDESVersion)

defineModule(sim, list(
  name = "fireSpread",
  description = "Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `stackName` be set.",
  keywords = c("fire", "percolation model", "spread algorithm"),
  childModules = character(),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))
  ),
  version = list(SpaDES = "1.3.1.9044", fireSpread = "1.5.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("methods", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter("stackName", "character", "landscape", NA, NA, "name of the RasterStack"),
    defineParameter("nFires", "numeric", 10L, 1L, 100L, "number of fires to initiate"),
    defineParameter("its", "numeric", 1e6, 1e6, 1e6, "number of iterations for fire spread"),
    defineParameter("persistprob", "numeric", 0.00, 0, 1, "probability of fire persisting in a pixel"),
    defineParameter("returnInterval", "numeric", 1.0, 1.0, 1.0, "fire return interval"),
    defineParameter("spreadprob", "numeric", 0.225, 0.05, 0.5, "probability of fire spreading into a pixel"),
    defineParameter("startTime", "numeric", start(sim) + 1, 0, end(sim), "time of initial fire ignition"),
    defineParameter(".plotInitialTime", "numeric", start(sim), start(sim), end(sim) + 1, "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", 1, 1, 1, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time interval between save events")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = P(sim, "fireSpread")$stackName, objectClass = "RasterStack",
                 desc = NA_character_, sourceURL = NA_character_),
    expectsInput(objectName = globals(sim)$burnStats, objectClass = "numeric",
                 desc = NA_character_, sourceURL = NA_character_)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = P(sim, "fireSpread")$stackName, objectClass = "RasterStack",
                  desc = NA_character_, other = NA_character_),
    createsOutput(objectName = globals(sim)$burnStats, objectClass = "numeric",
                  desc = NA_character_, other = NA_character_)
  )
))

## event types
doEvent.fireSpread <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more object dependencies:
      ### (use `checkObject` or similar)
      checkObject(sim, P(sim)$stackName, layer = "habitatQuality")

      if (is.null(sim[[globals(sim)$burnStats]])) {
        sim[[globals(sim)$burnStats]] <- numeric()
      } else {
        npix <- sim[[(globals(sim)$burnStats)]]
        stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
      }

      # do stuff for this event
      sim <- sim$fireSpreadInit(sim)

      # schedule the next events
      sim <- scheduleEvent(sim, P(sim)$startTime, "fireSpread", "burn")
      sim <- scheduleEvent(sim, P(sim)$.saveInterval, "fireSpread", "save", .last())
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSpread", "plot.init", .last())
    },
    burn = {
      # do stuff for this event
      sim <- sim$fireSpreadBurn(sim)

      # schedule the next events
      sim <- scheduleEvent(sim, time(sim), "fireSpread", "stats") # do stats immediately following burn
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "fireSpread", "burn")
    },
    stats = {
      # do stuff for this event
      sim <- sim$fireSpreadStats(sim)

      # schedule the next event
      ## stats scheduling done by burn event
    },
    plot.init = {
      # do stuff for this event
      setColors(sim[[P(sim)$stackName]], n = c(Fires = 10)) <- list(
        DEM = brewer.pal(9, "YlOrBr"),
        forestAge = brewer.pal(9, "BuGn"),
        habitatQuality = brewer.pal(8, "Spectral"),
        percentPine = brewer.pal(9, "Greens"),
        Fires = c("white", rev(heat.colors(9)))
      )

      clearPlot()
      Plot(sim[[P(sim)$stackName]],
           legendRange = list(0:maxValue(sim[[P(sim)$stackName]]$DEM), 0:100,
                              c(0, 1), 0:100, 0:10))

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "fireSpread", "plot", .last())
    },
    plot = {
      # do stuff for this event
      Plot(sim[[P(sim)$stackName]]$Fires, new = FALSE)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "fireSpread", "plot", .last())
    },
    save = {
      # do stuff for this event
      sim <- saveFiles(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval,
                           "fireSpread", "save", .last() + 1)
    },
    warning(paste(
      "Undefined event type: \'", events(sim)[1, "eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
fireSpreadInit <- function(sim) {
  landscapes <- sim[[P(sim)$stackName]]

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(landscapes), ncol = ncol(landscapes),
                  nrow = nrow(landscapes), vals = 0)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  Fires <- setValues(Fires, 0)

  # add Fires map to global$stackName stack
  landscapes$Fires <- Fires
  sim[[P(sim)$stackName]] <- landscapes

  return(invisible(sim))
}

fireSpreadBurn <- function(sim) {
  landscapes <- sim[[P(sim)$stackName]]

  Fires <- spread(landscapes[[1]],
                  loci = as.integer(sample(1:ncell(landscapes), P(sim)$nFires)),
                  spreadProb = P(sim)$spreadprob,
                  persistance = P(sim)$persistprob,
                  mask = NULL,
                  maxSize = 1e8,
                  directions = 8,
                  iterations = P(sim)$its,
                  plot.it = FALSE,
                  id = TRUE)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  sim[[P(sim)$stackName]] <- landscapes

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- sim[[globals(sim)$burnStats]]

  landscapes <- sim[[P(sim)$stackName]]

  sim[[globals(sim)$burnStats]] <- c(npix, length(which(values(landscapes$Fires) > 0)))

  return(invisible(sim))
}
