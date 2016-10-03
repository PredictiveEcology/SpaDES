defineModule(sim, list(
  name = "SpaDES_sampleModules",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M."), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = c("caribouMovement", "fireSpread", "randomLandscapes"),
  version = numeric_version("1.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year"
  citation = list(),
  documentation = list("SpaDES_sampleModules.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(objectName = NA_character_, objectClass = NA_character_, sourceURL = NA_character_, other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(objectName = NA_character_, objectClass = NA_character_, other = NA_character_, stringsAsFactors = FALSE)
))

### no other code is needed for this module group
