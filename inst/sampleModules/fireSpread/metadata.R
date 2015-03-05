### fireSpread module metadata
sim <- defineModule(sim, list(
  name="fireSpread",
  description="Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `.stackName` be set.",
  keywords=c("random map", "random landscape"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("methods", "raster", "RColorBrewer"),
  inputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE),
  outputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE)
))
