### caribouMovement module metadata
sim <- defineModule(sim, list(
  name="caribouMovement",
  description="simulate caribou movement via correlated random walk. Requires a RasterStack object whose name is specified by `simGlobals(sim)$.stackName`, containing a RasterLayer named `habitatQuality`.",
  keywords=c("caribou", "individual based movement model"),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("grid", "raster", "sp"),
  inputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE),
  outputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE)
))
