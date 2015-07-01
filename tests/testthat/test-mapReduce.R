test_that("mapReduced file does not work correctly", {
Ras <- raster(extent(0,15,0,15), res=1)
fullRas <- randomPolygons(Ras, numTypes=5, speedup=1, p=0.3)
names(fullRas) <- "mapcodeAll"
uniqueComms <- unique(fullRas)
reducedDT <- data.table(
  mapcodeAll=uniqueComms,
  communities=sample(1:1000,length(uniqueComms)),
  biomass=rnbinom(length(uniqueComms),mu=4000,0.4)
)
biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
setkey(reducedDT, biomass)
communities <- rasterizeReduced(reducedDT, fullRas, "communities")
# needs actual expect_xyz statements
})
