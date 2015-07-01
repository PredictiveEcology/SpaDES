test_that("mapReduced file does not work correctly", {

  Ras <- raster(extent(0,15,0,15), res=1)
  fullRas <- randomPolygons(Ras, numTypes=5, speedup=1, p=0.3)
  names(fullRas) <- "mapcodeAll"
  uniqueComms <- unique(fullRas)
  reducedDT <- data.table(mapcodeAll=uniqueComms,
     communities=sample(1:1000,length(uniqueComms)),
     biomass=rnbinom(length(uniqueComms),mu=4000,0.4))
  biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")

  expect_identical(sort(unique(getValues(biomass))), sort(reducedDT$biomass))
  expect_identical(length(unique(getValues(biomass))), length(unique(getValues(fullRas))))


  setkey(reducedDT, biomass)
  communities <- rasterizeReduced(reducedDT, fullRas, "communities")
  expect_identical(sort(unique(getValues(communities))), sort(reducedDT$communities))
  expect_identical(length(unique(getValues(communities))), length(unique(getValues(fullRas))))
})
