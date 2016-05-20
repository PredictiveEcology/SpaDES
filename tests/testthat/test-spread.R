test_that("spread produces legal RasterLayer", {
  set.seed(123)

  library(raster)
  # inputs for x
  a = raster(extent(0,100,0,100), res=1)
  b = raster(extent(a), res=1, vals=stats::runif(ncell(a),0,1))

  # check it makes a RasterLayer
  expect_that(spread(a, loci=ncell(a)/2, stats::runif(1,0.15,0.25)), is_a("RasterLayer"))

  #check wide range of spreadProbs
  for(i in 1:20) {
    expect_that(spread(a, loci=ncell(a)/2, stats::runif(1,0,1)), is_a("RasterLayer"))
  }

  # check spreadProbs outside of legal returns an "spreadProb is not a probability"
  expect_that(spread(a, loci=ncell(a)/2, 1.1), throws_error("spreadProb is not a probability"))
  expect_that(spread(a, loci=ncell(a)/2, -0.1), throws_error("spreadProb is not a probability"))

  # checks if maxSize is working properly
  # One process spreading
  expect_equal(ncell(a), tabulate(spread(a, spreadProb=1, mapID=TRUE)[]))

  # several processes spreading
  expect_equal(rep_len(3300,3),
               tabulate(spread(a, loci=c(100, 3500, 8000), spreadProb = 1,
                               mapID = TRUE, maxSize = rep_len(3300,3))[]))

  # Test that spreadState with a data.table works
  fires <- list()
  fires[[1]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices=TRUE,
                  0.235, 0, NULL, 1e8, 8, iterations = 2, mapID = TRUE)
  stopped <- list()
  stopped[[1]] <- fires[[1]][, sum(active), by=eventID][V1==0, eventID]
  for(i in 2:4){
    j = sample(1:1000,1);
    set.seed(j);
    fires[[i]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices=TRUE,
                  0.235, 0, NULL, 1e8, 8, iterations = 2, mapID = TRUE,
                  spreadState=fires[[i-1]])
    stopped[[i]] <- fires[[i]][, sum(active), by=eventID][V1==0, eventID]

    # Test that any fire that stopped previously is not rekindled
    expect_true(all(stopped[[i-1]] %in% stopped[[i]]))
  }

  # Test that passing NA to loci returns a correct data.table
  set.seed(123)
  fires <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices=TRUE,
                       0.235, 0, NULL, 1e8, 8, iterations = 2, mapID = TRUE)
  fires2 <- spread(a, loci=NA_real_, returnIndices=TRUE,
                       0.235, 0, NULL, 1e8, 8, iterations = 2, mapID = TRUE,
                       spreadState=fires)
  expect_true(all(fires2[,unique(eventID)] %in% fires[,unique(eventID)]))
  expect_true(all(fires[,unique(eventID)] %in% fires2[,unique(eventID)] ))
  expect_true(all(fires2[,length(initialLocus), by=eventID][,V1] ==
                    c(5,14,10,16,1,39,16,18,28,1)))

})


test_that("spread stopRule does not work correctly", {

  require(raster)
  a <- raster(extent(0,1e2,0,1e2), res = 1)
  hab <- gaussMap(a,speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) = "hab"
  hab2 <- hab>0
  maxRadius <- 25
  maxVal <- 50

  #set.seed(seed);
  #print(seed);
  #seed = 96848;
  #set.seed(seed);
  #print(seed);
  #fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL, maxSize = pi*14^2, 8, 1e6, mapID = TRUE, circle = FALSE)
  #Plot(fires, new=T)

  ## stopRule examples
  # examples with stopRule, which means that the eventual size is driven by the values on the raster
  #  passed in to the landscape argument
  set.seed(1234)
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape)>maxVal
  stopRuleA <- spread(hab, loci = startCells, 1, 0,
                  NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE,
                  circle = TRUE, stopRule = stopRule1)
  foo <- cbind(vals=hab[stopRuleA], id = stopRuleA[stopRuleA>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) > maxVal))


  # using stopRuleBehavior = "excludePixel"
  set.seed(1234)
  stopRuleB <- spread(hab, loci = startCells, 1, 0,
                  NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                  stopRuleBehavior = "excludePixel")
  foo <- cbind(vals=hab[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) <= maxVal))

  # If boolean, then it is exact
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == maxVal))


  # Test vector maxSize and stopRule when they interfere
  maxSizes <- sample(maxVal*2, length(startCells))
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  #Plot(stopRuleB, new=TRUE)
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == pmin(maxSizes, maxVal)))


  # Test non integer maxSize and stopRule when they interfere
  maxSizes <- runif(length(startCells), 1, maxVal*2)
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  #Plot(stopRuleB, new=TRUE)
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == pmin(floor(maxSizes), maxVal)))

  ####################################
  # Test for stopRuleBehavior
  ####################################
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape)>maxVal
  startCells <- as.integer(sample(1:ncell(hab), 2))
  set.seed(53432)
  circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, stopRule = stopRule2, stopRuleBehavior = "includeRing")
  cirs <- getValues(circs)
  vals <- tapply(hab[circs], cirs[cirs>0], sum)
  expect_true(all(vals>=maxVal))


  set.seed(53432)
  circs2 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludeRing")
  cirs <- getValues(circs2)
  vals <- tapply(hab[circs2], cirs[cirs>0], sum)
  expect_true(all(vals<=maxVal))

  set.seed(53432)
  circs3 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   mapID = TRUE, stopRule = stopRule2, stopRuleBehavior = "includePixel")
  cirs <- getValues(circs3)
  vals <- tapply(hab[circs3], cirs[cirs>0], sum)
  expect_true(all(vals<=(maxVal+maxValue(hab))))

  set.seed(53432)
  circs4 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   mapID = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludePixel")
  cirs <- getValues(circs4)
  vals <- tapply(hab[circs4], cirs[cirs>0], sum)
  expect_true(all(vals>=(maxVal-maxValue(hab))))

  # There should be 1 extra cell
  expect_true(sum(getValues(circs4)>0)+length(startCells) == sum(getValues(circs3)>0))
  # Order should be includeRing, includePixel, excludePixel, excludeRing
  expect_true(sum(getValues(circs)>0) > sum(getValues(circs3)>0))
  expect_true(sum(getValues(circs3)>0) > sum(getValues(circs4)>0))
  expect_true(sum(getValues(circs4)>0) > sum(getValues(circs2)>0))


  ####################################
  # Test for circles using maxDist
  ####################################

  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape)>maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, circleMaxRadius = maxRadius)
  cells <- which(getValues(circs)==1)
  centre <- xyFromCell(hab2,startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  expect_true(maxRadius==max(pd))

  # Test for circles using maxDist
  set.seed(543345)
  numCircs <- 4
#  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape)>maxVal
  startCells <- as.integer(sample(1:ncell(hab), numCircs))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, circleMaxRadius = maxRadius)
  #Plot(circs,new=TRUE)

  for(whCirc in 1:numCircs) {
    cells <- which(getValues(circs)==whCirc)
    centre <- xyFromCell(hab2,startCells)
    allCells <- xyFromCell(hab2, cells)
    pd <- pointDistance(centre[whCirc,], allCells, lonlat = FALSE)
    circEdge <- circs
    circEdge[] <- 0
    circEdge[cells[pd==maxRadius]] <- 1
    expect_true(all(circs[cells[pd==maxRadius]]==whCirc))
    if(!is.null(circs[as.vector(adj(hab2, cells[pd==maxRadius], pairs = FALSE))])) {
      # Test that there are both 0 and whCirc values, i.e,. it is on an edge
      expect_true(all(c(0,whCirc) %in% circs[as.vector(adj(hab2, cells[pd==maxRadius], pairs = FALSE))]))
    }
    #Plot(circEdge, addTo="circs", cols = c("transparent", rainbow(numCircs)[whCirc]))
  }


  # Test complex functions
  initialLoci <- (ncell(hab)-ncol(hab))/2 + c(4, -4)
  endSizes <- seq_along(initialLoci)*200
  stopRule3 <- function(landscape, mapID, endSizes) sum(landscape)>endSizes[mapID]

  TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
     directions = 8, mapID = TRUE, stopRule = stopRule3, endSizes = endSizes,
     stopRuleBehavior = "excludePixel")
  #Plot(TwoCirclesDiffSize, new=TRUE)
  cirs <- getValues(TwoCirclesDiffSize)
  vals <- tapply(hab[TwoCirclesDiffSize], cirs[cirs>0], sum)
  expect_true(all(vals<endSizes))


  # Testing allowOverlap

  initialLoci <- as.integer(sample(1:ncell(hab), 10))
  expect_silent(circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                  mapID = TRUE, circleMaxRadius = maxRadius, allowOverlap=TRUE))

  expect_silent(circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                                maxSize = 10, allowOverlap=TRUE))

  expect_silent(circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                                maxSize = seq_along(initialLoci)*3, allowOverlap=TRUE))


  # Test allowOverlap and stopRule
  for(i in 1:6) {
     maxVal <- sample(10:300,1)
     stopRule2 <- function(landscape,maxVal) sum(landscape)>maxVal
     expect_silent(
     circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                     stopRule = stopRule2, maxVal=maxVal, returnIndices = TRUE,
                     mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "includeRing")
     )

     vals <- tapply(hab[circs$indices], circs$eventID, sum)
     expect_true(all(vals>maxVal))
  }

 #stopRuleBehavior the allowOverlap
 maxVal <- 20
 stopRule2 <- function(landscape,maxVal) sum(landscape)>maxVal
 #expect_silent(
 circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                 stopRule = stopRule2, maxVal=maxVal, returnIndices = TRUE,
                 mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "excludePixel")
 #)
 vals <- tapply(hab[circs$indices], circs$eventID, sum)
 expect_true(all(vals<=maxVal))


 maxVal <- sample(10:100, 10)
 stopRule2 <- function(landscape,mapID,maxVal) sum(landscape)>maxVal[mapID]
 expect_silent(
 circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                 mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "excludePixel",
                 maxVal = maxVal, returnIndices = TRUE)
 )
 vals <- tapply(hab[circs$indices], circs$eventID, sum)
 expect_true(all(vals<=maxVal))
 # Test that maxSize can be a non integer value (i.e, Real)




 skip("This is just benchmarking, not testing")
 library(microbenchmark)
 microbenchmark(times = 200,
                excludePixel = spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                                mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "excludePixel",
                                maxVal = maxVal, returnIndices = TRUE),
                excludeRing = spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                               mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "excludeRing",
                               maxVal = maxVal, returnIndices = TRUE),
                includePixel = spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                               mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "includePixel",
                               maxVal = maxVal, returnIndices = TRUE),
                includeRing = spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                                      mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "includeRing",
                                      maxVal = maxVal, returnIndices = TRUE)
 )
 #Unit: milliseconds # with data.table
 #expr              min       lq     mean   median       uq       max neval
 #excludePixel 38.90842 41.71832 45.00469 44.33181 47.14418  62.58632   100
 #excludeRing  22.63004 23.80755 26.63432 25.76519 27.95789  41.47834   100
 #includePixel 38.46955 42.51963 48.04159 44.32482 47.41415 333.52346   100
 #includeRing  33.72337 36.62840 39.55411 38.71796 41.31295  63.29517   100

 # Remove data.table
 # Unit: milliseconds
 # expr              min       lq     mean   median       uq      max neval
 # excludePixel 27.31582 29.57508 33.80717 32.51402 35.86901 85.20527   200
 # excludeRing  15.59501 16.21633 19.32698 17.73696 20.59371 60.33322   200
 # includePixel 27.43088 29.19868 33.27228 31.67183 34.27935 94.79831   200
 # includeRing  22.76565 24.52749 27.56035 26.56609 29.32072 49.58507   200

 includePixel = spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                       mapID = TRUE, allowOverlap=TRUE, stopRuleBehavior = "includePixel",
                       maxVal = maxVal, returnIndices = TRUE)

 ## Make distance surface
 maxRadius = 10
 circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                 mapID = TRUE, circleMaxRadius = maxRadius, allowOverlap=TRUE)
 clumps <- raster::clump(circs)
 bounds <- raster::boundaries(clumps, classes=TRUE, type = "outer")
 spreadProb <- raster(clumps)
 spreadProb[] <- 1
 spreadProb[clumps==1 & bounds==0] <- 0

 clumps[is.na(clumps)] <- 0
 Plot(clumps,new=T,zero.color="white", cols = "Reds")

 whCells <- which(bounds[]>0)
 xy <- xyFromCell(circs, whCells)
 microbenchmark(times = 2,

                dists = spread(circs, loci = whCells, spreadProb = spreadProb, mapID = FALSE, circle=TRUE, allowOverlap=TRUE,
                               iterations = 20, directions = 8, returnIndices = FALSE)
                ,
                dists2 = distanceFromPoints(circs, xy = xy))
 Plot(dists,dists2,new=TRUE)

 })
