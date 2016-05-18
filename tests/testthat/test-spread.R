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
  maxVal <- 50
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape)>maxVal
  stopRuleA <- spread(hab, loci = startCells, 1, 0,
                  NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE,
                  circle = TRUE, stopRule = stopRule1)
  foo <- cbind(vals=hab[stopRuleA], id = stopRuleA[stopRuleA>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) > maxVal))


  # using stopRuleExact = TRUE
  stopRuleB <- spread(hab, loci = startCells, 1, 0,
                  NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                  stopRuleExact = TRUE)
  foo <- cbind(vals=hab[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) <= maxVal))

  # If boolean, then it is exact
  hab2 <- hab>0
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = 1e6, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleExact = TRUE)
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == maxVal))


  # Test vector maxSize and stopRule when they interfere
  maxSizes <- sample(maxVal*2, length(startCells))
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleExact = TRUE)
  #Plot(stopRuleB, new=TRUE)
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == pmin(maxSizes, maxVal)))


  # Test non integer maxSize and stopRule when they interfere
  maxSizes <- runif(length(startCells), 1, maxVal*2)
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, mapID = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleExact = TRUE)
  #Plot(stopRuleB, new=TRUE)
  foo <- cbind(vals=hab2[stopRuleB], id = stopRuleB[stopRuleB>0]);
  expect_true(all( tapply(foo[,"vals"], foo[,"id"], sum) == pmin(floor(maxSizes), maxVal)))

  # Test for circles
  maxVal <- 200
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape)>maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, stopRule = stopRule2, stopRuleExact=FALSE)
  cells <- which(getValues(circs)==1)
  centre <- xyFromCell(hab2,startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  #pi*r^2 = maxVal
  r = sqrt(maxVal/pi)
  expect_true(ceiling(r)+1==max(pd))
  expect_true((r+1)<=max(pd))


  set.seed(53432)
  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, stopRule = stopRule2, stopRuleExact=TRUE)
  cells <- which(getValues(circs)==1)
  centre <- xyFromCell(hab2,startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  #pi*r^2 = maxVal
  r = sqrt(maxVal/pi)

  hab3 <- hab2
  hab3[] <- 0
  hab3[cells[which(pd>r)]] <- 1
  #Plot(hab3, new=T)

  expect_true((r+1)>=max(pd))
  expect_true((r)<max(pd))



  # Test for circles using maxDist
  maxRadius <- 25
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape)>maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  mapID = TRUE, circleMaxRadius = maxRadius)
  cells <- which(getValues(circs)==1)
  centre <- xyFromCell(hab2,startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  #Plot(circs,new=TRUE)
  circEdge <- circs
  circEdge[] <- 0
  circEdge[cells[pd==maxRadius]] <- 1
  #Plot(circEdge, addTo="circs", cols = c("transparent", "red"))
  expect_true(maxRadius==max(pd))

  # Test for circles using maxDist
  maxRadius <- 25
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
     stopRuleExact = TRUE)
  #Plot(TwoCirclesDiffSize, new=TRUE)
  cirs <- getValues(TwoCirclesDiffSize)
  vals <- tapply(hab[TwoCirclesDiffSize], cirs[cirs>0], sum)
  expect_true(all(vals<endSizes))


  # Test that maxSize can be a non integer value (i.e, Real)
})
