test_that("spread produces legal RasterLayer", {
  set.seed(123)

  library(raster); on.exit(detach("package:raster"), add = TRUE)

  # inputs for x
  a <- raster(extent(0, 100 , 0,100), res = 1)
  b <- raster(extent(a), res = 1, vals = stats::runif(ncell(a), 0 ,1))

  # check it makes a RasterLayer
  expect_that(spread(a, loci = ncell(a) / 2, stats::runif(1, 0.15, 0.25)), is_a("RasterLayer"))

  #check wide range of spreadProbs
  for (i in 1:20) {
    expect_that(spread(a, loci = ncell(a) / 2, stats::runif(1, 0, 1)), is_a("RasterLayer"))
  }

  # check spreadProbs outside of legal returns an "spreadProb is not a probability"
  expect_that(spread(a, loci = ncell(a) / 2, 1.1), throws_error("spreadProb is not a probability"))
  expect_that(spread(a, loci = ncell(a) / 2, -0.1), throws_error("spreadProb is not a probability"))

  # checks if maxSize is working properly
  # One process spreading
  expect_equal(ncell(a), tabulate(spread(a, spreadProb = 1, id = TRUE)[]))

  # several processes spreading
  sizes <- rep_len(330, 3)
  expect_equal(sizes,
               tabulate(spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                               id = TRUE, maxSize = sizes)[]))

  # Test that spreadState with a data.table works
  fires <- list()
  fires[[1]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices = TRUE,
                       spreadProb=0.235, persistence=0, mask=NULL,
                       maxSize = 1e8, 8, iterations = 2, id = TRUE)
  stopped <- list()
  stopped[[1]] <- fires[[1]][, sum(active), by = id][V1 == 0, id]
  for (i in 2:4) {
    j <- sample(1:1000, 1);
    set.seed(j);
    fires[[i]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices = TRUE,
                         spreadProb = 0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE,
                         spreadState = fires[[i - 1]])
    stopped[[i]] <- fires[[i]][, sum(active), by = id][V1 == 0, id]

    # Test that any fire that stopped previously is not rekindled
    expect_true(all(stopped[[i - 1]] %in% stopped[[i]]))
  }

  # Test that passing NA to loci returns a correct data.table
  set.seed(123)
  fires <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices = TRUE,
                  0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE)
  fires2 <- spread(a, loci = NA_real_, returnIndices = TRUE,
                   0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE,
                   spreadState = fires)
  expect_true(all(fires2[, unique(id)] %in% fires[, unique(id)]))
  expect_true(all(fires[, unique(id)] %in% fires2[, unique(id)]))
  expect_true(all(fires2[, length(initialLocus), by = id][, V1] ==
                    c(4L, 8L, 7L, 9L, 1L, 25L, 13L, 13L, 20L, 1L)))
})

test_that("spread stopRule does not work correctly", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50

  #set.seed(seed);
  #print(seed);
  #seed = 96848;
  #set.seed(seed);
  #print(seed);
  #fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL, maxSize = pi*14^2, 8, 1e6, id = TRUE, circle = FALSE)
  #if(interactive()) Plot(fires, new=T)

  ## stopRule examples
  # examples with stopRule, which means that the eventual size is driven by the values on the raster
  #  passed in to the landscape argument
  set.seed(1234)
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape) > maxVal
  stopRuleA <- spread(hab, loci = startCells, spreadProb = 1, persistence = 0,
                      mask = NULL, maxSize = 1e6, directions = 8,
                      iterations = 1e6, id = TRUE,
                      circle = TRUE, stopRule = stopRule1)
  foo <- cbind(vals = hab[stopRuleA], id = stopRuleA[stopRuleA > 0]);
  expect_true(all( tapply(foo[, "vals"], foo[, "id"], sum) > maxVal))

  # using stopRuleBehavior = "excludePixel"
  set.seed(1234)
  stopRuleB <- spread(hab, loci = startCells, 1, 0,
                      NULL, maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  foo <- cbind(vals = hab[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all( tapply(foo[, "vals"], foo[, "id"], sum) <= maxVal))

  # If boolean, then it is exact
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all( tapply(foo[, "vals"], foo[, "id"], sum) == maxVal))

  # Test vector maxSize and stopRule when they interfere
  maxSizes <- sample(maxVal * 2, length(startCells))
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(stopRuleB, new = TRUE)
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all( tapply(foo[, "vals"], foo[, "id"], sum) == pmin(maxSizes, maxVal)))

  # Test non integer maxSize and stopRule when they interfere
  maxSizes <- runif(length(startCells), 1, maxVal * 2)
  stopRuleB <- spread(hab2, loci = startCells, 1, 0,
                      NULL, maxSize = maxSizes, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(stopRuleB, new = TRUE)
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all( tapply(foo[, "vals"], foo[, "id"], sum) == pmin(floor(maxSizes), maxVal)))

  ####################################
  # Test for stopRuleBehavior
  ####################################
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 2))
  set.seed(53432)
  circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                  id = TRUE, stopRule = stopRule2, stopRuleBehavior = "includeRing")
  cirs <- getValues(circs)
  vals <- tapply(hab[circs], cirs[cirs > 0], sum)
  expect_true(all(vals >= maxVal))

  set.seed(53432)
  circs2 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludeRing")
  cirs <- getValues(circs2)
  vals <- tapply(hab[circs2], cirs[cirs > 0], sum)
  expect_true(all(vals <= maxVal))

  set.seed(53432)
  circs3 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "includePixel")
  cirs <- getValues(circs3)
  vals <- tapply(hab[circs3], cirs[cirs > 0], sum)
  expect_true(all(vals <= (maxVal + maxValue(hab))))

  set.seed(53432)
  circs4 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludePixel")
  cirs <- getValues(circs4)
  vals <- tapply(hab[circs4], cirs[cirs > 0], sum)
  expect_true(all(vals >= (maxVal - maxValue(hab))))

  # There should be 1 extra cell
  expect_true(sum(getValues(circs4) > 0) + length(startCells) == sum(getValues(circs3) > 0))
  # Order should be includeRing, includePixel, excludePixel, excludeRing
  expect_true(sum(getValues(circs) > 0) > sum(getValues(circs3) > 0))
  expect_true(sum(getValues(circs3) > 0) > sum(getValues(circs4) > 0))
  expect_true(sum(getValues(circs4) > 0) > sum(getValues(circs2) > 0))

  ####################################
  # Test for circles using maxDist
  ####################################

  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  id = TRUE, circleMaxRadius = maxRadius)
  cells <- which(getValues(circs) == 1)
  centre <- xyFromCell(hab2, startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  expect_true(maxRadius == max(pd))

  # Test for circles using maxDist
  set.seed(543345)
  numCircs <- 4
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), numCircs))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE,
                  loci = startCells,
                  id = TRUE, circleMaxRadius = maxRadius)
  if (interactive()) Plot(circs, new = TRUE)

  for (whCirc in 1:numCircs) {
    cells <- which(getValues(circs) == whCirc)
    centre <- xyFromCell(hab2, startCells)
    allCells <- xyFromCell(hab2, cells)
    pd <- pointDistance(centre[whCirc, ], allCells, lonlat = FALSE)
    circEdge <- circs
    circEdge[] <- 0
    circEdge[cells[pd == maxRadius]] <- 1
    expect_true(all(circs[cells[pd == maxRadius]] == whCirc))
    if (!is.null(circs[as.vector(adj(hab2, cells[pd == maxRadius], pairs = FALSE))])) {
      # Test that there are both 0 and whCirc values, i.e,. it is on an edge
      expect_true(all(c(0, whCirc) %in% circs[as.vector(adj(hab2, cells[pd == maxRadius], pairs = FALSE))]))
    }
    if (interactive()) Plot(circEdge, addTo = "circs", cols = c("transparent", rainbow(numCircs)[whCirc]))
  }

  # Test complex functions
  initialLoci <- (ncell(hab) - ncol(hab)) / 2 + c(4, -4)
  endSizes <- seq_along(initialLoci) * 200
  stopRule3 <- function(landscape, id, endSizes) sum(landscape) > endSizes[id]

  TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                               directions = 8, id = TRUE, stopRule = stopRule3, endSizes = endSizes,
                               stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(TwoCirclesDiffSize, new = TRUE)
  cirs <- getValues(TwoCirclesDiffSize)
  vals <- tapply(hab[TwoCirclesDiffSize], cirs[cirs > 0], sum)
  expect_true(all(vals < endSizes))

  # Test allowOverlap
  initialLoci <- as.integer(sample(1:ncell(hab), 10))
  expect_silent(circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                                id = TRUE, circleMaxRadius = maxRadius, allowOverlap = TRUE))

  expect_silent(circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                                maxSize = 10, allowOverlap = TRUE))

  expect_silent(circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                                maxSize = seq_along(initialLoci) * 3, allowOverlap = TRUE))

  # Test allowOverlap and stopRule
  for (i in 1:6) {
    maxVal <- sample(10:300, 1)
    stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal
    expect_silent(
      circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                      stopRule = stopRule2, maxVal = maxVal, returnIndices = TRUE,
                      id = TRUE, allowOverlap = TRUE, stopRuleBehavior = "includeRing")
    )

    vals <- tapply(hab[circs$indices], circs$id, sum)
    expect_true(all(vals > maxVal))
  }

  #stopRuleBehavior the allowOverlap
  maxVal <- 20
  stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal
  #expect_silent(
  circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                  stopRule = stopRule2, maxVal = maxVal, returnIndices = TRUE,
                  id = TRUE, allowOverlap = TRUE, stopRuleBehavior = "excludePixel")
  #)
  vals <- tapply(hab[circs$indices], circs$id, sum)
  expect_true(all(vals <= maxVal))

  maxVal <- sample(10:100, 10)
  stopRule2 <- function(landscape, id, maxVal) sum(landscape) > maxVal[id]
  expect_silent(
    circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                    stopRule = stopRule2, id = TRUE, allowOverlap = TRUE,
                    stopRuleBehavior = "excludePixel", maxVal = maxVal,
                    returnIndices = TRUE)
  )
  vals <- tapply(hab[circs$indices], circs$id, sum)
  expect_true(all(vals <= maxVal))
  # Test that maxSize can be a non integer value (i.e, Real)

  # Test arbitrary raster as part of stopRule
  # Stop if sum of landscape is big or mean of quality is too small
  for (i in 1:6) {
    initialLoci <- as.integer(sample(1:ncell(hab), 10))
    quality <- raster(hab)
    quality[] <- runif(ncell(quality), 0, 1)
    sumLandscapeRule <- 100
    meanHabitatRule <- 0.4
    stopRule4 <- function(landscape, quality, cells) {
      (sum(landscape) > sumLandscapeRule) | (mean(quality[cells]) < meanHabitatRule)
    }

    circs <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                    directions = 8, id = TRUE, stopRule = stopRule4, quality = quality,
                    stopRuleBehavior = "includePixel", returnIndices = TRUE)

    ras <- raster(quality)
    ras[] <- 0
    circsVals <- circs[, numEvents := sum(unique(id)), by = indices]
    ras[circsVals$indices] <- circsVals$numEvents
    a1 <- cbind(quality = quality[ras], hab = hab[ras], id = ras[ras])
    a2 <- tapply(a1[, "hab"], a1[, "id"], sum)
    a3 <- tapply(a1[, "quality"], a1[, "id"], mean)
    wh <- which(a3 < meanHabitatRule)
    a4 <- tapply(a1[, "quality"], a1[, "id"], length)
    expect_true(all(a2[wh] < sumLandscapeRule))
    expect_true(all(a2[-wh] >= sumLandscapeRule))
    expect_true(all(a3[-wh] >= meanHabitatRule))
    expect_true(all(a3[wh] < meanHabitatRule))
    if (interactive()) {
      clearPlot()
      Plot(ras)
    }
  }
})

test_that("asymmetry doesn't work properly", {
  library(CircStats); on.exit(detach("package:CircStats"), add = TRUE)
  library(raster); on.exit(detach("package:raster"), add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50
  set.seed(53432)

  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  N <- 16
  avgAngles <- numeric(N)
  lenAngles <- numeric(N)

  # function to calculate mean angle -- returns in degrees
  meanAngle <- function(angles) {
    deg(atan2(mean(sin(rad(angles))), mean(cos(rad(angles)))))
  }

  if (interactive()) clearPlot()
  seed <- sample(1e6, 1)
  set.seed(seed)
  for (asymAng in (2:N)) {
    circs <- spread(hab, spreadProb = 0.25, loci = ncell(hab) / 2 - ncol(hab) / 2,
                    id = TRUE, returnIndices = TRUE,
                    asymmetry = 40, asymmetryAngle = asymAng * 20)
    ci <- raster(hab)
    ci[] <- 0
    ci[circs$indices] <- circs$id
    ciCentre <- raster(ci)
    ciCentre[] <- 0
    ciCentre[unique(circs$initialLocus)] <- 1
    newName <- paste0("ci", asymAng * 20)
    assign(newName, ci)

    where2 <- function(name, env = parent.frame()) { # simplified from pryr::where
      if (exists(name, env, inherits = FALSE)) env else where2(name, parent.env(env))
    }
    env <- where2(newName)
    if (interactive()) {
      Plot(get(newName, envir = env))
      Plot(ciCentre, cols = c("transparent", "black"), addTo = newName)
    }
    # Sys.sleep(1)
    a <- cbind(id = circs$id, to = circs$indices, xyFromCell(hab, circs$indices))
    initialLociXY <- cbind(id = unique(circs$id), xyFromCell(hab, unique(circs$initialLocus)))
    #dirs <- .matchedPointDirection(a, initialLociXY)
    dirs <- directionFromEachPoint(from = initialLociXY, to = a)
    dirs[, "angles"] <- CircStats::deg(dirs[, "angles"])
    avgAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], meanAngle) %% 360
    lenAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], length)
  }

  whBig <- which(lenAngles > 50)
  pred <- (1:N)[whBig] * 20
  expect_true(abs(coef(lm(avgAngles[whBig]~pred))[[2]] - 1) < 0.1)
})

test_that("spread benchmarking", {
  skip("This is just benchmarking, not testing")

  library(raster); on.exit(detach("package:raster"), add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50

  library(microbenchmark)
  initialLoci <- as.integer(sample(1:ncell(hab), 3))
  maxVal <- 20
  stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal

  microbenchmark(times = 200,
                 excludePixel = spread(hab, spreadProb = 1, circle = TRUE,
                                       loci = initialLoci, stopRule = stopRule2,
                                       id = TRUE, allowOverlap = TRUE,
                                       stopRuleBehavior = "excludePixel",
                                       maxVal = maxVal, returnIndices = TRUE),
                 excludeRing = spread(hab, spreadProb = 1, circle = TRUE,
                                      loci = initialLoci, stopRule = stopRule2,
                                      id = TRUE, allowOverlap = TRUE,
                                      stopRuleBehavior = "excludeRing",
                                      maxVal = maxVal, returnIndices = TRUE),
                 includePixel = spread(hab, spreadProb = 1, circle = TRUE,
                                       loci = initialLoci, stopRule = stopRule2,
                                       id = TRUE, allowOverlap = TRUE,
                                       stopRuleBehavior = "includePixel",
                                       maxVal = maxVal, returnIndices = TRUE),
                 includeRing = spread(hab, spreadProb = 1, circle = TRUE,
                                      loci = initialLoci, stopRule = stopRule2,
                                      id = TRUE, allowOverlap = TRUE,
                                      stopRuleBehavior = "includeRing",
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

  includePixel <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                         stopRule = stopRule2, id = TRUE, allowOverlap = TRUE,
                         stopRuleBehavior = "includePixel",
                         maxVal = maxVal, returnIndices = TRUE)

  ## Make distance surface
  maxRadius <- 10
  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                  id = TRUE, circleMaxRadius = maxRadius, allowOverlap = TRUE)
  clumps <- raster::clump(circs)
  bounds <- raster::boundaries(clumps, classes = TRUE, type = "outer")
  spreadProb <- raster(clumps)
  spreadProb[] <- 1
  spreadProb[clumps == 1 & bounds == 0] <- 0

  clumps[is.na(clumps)] <- 0
  if (interactive()) Plot(clumps, new = TRUE, zero.color = "white", cols = "Reds")

  whCells <- which(bounds[] > 0)
  xy <- xyFromCell(circs, whCells)
  microbenchmark(times = 2,
                 dists = spread(circs, loci = whCells, spreadProb = spreadProb,
                                id = FALSE, circle = TRUE, allowOverlap = TRUE,
                                iterations = 20, directions = 8, returnIndices = FALSE),
                 dists2 = distanceFromPoints(circs, xy = xy))
  if (interactive()) Plot(dists, dists2, new = TRUE)

  library(raster); on.exit(detach("package:raster"), add = TRUE)

  a <- raster(extent(0, 436, 0, 296), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxVal <- 250
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape) > maxVal
  microbenchmark(stopRuleA <- spread(hab, loci = startCells, 1, 0,
                                     NULL, maxSize = 1e6, 8, 1e6, id = TRUE,
                                     circle = TRUE, stopRule = stopRule1))
  if (interactive()) Plot(stopRuleA, new = TRUE)

  # Internal conversion to vector -- almost 3x faster
  #     min       lq     mean   median       uq      max neval
  #34.91276 35.26146 37.86623 35.81296 40.09197 61.20151    50

  # Keep as raster
  #     min       lq     mean   median       uq      max neval
  #97.65601 102.6857 118.7154 115.3167 126.9112 173.6077    50

  # ARbitrary stopRule is much slower than sizes -- 5x for this example
  a <- raster(extent(0, 100, 0, 100), res = 1)
  a[] <- 1
  sizes <- rep(3300, 3)
  set.seed(343)
  microbenchmark(times = 100, maxSize = spread(a, loci = c(100, 3500, 8000),
                                               spreadProb = 1, id = TRUE,
                                               maxSize = sizes))
  set.seed(343)
  microbenchmark(times = 20,
                 stopRule = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                   stopRuleBehavior = "excludePixel", id = TRUE,
                                   stopRule = function(cells, id) length(cells) > sizes[id]))
  # Unit: milliseconds
  #    expr      min       lq     mean   median       uq     max neval
  # maxSize 35.44505 37.38652 41.36509  38.2398 47.60329 84.6241   100
  #stopRule 102.4452 205.5738 216.8115 211.6224  214.788  451.72    20

  quality <- raster(hab)
  quality[] <- runif(ncell(quality), 0, 1)
  stopRule4 <- function(landscape, quality, cells) (sum(landscape) > 200) | (mean(quality[cells]) < 0.5)
  set.seed(23432)
  microbenchmark(circs <- spread(hab, spreadProb = 1, loci = initialLoci,
                                 circle = TRUE, directions = 8, id = TRUE,
                                 stopRule = stopRule4, quality = quality,
                                 stopRuleBehavior = "includeRing"), times = 50)

  # ARbitrary stopRule is much slower than sizes -- 5x for this example
  a <- raster(extent(0, 300, 0, 300), res = 1)
  a[] <- 1
  sizes <- rep(6600, 3)
  set.seed(343)
  microbenchmark(times = 20, maxSize = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                              id = TRUE, maxSize = sizes))
  set.seed(343)
  microbenchmark(times = 20,
                 stopRule = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                   stopRuleBehavior = "excludePixel", id = TRUE,
                                   stopRule = function(cells, id) length(cells) > sizes[id]))
  # With 300x300 raster and 6600 sizes
  # maxSizes
  # Unit: milliseconds
  #     expr      min       lq     mean   median       uq      max neval
  # maxSize  50.39086 54.11104 74.02115  57.60774 101.3887 129.1427    20
  # stopRule  423.923  470.764 552.4836  521.938  594.7501 886.5732    20
  library(profvis)
  pv <- profvis(spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                       stopRuleBehavior = "excludePixel", id = TRUE,
                       stopRule = function(cells, id) length(cells) > sizes[id]))
  pv

  ##foo <- fooOrig
  microbenchmark(times = 10, long = {
    ord <- order(foo[, "id"])
    foo1 <- foo[ord, ]
    ids <- unique(foo1[, "id"])
    fooB <- unlist(lapply(ids, function(id){
      duplicated(
        foo1[foo1[, "id"] == id, "indices"]
      )
    }))
  }, short = {
    fooA <- unlist(tapply(foo1[, "indices"], foo1[, "id"], duplicated))
  })
})

test_that("rings and cir", {
  library(sp)
  library(raster)
  library(fpCompare)
  library(data.table)

  on.exit({
    detach("package:data.table")
    detach("package:fpCompare")
    detach("package:raster")
  }, add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  N <- 2
  caribou <- SpatialPoints(coords = cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                                          y = stats::runif(N, xmin(hab), xmax(hab))))

  radius <- 15
  cirsEx <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius, simplify = TRUE,
                includeBehavior = "excludePixels")
  cirsIncl <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius, simplify = TRUE,
                  includeBehavior = "includePixels")

  expect_true(NROW(cirsEx) < NROW(cirsIncl))

  # With including pixels, then distances are not strictly within the bounds of minRadius
  #   and maxRadius, because every cell is included if it has a point anywhere within
  #   the cell, causing cells whose centres are beyond maxRadius or shorter than minRadius
  #   to be accepted
  b <- cbind(coordinates(caribou), id = seq_along(caribou))
  a <- as.matrix(cirsIncl)
  colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
  dists <- distanceFromEachPoint(b, a)
  expect_true(radius * 1.5 < max(dists[, "dists"]))
  expect_true(radius > min(dists[, "dists"]))

  # With excluding pixels, then distances are strictly within the bounds
  b <- cbind(coordinates(caribou), id = seq_along(caribou))
  a <- as.matrix(cirsEx)
  colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
  dists <- distanceFromEachPoint(b, a)
  expect_true((radius * 1.5) %>=% max(dists[, "dists"]))
  expect_true(radius %<=% min(dists[, "dists"]))

  ras1 <- raster(hab)
  ras1[] <- 0
  cirsOverlap <- data.table(cirsEx)[, list(sumIDs = sum(id)), by = indices]
  ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
  if (interactive()) {clearPlot(); Plot(ras1)}

  ras3 <- raster(hab)
  ras3[] <- 0
  cirsOverlap <- data.table(cirsIncl)[, list(sumIDs = sum(id)), by = indices]
  ras3[cirsOverlap$indices] <- cirsOverlap$sumIDs
  ras3 <- ras1 * 10 + ras3
  if (interactive()) Plot(ras3)
  expect_true(all(getValues(ras3) != 10)) # None should have only ras1, i.e., only circEx cells
  expect_true(all(getValues(ras3) != 20)) # None should have only ras1, i.e., only circEx cells

  cirsExSkinny <- data.table(cir(hab, caribou, maxRadius = radius, simplify = TRUE,
                                 includeBehavior = "excludePixels"))
  expect_true(NROW(cirsExSkinny) == 0)

  # Compare rings and cir -- if start in centre of cells, then should be identical
  N <- 2
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5))

  loci <- cellFromXY(hab, coordinates(caribou)[1, ])
  cirs <- data.table(cir(hab, caribou[1, ], maxRadius = radius * 1.5001, minRadius = radius,
                         simplify = TRUE, allowOverlap = TRUE,
                         includeBehavior = "excludePixels", returnDistances = TRUE))
  cirs2 <- rings(hab, loci, minRadius = radius, maxRadius = radius * 1.5001,
                 allowOverlap = TRUE, returnIndices = TRUE, includeBehavior = "includeRing")

  expect_true(all.equal(range(cirs$dists), range(cirs2$dists)))
  setkey(cirs2, dists, indices)
  setkey(cirs,  dists, indices)
  ras1 <- raster(hab)
  ras1[] <- 0
  cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
  ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
  if (interactive()) Plot(ras1, new = TRUE)

  ras2 <- raster(hab)
  ras2[] <- 0
  cirsOverlap2 <- cirs2[, list(sumIDs = sum(id)), by = indices]
  ras2[cirsOverlap2$indices] <- cirsOverlap2$sumIDs
  ras3 <- ras1 - ras2
  if (interactive()) Plot(ras2, ras3, zero.color = "transparent")
  expect_equal(0, sum(abs(getValues(ras3))))

  N <- 2
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5))

  loci <- cellFromXY(hab, coordinates(caribou))
  dists1 <- rings(hab, loci, minRadius = 0, maxRadius = ncol(hab), returnDistances = TRUE,
                  includeBehavior = "includeRing")
  dists2 <- distanceFromPoints(hab, coordinates(caribou))
  dists3 <- cir(landscape = hab, loci = loci, minRadius = 0, maxRadius = ncol(hab),
                includeBehavior = "includePixels", allowOverlap = FALSE,
                returnIndices = FALSE, closest = TRUE, returnDistances = TRUE)
  if (interactive()) Plot(dists1, dists2, dists3, new = TRUE)
  diffDists12 <- abs(dists1 - dists2)
  diffDists23 <- abs(dists2 - dists3)
  tabs12 <- table(round(getValues(diffDists12)))
  tabs23 <- table(round(getValues(diffDists23)))

  # This tests that the two approaches are 99% similar
  expect_true(tabs12[names(tabs12) == 0] / ncell(diffDists12) > 0.99)
  expect_true(tabs23[names(tabs23) == 0] / ncell(diffDists23) > 0.99)

  if (interactive()) Plot(diffDists12, diffDists23, new = TRUE)

  skip("Below here is just benchmarking, not testing")

  library(microbenchmark)
  #loci <- cellFromXY(hab, coordinates(caribou))
  microbenchmark(
    times = 10,
    dists1 <- rings(hab, loci, minRadius = 0, maxRadius = ncol(hab), returnDistances = TRUE,
                    includeBehavior = "includeRing"),
    dists2 <- distanceFromPoints(hab, coordinates(caribou)),
    dists3 <- cir(landscape = hab, loci = loci, minRadius = 0, maxRadius = ncol(hab),
                  includeBehavior = "includePixels", allowOverlap = FALSE,
                  returnIndices = FALSE, closest = TRUE, returnDistances = TRUE)
  )
})

test_that("distanceFromPoints does not work correctly", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(fpCompare); on.exit(detach("package:fpCompare"), add = TRUE)
  library(data.table); on.exit(detach("package:data.table"), add = TRUE)

  hab <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(hab, speedup = 1) # if raster is large (>1e6 pixels), use speedup > 1
  names(hab) <- "hab"
  N <- 1
  coords <- cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                  y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5)
  distsDFP1Pt <- distanceFromPoints(hab, coords[1, , drop = FALSE])
  distsDFEP1Pt <- distanceFromEachPoint(coords[1, , drop = FALSE], landscape = hab)
  ras1 <- raster(hab)
  ras1[] <- distsDFEP1Pt[, "dists"]
  expect_identical(0, unique(round(getValues(distsDFP1Pt - ras1), 7)) )
  if (interactive()) Plot(distsDFP1Pt, ras1, new = TRUE)

  maxDistance <- 30
  distsDFEPMaxD <- dists6 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = maxDistance)
  expect_true(round(max(distsDFEPMaxD[, "dists"]), 7) == maxDistance) # test that maxDistance arg is working

  # evaluate cumulativeFn
  N <- 5
  hab <- raster(extent(0, 10, 0, 10), res = 1)
  coords <- cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                  y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5)
  dfep20 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab)
  idw <- tapply(dfep20[, c("dists")], cellFromXY(hab, dfep20[, c("x", "y")]), function(x) sum(1 / (1 + x)))
  dfep <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab, cumulativeFn = `+`)
  expect_true(sum(idw - dfep[, "val"]) %==% 0)

  skip("this is currently only benchmarking")

  library(microbenchmark); on.exit(detach("package:microbenchmark"), add = TRUE)

  loci <- cellFromXY(hab, xy = coords)
  distsCir <-  dists7 <- cir(coords, landscape = hab, maxRadius = 30,
                             minRadius = 0, returnDistances = TRUE)
  distsRings <-  dists8 <- rings(loci = loci, landscape = hab, maxRadius = 30,
                                 minRadius = 0, allowOverlap = TRUE, returnIndices = TRUE)

  hab <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(hab, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  N <- 10
  coords <- cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                 y = stats::runif(N, xmin(hab), xmax(hab)))
  indices <- 1:ncell(hab)

  microbenchmark(
    times = 3,
    distsDFP10Pts = dists2 <- distanceFromPoints(hab, coords[, c("x", "y")]),
    distsDFP1Pt = dists3 <- distanceFromPoints(hab, coords[1, c("x", "y"), drop = FALSE]),
    distsDFEP10Pts = dists1 <- distanceFromEachPoint(coords, landscape = hab),
    distsDFEP1Pt = dists5 <- distanceFromEachPoint(coords[1, , drop = FALSE], landscape = hab),
    distsDFEPMaxD =  dists6 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 30),
    distsCir =  dists7 <- cir(coords[, c("x", "y")], landscape = hab, maxRadius = 30,
                              minRadius = 0, returnDistances = TRUE,
                              returnIndices = TRUE, allowOverlap = TRUE),
    distsRings =  dists8 <- rings(loci = loci, landscape = hab, maxRadius = 30, minRadius = 0,
                                 allowOverlap = TRUE, returnIndices = TRUE)
  )

  #  for(i in 1:10) {
  nPix <- seq(100, 500, length.out = 5)
  nLoci <- seq(1, log(1000), length.out = 5)
  results <- list()

  for (numPix in nPix) {
    a <- as.character(numPix)
    results[[a]] <- list()
    for (numLoci in round(exp(nLoci))) {
      b <- as.character(numLoci)
      results[[a]][[b]] <- list()
      hab <- raster(extent(0, numPix, 0, numPix), res = 1)
      N <- numLoci
      coords <- cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                      y = stats::runif(N, xmin(hab), xmax(hab)))
      results[[a]][[b]] <- summary(microbenchmark(
        times = 1,
        #dfep = distanceFromEachPoint(coords[,c("x", "y"),drop = FALSE], landscape = hab),
        #cir = cir(coords=coords[,c("x", "y")], landscape = hab,
        #               minRadius = 0, returnDistances = TRUE, allowOverlap = TRUE),
        dfp  = distanceFromPoints(hab, coords[, c("x", "y"), drop = FALSE]),
        dfep20 = distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab),
        dfep20Cum = distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab, cumulativeFn = `+`),
        cir20 = cir(coords = coords[, c("x", "y")], landscape = hab, maxRadius = 20,
                    minRadius = 0, returnDistances = TRUE, allowOverlap = TRUE)
      ))
      #print(paste("numLoci =", numLoci, "numPix =", numPix))
    }
  }

  numPix <- 3300
  hab <- raster(extent(0, numPix, 0, numPix), res = 1)
  N <- 200#numLoci
  coords <- cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                  y = stats::runif(N, xmin(hab), xmax(hab)))
  a <- Sys.time()
  dfep20Cum <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab,
                                     cumulativeFn = `+`, maxDistance = 50)
  b <- Sys.time()
  cir20 <- cir(coords = coords[, c("x", "y")], landscape = hab, maxRadius = 50,
               minRadius = 0, returnDistances = TRUE, allowOverlap = TRUE)
  d <- Sys.time()

  idwRaster <- raster(hab)
  idwRaster[] <- dfep20Cum[, "val"]
  if (interactive()) Plot(idwRaster)

  cells <- cellFromXY(hab, cir20[, c("x", "y")])
  cir20DT <- data.table(cir20, cells, key = "cells")
  idw <- cir20DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  # min        lq      mean    median        uq       max neval
  # 376.16213 407.44622 437.55936 437.79256 482.09462 484.06806     6
  # 476.83388 553.83873 571.78810 562.29938 585.29849 690.15876     6
  # 26.33451  26.34125  30.87619  32.25658  33.34990  34.71832     6
  # 70.91884  70.97720  71.32302  71.19613  71.76523  71.88459     6
  #}
  # sum of idw
  cells <- cellFromXY(hab, dists7[, c("x", "y")])
  dists7DT <- data.table(dists7, cells, key = "cells")
  idw <- dists7DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  # sum of idw
  cells <- cellFromXY(hab, dists6[, c("x", "y")])
  dists6DT <- data.table(dists6, cells, key = "cells")
  idw <- dists6DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw

  cells <- cellFromXY(hab, dists7[, c("x", "y")])
  dists7DT <- data.table(dists7, cells, key = "cells")
  idw <- dists7DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRasCir <- raster(hab)
  distsRasCir[] <- 0
  distsRasCir[idw$cells] <- idw$idw

  dists8DT <- data.table(dists8, key = "indices")
  idw <- dists8DT[, list(idw = sum(1 / sqrt(1 + dists))), by = indices]
  distsRasRings <- raster(hab)
  distsRasRings[] <- 0
  distsRasRings[idw$indices] <- idw$idw

  distDiff1 <- round(distsRas - distsRasCir, 2)
  tabs <- table(getValues(distDiff1))
  expect_true(tabs[names(tabs) == 0] / ncell(distDiff1) > 0.93) # This  tests that the two approaches are 93% similar

  if (interactive()) {
    Plot(distsRasCir, distsRasRings, distsRas, dists3, new = TRUE)
    Plot(distDiff1, zero.color = "white", new = TRUE)
  }
  sum(abs(getValues(distsRasCir - distsRasRings)))

    hab <- raster(extent(0, 1e3, 0, 1e3), res = 1, val = 0)
  N <- 10
  coords <- cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                  y = stats::runif(N, xmin(hab), xmax(hab)))
  coords <- cbind(coords, id = cellFromXY(hab, coords))

  microbenchmark(times = 2,
                 d1 = dists10 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 10),
                 d2 = dists30 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 30),
                 d3 =  dists7 <- cir(landscape = hab, coords = coords[, c("x", "y")],
                                     maxRadius = 30, minRadius = 0, returnDistances = TRUE,
                                     allowOverlap = TRUE),
                 d4 = dists50 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 300),
                 d5 = distsDFP <- distanceFromPoints(xy = coords[, c("x", "y")], object = hab)
  )

  # sum of idw
  cells <- cellFromXY(hab, dists50[, c("x", "y")])
  dists50DT <- data.table(dists50, cells, key = "cells")
  idw <- dists50DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]

  #idw <- dists1DT[, list(sumID = sum(id)), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  #Unit: milliseconds
  #          expr      min       lq     mean   median       uq      max neval
  #distsDFEP10Pts 856.8681 1058.968 1131.578 1165.994 1225.173 1246.662    10


  #
  head(dists5)
  distsRas <- raster(hab)
  distsRas[] <- dists5[, "dists"]
  if (interactive()) Plot(distsRas, dists3, new = TRUE)

  distsRas1 <- raster(hab)
  indices <- cellFromXY(hab, dists1[, c("x", "y")])
  invDist <- tapply(dists1[, "dists"], indices, function(x) sum(1 / (1 + x)))
  distsRas1[] <- as.vector(invDist)
  if (interactive()) Plot(distsRas1)

  dists5b <- do.call(rbind, dists5)
  all.equal(dists1b, dists3[, 1:4])

  dists4b <- do.call(rbind, dists4)
  all.equal(dists4b, dists1b[, c(1, 3:4)])

  library(microbenchmark)
  microbenchmark(times = 100,
                 cirs <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius,
                             simplify = TRUE, includeBehavior = "excludePixels"),
                 cirs2 <- {
                   loci <- cellFromXY(hab, coordinates(caribou))
                   cirs2 <- rings(hab, loci, minRadius = radius, maxRadius = radius * 1.5)
                 })


  loci <- cellFromXY(hab, coordinates(caribou))
  radius <- 15
  N <- 10
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5))
  microbenchmark(times = 200,
                 cirs2 <- rings(hab, loci, minRadius = 0, maxRadius = radius, returnIndices = TRUE, allowOverlap = FALSE),
                 aNoDists = cir(hab, coords = coordinates(caribou), allowOverlap = FALSE, returnDistances = FALSE,
                                maxRadius = radius, minRadius = 0, includeBehavior = "includePixels"),
                 aDists = cir(hab, coords = coordinates(caribou),
                              allowOverlap = FALSE, returnDistances = TRUE,
                              maxRadius = radius, minRadius = 0, includeBehavior = "includePixels",
                              returnAngles = TRUE)
  )

  #Unit: milliseconds
  #expr           min        lq      mean    median        uq       max neval
  #cirs2    25412.453 25755.583 33040.352 26688.340 36125.005  87270.60   200
  #aNoDists   934.076  954.4595  1098.388   963.697  975.8685  8149.194   400
  #aDists    1617.404 1639.2535  1855.630  1656.263 1694.3885  9116.704   400

  microbenchmark(times = 200,
                 noOverlap = cir(hab, coords = coordinates(caribou),
                                 allowOverlap = FALSE, returnDistances = FALSE,
                                 maxRadius = radius, minRadius = 0, includeBehavior = "includePixels",
                                 returnAngles = TRUE),

                 yesOverlap = cir(hab, coords = coordinates(caribou),
                                  allowOverlap = TRUE, returnDistances = FALSE,
                                  maxRadius = radius, minRadius = 0, includeBehavior = "includePixels",
                                  returnAngles = TRUE)
  )
  # for profvis
  for (i in 1:600) {
    cir(hab, coords = coordinates(caribou),
        allowOverlap = FALSE, returnDistances = FALSE,
        maxRadius = radius, minRadius = 0, includeBehavior = "includePixels",
        returnAngles = FALSE)
  }

  TEST <- TRUE
  count <- 0
  tmp <- data.frame(len = numeric(), size1 = numeric(), j = numeric(), oneClump = logical())
  while (TEST) {
    size1 <- sample(1.1 ^ (2:10 * 8), 1)
    hab <- raster(extent(0, size1, 0, size1), res = 1)
    N <- 1
    radius <- ncol(hab) / 5
    caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                                            y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5))
    count <- count + 1
    seed <- sample(1e6, 1)
    set.seed(seed)
    tmp1 <- capture.output(cirs <- cir(hab, caribou, maxRadius = radius, minRadius = 0, simplify = TRUE,
                                       includeBehavior = "excludePixels"))
    tmp[count, 1] <- as.numeric(strsplit(tmp1, split = " ")[[1]][2])
    ras1 <- raster(hab)
    ras1[] <- 1
    cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
    ras1[cirsOverlap$indices] <- 0 #cirsOverlap$sumIDs
    ras1Clump <- clump(ras1)
    if (interactive()) Plot(ras1, ras1Clump, new = TRUE)
    smallerEx <- extent(ras1) - 2
    ras1ClumpSm <- crop(ras1Clump, smallerEx)
    tmp[count, 2:4] <- c(size1, j, all(table(getValues(ras1ClumpSm)) > 2))
    #expect_true(all(table(getValues(ras1ClumpSm)) > 2))
    TEST <- all(table(getValues(ras1ClumpSm)) > 2)
    #print(count)
  }
  if (interactive()) Plot(ras1, ras1Clump, new = TRUE)

  tmpDT <- data.table(tmp)
  setkey(tmpDT, oneClump)
  tmpDT[, min(len, na.rm = TRUE), by = oneClump]
  setkey(tmpDT, oneClump, len)
  tmpDT[oneClump == 0]
})

test_that("simple cir does not work correctly", {
  set.seed(1234)
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(fpCompare); on.exit(detach("package:fpCompare"), add = TRUE)

  hab <- raster(extent(0, 1e1, 0, 1e1), res = 1)

  circleRas <- cir(hab, maxRadius = 1, includeBehavior = "excludePixels")
  expect_true(NROW(circleRas) == 4)
  expect_true(all(circleRas[, "indices"] == c(35, 44, 55, 46)))
  expect_true(all(mean(circleRas[, "x"]) == (ncol(hab) / 2 - 0.5)))
  expect_true(all(mean(circleRas[, "y"]) == (nrow(hab) / 2 + 0.5)))

  N <- 1
  coords <- cbind(x1 = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                  y1 = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5)
  expect_error(cir(hab, coords = coords), "coords must have columns named x and y")

  # test id column in coords
  N <- 2
  coords <- cbind(x = (stats::runif(N, xmin(hab) + 0.5, xmax(hab) - 0.5)),
                  y = (stats::runif(N, xmin(hab) + 0.5, xmax(hab) - 0.5)),
                  id = c(45, 56))
  cirs <- cir(hab, coords = coords, maxRadius = 1, minRadius = 0,
              includeBehavior = "includePixels", returnIndices = TRUE)
  expect_true(all(unique(cirs[, "id"]) == c(45, 56)))
  expect_true(all(distanceFromEachPoint(coords, cirs)[, "dists"] %<=% 1))

  # test closest
  N <- 1
  coords <- cbind(x = c(5, 6), y = c(5, 5))
  cirsClosestT <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
                      includeBehavior = "includePixels", closest = TRUE,
                      returnIndices = TRUE, allowOverlap = FALSE)
  cirsClosestF <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
                      includeBehavior = "includePixels", closest = FALSE,
                      returnIndices = TRUE, allowOverlap = FALSE)
  expect_true(all(table(cirsClosestF[, "id"]) == c(17, 4)))
  expect_true(all(table(cirsClosestT[, "id"]) - table(cirsClosestF[, "id"]) == c(-5, 5)))

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
               includeBehavior = "includePixels", closest = FALSE,
               returnIndices = FALSE, allowOverlap = FALSE, returnDistances = FALSE)
  expect_is(cirs2, "Raster")
  expect_true(max(getValues(cirs2)) == 2)
  expect_true(min(getValues(cirs2)) == 0)

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
               includeBehavior = "includePixels", closest = FALSE,
               returnIndices = FALSE, allowOverlap = TRUE, returnDistances = FALSE)
  expect_is(cirs2, "Raster")
  #expect_true(max(getValues(cirs2)) == 3)
  expect_true(min(getValues(cirs2)) == 0)

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0, includeBehavior = "includePixels",
               closest = FALSE, returnIndices = FALSE, allowOverlap = TRUE, returnDistances = TRUE)
  expect_is(cirs2, "Raster")
  #expect_true(max(getValues(cirs2)) < 2.82843)
  expect_true(min(getValues(cirs2)) == 0)

  hab <- raster(extent(0, 1e1, 0, 1e1), res = c(1, 2))
  expect_error(cir(hab, maxRadius = 1, includeBehavior = "excludePixels"),
               "cir function only accepts rasters with identical resolution in x and y dimensions")

  hab <- raster(extent(0, 1e1, 0, 1e1), res = 1)
  expect_error(cir(hab, maxRadius = 1, includeBehavior = "excludeRings"),
               "includeBehavior can only be \"includePixels\" or \"excludePixels\"")
})

test_that("wrap does not work correctly", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  xrange <- yrange <- c(-50, 50)
  hab <- raster(extent(c(xrange, yrange)))
  hab[] <- 0

  # initialize caribou agents
  N <- 10

  # previous points
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  # initial points, outside of range
  starts <- cbind(x = stats::runif(N, xrange[1] - 10, xrange[1]),
                  y = stats::runif(N, yrange[1] - 10, yrange[1]))

  expect_false(all(wrap(starts, bounds = extent(hab)) == starts))
  expect_false(all(wrap(starts, bounds = hab) == starts))
  expect_false(all(wrap(starts, bounds = bbox(hab)) == starts))
  expect_error(wrap(starts, bounds = starts),
               "Must use either a bbox, Raster\\*, or Extent for 'bounds'")

  # create spdf
  spdf <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
  expect_true(all(coordinates(wrap(spdf, bounds = hab)) == wrap(starts, hab)))
  expect_true(all(coordinates(wrap(spdf, bounds = hab, withHeading = FALSE)) == wrap(starts, hab)))
  expect_true(all(coordinates(wrap(spdf, bounds = bbox(hab), withHeading = FALSE)) == wrap(starts, hab)))
  expect_error(wrap(spdf, bounds = starts, withHeading = FALSE),
               "Must use either a bbox, Raster\\*, or Extent for 'bounds'")

  # errrors
  starts <- cbind(x1 = stats::runif(N, xrange[1] - 10, xrange[1]),
                  y = stats::runif(N, yrange[1] - 10, yrange[1]))
  spdf <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
  expect_error(wrap(spdf, bounds = extent(hab)),
               "When X is a matrix, it must have 2 columns, x and y,")
})

test_that("cir angles arg doesn't work", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  library(fpCompare); on.exit(detach("package:fpCompare"), add = TRUE)
  Ras <- raster(extent(0, 100, 0, 100), res = 1)
  Ras[] <- 0
  N <- 2
  coords <- cbind(x = stats::runif(N, xmin(Ras), xmax(Ras)),
                  y = stats::runif(N, xmin(Ras), xmax(Ras)))
  angles <- seq(0, 2 * pi, length.out = 21)[-21]
  circ <- cir(Ras, coords, angles = angles, maxRadius = 3, minRadius = 0,
              returnIndices = TRUE, allowOverlap = TRUE, returnAngles = TRUE)
  anglesTab <- table(circ[, "angles"])
  expect_true(all(as.numeric(names(anglesTab)) %==% angles))
  expect_true(all(length(anglesTab) == (length(angles))))

  skip("microbenchmarking below this")

  library(microbenchmark); on.exit(detach("package:microbenchmark"), add = TRUE)
  Ras <- raster(extent(0, 330, 0, 330), res = 1)
  Ras[] <- 0
  N <- 1e3
  coords <- cbind(x = stats::runif(N, xmin(Ras), xmax(Ras)),
                  y = stats::runif(N, xmin(Ras), xmax(Ras)))
  angles <- seq(0, 2 * pi, length.out = 21)[-21]
  newWay <- FALSE
  microbenchmark(times = 100,
  circ <- cir(Ras, coords, angles = angles,
              maxRadius = 3, minRadius = 0, returnIndices = TRUE,
              allowOverlap = TRUE, returnAngles = TRUE)
  )
  #min       lq     mean   median       uq      max neval
  #65.16964 76.17871 100.7652 90.87756 118.5901 390.5539   100

  newWay <- TRUE
  microbenchmark(times = 100,
                 circ <- cir(Ras, coords, angles = angles,
                             maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                             allowOverlap = TRUE, returnAngles = TRUE)
  )
  #     min       lq     mean   median       uq      max neval
  #22.44104 26.70138 32.47423 30.65686 35.89569 45.72201    10

  newWay <- FALSE; circOW <- cir(Ras, coords, angles = angles,
                                 maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                                 allowOverlap = TRUE, returnAngles = TRUE)
  newWay <- TRUE; circNW <- cir(Ras, coords, angles = angles,
                                maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                                allowOverlap = TRUE, returnAngles = TRUE)
})

test_that("multi-core version of distanceFromEachPoints does not work correctly", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (interactive()) {
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    library(parallel);

    hab <- randomPolygons(raster(extent(0, 1e2, 0, 1e2)), res = 1)

    # evaluate cumulativeFn
    N <- 50
    coords <- cbind(x = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5,
                    y = round(stats::runif(N, xmin(hab), xmax(hab))) + 0.5)
    dfep <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                  landscape = hab, cumulativeFn = `+`)

    ## using parallel package cluster
    system.time({
      cl1 <- makeCluster(1, rscript_args = "--vanilla --no-environ")
      clusterEvalQ(cl1, {library(SpaDES)})
    })
    system.time(
      dfepCluster <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                           landscape = hab, cumulativeFn = `+`,
                                           cl = cl1)
    )
    stopCluster(cl1)
    expect_true(all.equal(dfep, dfepCluster))

    ## using raster package cluster
    system.time({
      beginCluster(1)
    })
    system.time(
      dfepCluster2 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                            landscape = hab, cumulativeFn = `+`)
    )
    endCluster()
    expect_true(all.equal(dfep, dfepCluster2))
  }
})

test_that("spreadProb with relative values does not work correctly", {
  library(raster)
  seed <- 64350
  set.seed(seed)
  emptyRas <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- randomPolygons(emptyRas, numTypes = 40)
  names(hab) <- "hab"

  hab3 <- (hab > 20) * 200 + 1
  sam <- sample(which(hab3[] == 1), 1)
  set.seed(seed)
  events1 <- spread(hab3, spreadProb = hab3, loci = sam, directions = 8,
                    neighProbs = c(0, 1), maxSize = c(100), exactSizes = TRUE)

  # Compare to absolute probability version
  set.seed(seed)
  events2 <- spread(hab3, id = TRUE, loci = sam, directions = 8,
                    neighProbs = c(0, 1), maxSize = c(100), exactSizes = TRUE)

  # many more high value hab pixels spread to in event1
  expect_true(sum(hab3[events1[] > 0]) > sum(hab3[events2[] > 0]))
})
