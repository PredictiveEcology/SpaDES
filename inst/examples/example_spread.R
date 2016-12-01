library(raster)
library(RColorBrewer)

# Make random forest cover map
emptyRas <- raster(extent(0, 1e2, 0, 1e2), res = 1)
hab <- randomPolygons(emptyRas, numTypes = 40)
names(hab) <- "hab"
mask <- raster(emptyRas)
mask <- setValues(mask, 0)
mask[1:5000] <- 1
numCol <- ncol(emptyRas)
numCell <- ncell(emptyRas)
directions <- 8

# Can use transparent as a color
setColors(hab) <- paste(c("transparent", brewer.pal(8, "Greys")))

# note speedup is equivalent to making pyramids, so, some details are lost
if (interactive()) {
  clearPlot()
  Plot(hab, speedup = 3)
}

# initiate 10 fires
startCells <- as.integer(sample(1:ncell(emptyRas),10))
fires <- spread(hab, loci = startCells, 0.235, persistence = 0, numNeighs = 2,
                mask=NULL, maxSize = 1e8, directions=8, iterations=1e6, id = TRUE)

#set colors of raster, including a transparent layer for zeros
setColors(fires, 10) <- c("transparent", brewer.pal(8, "Reds")[5:8])
if (interactive()) {
  Plot(fires)
  Plot(fires, addTo = "hab")

  #alternatively, set colors using cols= in the Plot function
  clearPlot()
  Plot(hab)
  Plot(fires) # default color range makes zero transparent.
  # Instead, to give a color to the zero values, use \code{zero.color=}
  Plot(fires, addTo = "hab",
       cols = colorRampPalette(c("orange", "darkred"))(10))
  hab2 <- hab
  Plot(hab2)
  Plot(fires, addTo = "hab2", zero.color = "white",
     cols = colorRampPalette(c("orange", "darkred"))(10))
  # or overplot the original (NOTE: legend stays at original values)
  Plot(fires, cols = topo.colors(10))
}

####################
## Continue event by passing interrupted object into spreadState
####################

## Interrupt a spread event using iterations - need returnIndices = TRUE to use outputs
##   as new inputs in next iteration
fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
                returnIndices = TRUE, 0.235, 0, NULL, 1e8, 8, iterations = 3, id = TRUE)
fires[, list(size = length(initialLocus)), by = id]  # See sizes of fires

fires2 <- spread(hab, loci = NA_real_, returnIndices = TRUE, 0.235, 0, NULL,
                 1e8, 8, iterations = 2, id = TRUE, spreadState = fires)
# NOTE events are assigned arbitrary IDs, starting at 1

## Add new fires to the already burning fires
fires3 <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
                 returnIndices = TRUE, 0.235, 0, NULL, 1e8, 8, iterations = 1,
                 id = TRUE, spreadState = fires)
fires3[, list(size = length(initialLocus)), by = id]  # See sizes of fires
# NOTE old ids are maintained, new events get ids begining above previous
# maximum (e.g., new fires 11 to 20 here)

## Use data.table and loci...
fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
                returnIndices = TRUE, 0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE)
fullRas <- raster(hab)
fullRas[] <- 1:ncell(hab)
burned <- fires[active == FALSE]
burnedMap <- rasterizeReduced(burned, fullRas, "id", "indices")
if (interactive()) {
  clearPlot()
  Plot(burnedMap)
}

####################
## stopRule examples
####################

# examples with stopRule, which means that the eventual size is driven by the values on the raster
#  passed in to the landscape argument
set.seed(1234)
stopRule1 <- function(landscape) sum(landscape) > 50
stopRuleA <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL,
                    maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1)

set.seed(1234)
stopRule2 <- function(landscape) sum(landscape) > 100
# using stopRuleBehavior = "excludePixel"
stopRuleB <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL,
                    maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule2,
                    stopRuleBehavior = "excludePixel")

# using stopRuleBehavior = "includeRing", means that end result is slightly larger patches, as a
#  complete "iteration" of the spread algorithm is used.
set.seed(1234)
stopRuleB_NotExact <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0,
                NULL, maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule2)
if (interactive()) {
  clearPlot()
  Plot(stopRuleA, stopRuleB, stopRuleB_NotExact)
}

# Test that the stopRules work
# stopRuleA was not exact, so each value will "overshoot" the stopRule, here it was hab>50
foo <- cbind(vals = hab[stopRuleA], id = stopRuleA[stopRuleA > 0]);
tapply(foo[, "vals"], foo[, "id"], sum) # Correct ... all are above 50

# stopRuleB was exact, so each value will be as close as possible while rule still is TRUE
#  Because we have discrete cells, these numbers will always slightly under the rule
foo <- cbind(vals = hab[stopRuleB], id = stopRuleB[stopRuleB > 0]);
tapply(foo[, "vals"], foo[, "id"], sum) # Correct ... all are above 50

# stopRuleB_notExact will overshoot
foo <- cbind(vals = hab[stopRuleB_NotExact], id = stopRuleB_NotExact[stopRuleB_NotExact > 0]);
tapply(foo[, "vals"], foo[, "id"], sum) # Correct ... all are above 50

# Cellular automata shapes
# Diamonds - can make them with: a boolean raster, directions = 4,
#    stopRule in place, spreadProb = 1
diamonds <- spread(hab > 0, spreadProb = 1, directions = 4, id = TRUE, stopRule = stopRule2)
if (interactive()) {
  clearPlot()
  Plot(diamonds)
}

# Squares - can make them with: a boolean raster, directions = 8,
#    stopRule in place, spreadProb = 1
squares <- spread(hab > 0, spreadProb = 1, directions = 8, id = TRUE, stopRule = stopRule2)
if (interactive()) Plot(squares)

# Interference shapes - can make them with: a boolean raster, directions = 8,
#    stopRule in place, spreadProb = 1
stopRule2 <- function(landscape) sum(landscape) > 200
squashedDiamonds <- spread(hab > 0, spreadProb = 1,
                           loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                           directions = 4, id = TRUE, stopRule = stopRule2)
if (interactive()) {
  clearPlot()
  Plot(squashedDiamonds)
}

# Circles with spreadProb < 1 will give "more" circular shapes, but definitely not circles
stopRule2 <- function(landscape) sum(landscape) > 200
seed <- sample(1e4, 1)
set.seed(seed)
circlish <- spread(hab > 0, spreadProb = 0.23,
                   loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                   directions = 8, id = TRUE, circle = TRUE)#, stopRule = stopRule2)
set.seed(seed)
regularCA <- spread(hab > 0, spreadProb = 0.23,
                    loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                    directions = 8, id = TRUE)#, stopRule = stopRule2)
if (interactive()) {
  clearPlot()
  Plot(circlish, regularCA)
}

####################
# complex stopRule
####################

initialLoci <- sample(seq_len(ncell(hab)), 2)
endSizes <- seq_along(initialLoci) * 200

# Can be a function of landscape, id, and/or any other named
#   variable passed into spread
stopRule3 <- function(landscape, id, endSizes) sum(landscape) > endSizes[id]

TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                             directions = 8, id = TRUE, stopRule = stopRule3,
                             endSizes = endSizes, stopRuleBehavior = "excludePixel")

# or using named list of named elements:
# TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
#                              directions = 8, id = TRUE, stopRule = stopRule3,
#                              vars = list(endSizes = endSizes), stopRuleBehavior = "excludePixel")

if (interactive()) {
  clearPlot()
  Plot(TwoCirclesDiffSize)
}
cirs <- getValues(TwoCirclesDiffSize)
vals <- tapply(hab[TwoCirclesDiffSize], cirs[cirs > 0], sum)

# Stop if sum of landscape is big or mean of quality is too small
quality <- raster(hab)
quality[] <- runif(ncell(quality), 0, 1)
stopRule4 <- function(landscape, quality, cells) {
  (sum(landscape) > 20) | (mean(quality[cells]) < 0.3)
}

TwoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                             directions = 8, id = TRUE, stopRule = stopRule4,
                             quality = quality, stopRuleBehavior = "excludePixel")

##############
# allowOverlap
##############
set.seed(3113)
initialLoci <- as.integer(sample(1:ncell(hab), 10))

# using "landscape", "id", and a variable passed in
maxVal <- rep(500, length(initialLoci))

# define stopRule
stopRule2 <- function(landscape, id, maxVal) sum(landscape) > maxVal[id]
circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci, stopRule = stopRule2,
                id = TRUE, allowOverlap = TRUE, stopRuleBehavior = "includeRing",
                maxVal = maxVal, returnIndices = TRUE)
(vals <- tapply(hab[circs$indices], circs$id, sum))
vals <= maxVal ## all TRUE
overlapEvents <- raster(hab)
overlapEvents[] <- 0
toMap <- circs[, sum(id), by = indices]
overlapEvents[toMap$indices] <- toMap$V1

if (interactive()) {
  clearPlot()
  Plot(overlapEvents)
}


## Using alternative algorithm, not probabilistic diffusion
## Will give exactly correct sizes, yet still with variability
## within the spreading (i.e., cells with and without successes)
dev();
seed <- sample(1e6,1)
seed <- 651883
set.seed(seed); print(seed)
maxSizes <- rlnorm(10, meanlog = 3.5, sdlog = 2)
fires <- spread(hab, loci = startCells, 1, persistence = 0,
                neighProbs = c(0.5, 0.5, 0.5)/1.5,
                mask=NULL, maxSize = maxSizes, directions=8,
                iterations=1e6, id = TRUE, plot.it = FALSE, exactSizes = TRUE);
table(fires[fires>0][]) < floor(maxSizes);
Plot(fires, new=TRUE, cols = c("red", "yellow"),
                              zero.color = "white")
