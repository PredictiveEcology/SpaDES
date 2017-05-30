library(raster)

a <- raster(extent(0, 10 , 0, 10), res = 1)
sams <- sort(sample(ncell(a), 3))

# Simple use -- similar to spread(...)
out <- spread2(a, start = sams, 0.225)
if(interactive()) {
  clearPlot()
  Plot(out)
}

# Use maxSize -- this gives an upper limit
maxSizes <- sort(sample(1:10, size = length(sams)))
out <- spread2(a, start = sams, 0.225, maxSize = maxSizes, asRaster=FALSE)
# check TRUE using data.table .N
out[,.N,by="initialPixels"]$N <= maxSizes

# Use exactSize -- gives an exact size, if there is enough space on the Raster
exactSizes <- maxSizes
out <- spread2(a, start = sams, spreadProb = 0.225,
                exactSize = exactSizes, asRaster=FALSE)
out[,.N,by="initialPixels"]$N == maxSizes # should be TRUE TRUE TRUE

# Use exactSize -- but where it can't be achieved
exactSizes <- sort(sample(100:110, size = length(sams)))
out <- spread2(a, start = sams, 1, exactSize = exactSizes)

# Iterative calling -- create a function with a high escape probability
spreadWithEscape <- function(ras, start, escapeProb, spreadProb) {
  out <- spread2(ras, start = sams, spreadProb = escapeProb, asRaster = FALSE)
  while(any(out$state=="sourceActive")) {
    # pass in previous output as start
    out <- spread2(ras, start = out, spreadProb = spreadProb,
                    asRaster = FALSE, skipChecks = TRUE) # skipChecks for speed
  }
  out
}

set.seed(421)
out1 <- spreadWithEscape(a, sams, escapeProb = 0.25, spreadProb = 0.225)
set.seed(421)
out2 <- spread2(a, sams, 0.225, asRaster = FALSE)
# The one with high escape probability is larger (most of the time)
NROW(out1)>NROW(out2)


## Use neighProbs, with a spreadProb that is a RasterLayer
# Create a raster of different values, which will be the relative probabilities
#   i.e., they are rescaled to relative probabilities within the 8 neighbour choices.
#   The neighProbs below means 70% of the time, 1 neighbour will be chosen,
#   30% of the time 2 neighbours.
#   The cells with spreadProb of 5 are 5 times more likely than cells with 1 to be chosen,
#   when they are both within the 8 neighbours
sp <- raster(extent(0,3,0,3), res = 1, vals = 1:9) #small raster, simple values
# Check neighProbs worked
out <- list()
for(i in 1:100) { # enough replicates to see stabilized probabilities
  out[[i]] <- spread2(sp, spreadProbRel = sp, spreadProb = 1,
                      start = 5, iterations = 1,
                      neighProbs = c(1), asRaster = FALSE)
}
out <- data.table::rbindlist(out)[pixels!=5] # remove starting cell
table(sp[out$pixels])
# should be non-significant -- note no 5 because that was the starting cell
#  This tests whether the null model is true ... there should be proportions
#  equivalent to 1:2:3:4:6:7:8:9 ... i.e,. cell 9 should have 9x as many events
#  spread to it as cell 1. This comes from sp object above which is providing
#  the relative spread probabilities
keep <- c(1:4,6:9)
chisq.test(keep, unname(tabulate(sp[out$pixels],9)[keep]),
           simulate.p.value = TRUE)


## Example showing asymmetry
sams <- ncell(a)/4 - ncol(a)/4*3
circs <- spread2(a, spreadProb = 0.213, start = sams,
                  asymmetry = 2, asymmetryAngle = 135,
                  asRaster = TRUE)

# ADVANCED: Estimate spreadProb when using asymmetry, such that the expected event size is the same
#   as without using asymmetry
ras <- raster(a)
ras[] <- 1
if(interactive()) {
  N = 100
  sizes <- integer(N)
  for(i in 1:N) {
    circs <- spread2(ras, spreadProb = 0.225, start = round(ncell(ras)/4 - ncol(ras)/4*3),
                      asRaster = FALSE)
    sizes[i] <- circs[,.N]
  }
  goalSize <- mean(sizes)


  library(parallel)
  library(DEoptim)
  cl <- makeCluster(pmin(10, detectCores()-2)) # only need 10 cores for 10 populations in DEoptim
  parallel::clusterEvalQ(cl, {
    library(SpaDES)
    library(raster)
    library(fpCompare)
  })

  objFn <- function(sp, N = 20, ras, goalSize) {
    sizes <- integer(N)
    for(i in 1:N) {
      circs <- spread2(ras, spreadProb = sp, start = ncell(ras)/4 - ncol(ras)/4*3,
                        asymmetry = 2, asymmetryAngle = 135,
                        asRaster = FALSE)
      sizes[i] <- circs[,.N]
    }
    abs(mean(sizes) - goalSize)
  }
  aa <- DEoptim(objFn, lower = 0.2, upper = 0.23,
                control = DEoptim.control(cluster = cl, NP = 10, VTR = 0.02,
                                          initialpop = as.matrix(rnorm(10, 0.213, 0.001))),
                ras = a, goalSize = goalSize)

  # The value of spreadProb that will give the same expected event sizes to spreadProb = 0.225 is:
  sp <- aa$optim$bestmem
  circs <- spread2(ras, spreadProb = sp, start = ncell(ras)/4 - ncol(ras)/4*3,
                    asymmetry = 2, asymmetryAngle = 135,
                    asRaster = FALSE)

  stopCluster(cl)
}
