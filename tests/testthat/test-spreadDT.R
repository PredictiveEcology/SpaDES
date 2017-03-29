test_that("spreadDT tests", {
  library(raster)
  on.exit(detach("package:raster"), add = TRUE)
  library(data.table)
  on.exit(detach("package:data.table"), add = TRUE)
  library(fpCompare)
  on.exit(detach("package:fpCompare"), add = TRUE)

  # inputs for x
  a <- raster(extent(0, 10 , 0, 10), res = 1)
  b <- raster(a)
  b[] <- 1
  bb <-
    focal(
      b,
      matrix(1 / 9, nrow = 3, ncol = 3),
      fun = sum,
      pad = TRUE,
      padValue = 0
    )
  innerCells <- Which(bb %==% 1, cells = TRUE)

  # check it makes a RasterLayer
  set.seed(123)

  for (i in 1:20) {
    sams <- sample(innerCells, 2)
    out <- spreadDT(a, start = sams, 0.225, asRaster = FALSE)
    expect_true(length(unique(out$initialPixels)) == 2)
    expect_true(all(out$active == 0))
  }

  if (interactive())
    print("testing size")
  maxSizes <- 2:3
  for (i in 1:20) {
    seed <- sample(1e6, 1)
    set.seed(seed)
    sams <- sample(innerCells, 2)
    out <-
      spreadDT(a,
               start = sams,
               0.225,
               size = maxSizes,
               asRaster = FALSE)
    expect_true(all(out[, .N, by = "initialPixels"]$N <= maxSizes))
  }

  if (interactive())
    print("testing exact size")
  exactSizes <- c(5, 3)
  for (i in 1:20) {
    sams <- sample(innerCells, 2)
    out <-
      spreadDT(
        a,
        start = sams,
        0.225,
        size = exactSizes,
        exactSize = TRUE,
        asRaster = FALSE
      )
    expect_true(all(out[, .N, by = "initialPixels"]$N == exactSizes))
  }

  if (interactive())
    print("testing exact size, can't be achieved, allow jumping")
  exactSizes <-
    c(154, 111, 134) # too big for landscape, can't achieve it --
  #  will hit max numRetries, and will try jumping
  for (i in 1:20) {
    seed <- sample(1e6, 1)
    set.seed(seed)
    #print(seed)
    sams <- sample(innerCells, 3)
    out <-
      spreadDT(
        a,
        start = sams,
        0.225,
        size = exactSizes,
        exactSize = TRUE,
        asRaster = FALSE
      )
    expect_true(all(out[, .N, by = "initialPixels"]$N < exactSizes))
    expect_true(all(out$numRetries == 11)) # current max
  }

  if (interactive())
    print("test circle = TRUE")
  for (i in 1:20) {
    seed <- sample(1e6, 1)
    set.seed(seed)
    #print(seed)
    sams <- sample(innerCells, length(sams))
    expect_error(spreadDT(
      a,
      start = sams,
      0.225,
      circle = TRUE,
      asRaster = FALSE,
      plot.it = TRUE
    ))
    out <-
      spreadDT(a,
               start = sams,
               1,
               circle = TRUE,
               asRaster = FALSE)
    expect_true(is.numeric(out$distance))
    expect_true(NROW(out) == ncell(a))
  }

  # test circle
  out <- spreadDT(
    a,
    start = sams,
    1,
    circle = TRUE,
    asRaster = FALSE,
    returnDistances = TRUE
  )
  expect_true(NROW(out) == ncell(a))
  expect_true(all(out$state == "inactive"))
  expect_true(all(out$distance <= (sqrt(2) * ncol(a))))

  out <-
    spreadDT(
      a,
      start = sams,
      1,
      circle = TRUE,
      allowOverlap = TRUE,
      asRaster = FALSE,
      returnDistances = TRUE
    )
  expect_true(NROW(out) == ncell(a) * length(sams))
  expect_true(all(out$state == "inactive"))
  expect_true(all(out$distance <= (sqrt(2) * ncol(a))))
  setkey(out, id, distance)

  if (interactive()) {
    for (ids in unique(out$id)) {
      dev(3 + ids)
      ras <- raster(a)
      ras[out[id == ids, pixels]] <- out[id == ids, distance]
      clearPlot()
      Plot(ras)
    }
  }

  if (interactive())
    print("compare spreadDT circle with cir circle")
  cirOut <-
    data.table(
      cir(
        a,
        allowOverlap = TRUE,
        loci = sams,
        minRadius = 0,
        maxRadius = 15,
        returnDistances = T,
        simplify = TRUE
      )
    )
  if (interactive()) {
    for (ids in unique(cirOut$id)) {
      dev(3 + ids)
      ras[cirOut[id == ids, indices]] <- cirOut[id == ids, dists]
      clearPlot()
      Plot(ras)
    }
  }
  cirOut$dists <- round(cirOut$dists, 4)
  out$distance <- round(out$distance, 4)
  setkey(cirOut, id, dists)
  cirOut <- unique(cirOut)
  compare <- out[cirOut, on = c(id = "id", pixels = "indices")]
  expect_true(sum(abs(compare$dists - compare$distance)) %==% 0)


  if (interactive())
    print("Scales with number of starts, not size of raster")
  set.seed(21)
  b <- raster(extent(0, 33000 , 0, 33000), res = 1)
  sams <- sample(ncell(b), 2)
  st1 <- system.time(out <- spreadDT(
    b,
    start = sams,
    0.225,
    allowOverlap = TRUE,
    asRaster = FALSE
  ))
  expect_lt(st1[1], 1)


  if (interactive())
    print("test neighProbs")
  maxSizes <- 14
  sp <- raster(a)
  spreadProbOptions <- 1:5
  sp[] <- sample(spreadProbOptions, ncell(sp), replace = TRUE)
  set.seed(2123)
  sams <- sample(innerCells, 2)
  set.seed(321)
  out <- spreadDT(
    a,
    spreadProb = sp,
    start = sams,
    neighProbs = c(0.7, 0.3),
    size = maxSizes,
    asRaster = FALSE
  )
  expect_true(uniqueN(out) == maxSizes * length(sams))
  expect_true(NROW(out) == maxSizes * length(sams))


  if (interactive())
    print("check variable lengths of neighProbs")
  set.seed(29937)
  sams <- sample(innerCells, 2)
  for (i in 1:8) {
    alwaysN <- rep(0, i)
    alwaysN[i] <- 1
    out <- spreadDT(
      a,
      spreadProb = sp,
      iterations = 1,
      start = sams,
      neighProbs = alwaysN,
      asRaster = FALSE
    )
    expect_true(NROW(out) == (length(alwaysN) * 2 + length(sams)))
  }

  if (interactive())
    print(
      paste(
        "Test that when using neighProbs & a Raster of spreadProbs,",
        "the spreadProb raster is followed probabilistically",
        "This test does only 1 iteration from 2 pixels that are",
        "not interacting with edges or each other"
      )
    )
  sams <- sort(c(0:2 * 3 + 12) + rep(c(0, 30, 60, 90), 3))
  sams <- sams[sams < 90]
  #sams <- c(36, 79)
  set.seed(654)
  out <- list()
  for (i in 1:10) {
    #out[[i]] <- spreadDT(a, spreadProb = sp, iterations = 1,
    #                             start = sams, neighProbs = c(0.7,0.3), size = maxSizes, asRaster=FALSE)
    out[[i]] <- spreadDT(
      a,
      spreadProb = sp,
      iterations = 1,
      start = sams,
      neighProbs = c(1),
      asRaster = FALSE
    )
  }
  out <- rbindlist(out)[state == "activeSource"]
  uniquePixels <- out[, list(uniquePix = unique(pixels)), by = "id"]
  avail <- table(sp[uniquePixels$uniquePix])
  actual <- unname(table(sp[out$pixels]))
  relProbs <- spreadProbOptions / sum(spreadProbOptions)
  aa <- rmultinom(1, size = 1e4, prob = relProbs)[, 1] * unname(avail)
  #dev(4);barplot(aa)
  #dev(5);barplot(actual)
  suppressWarnings(cht <- chisq.test(x = cbind(aa, actual)))
  expect_true(cht$p.value > 0.05)

  #
  if (interactive())
    print("check wide range of spreadProbs")
  set.seed(654)
  temp <<- 1
  for (i in 1:20) {
    ras1 <- spreadDT(a, spreadProb = stats::runif(1, 0, 1))
    expect_that(ras1, is_a("RasterLayer"))
  }
  rm("temp", envir = .GlobalEnv)


  if (interactive())
    print("testing iterative calling of spreadDT")
  set.seed(299)
  sams <- sample(innerCells, 2)
  set.seed(299)
  out <- spreadDT(a,
                  iterations = 1,
                  start = sams,
                  asRaster = FALSE)
  stillActive <- TRUE
  while (stillActive) {
    stillActive <- any(out$state == "activeSource")
    out <- spreadDT(a,
                    iterations = 1,
                    start = out,
                    asRaster = FALSE)
  }

  set.seed(299)
  out2 <- spreadDT(a, start = sams, asRaster = FALSE)
  keyedCols <- c("id", "pixels")
  setkeyv(out2, keyedCols)
  setkeyv(out, keyedCols)
  expect_equivalent(out2, out)



  skip("benchmarking spreadDT")
  a <- raster(extent(0, 1000, 0, 1000), res = 1)
  set.seed(123)
  sams <- sample(innerCells, 30)
  set.seed(123)
  profvis::profvis({
    out <- spreadDT(a, start = sams, 0.235, asRaster = FALSE)
  })
  set.seed(123)
  profvis::profvis({
    out <- spreadDT(
      a,
      start = sams,
      0.235,
      asRaster = FALSE,
      allowOverlap = TRUE
    )
  })

  set.seed(123)
  microbenchmark(times = 30, {
    out1 <- spreadDT(a, start = sams, 0.235, asRaster = FALSE)
  },
  b = {
    out2 <- spread(a, loci = sams, 0.235, id = TRUE)
  },
  c = {
    out2 <- spread(a,
                   loci = sams,
                   0.235,
                   id = TRUE,
                   lowMemory = TRUE)
  })
  set.seed(123)
  profvis::profvis({
    out <- spreadDT(
      a,
      start = sams,
      0.235,
      asRaster = FALSE,
      allowOverlap = TRUE
    )
  })


### Benchmarking
  iterativeFun <- function(a, quick, N, sp) {
    sams <- sample(innerCells, N)
    out <-
      spreadDT(a,
               iterations = 1,
               start = sams,
               asRaster = FALSE, spreadProb = sp)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$state == "activeSource")
      out <-
        spreadDT(
          a, spreadProb = sp,
          iterations = 1,
          start = out,
          asRaster = FALSE,
          quick = quick
        )
    }
    out
  }

  nonIterativeFun <- function(a, quick, N, sp) {
    sams <- sample(innerCells, N)
    out <-
      spreadDT(a, start = sams, asRaster = FALSE, quick=quick, spreadProb = sp)
    out
  }

  origSpread <- function(a, quick, N, sp) {
    sams <- sample(innerCells, N)
    out <-
      spread(
        a,spreadProb = sp,
        loci = sams,
        id = TRUE,
        returnIndices = TRUE,
        quick = quick
      )
    out
  }

  origSpreadIterations <- function(a, quick, N, sp) {
    sams <- sample(innerCells, N)
    out <-
      spread(a, spreadProb = sp,
             iterations = 1,
             loci = sams,
             returnIndices = TRUE)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$active)
      out <-
        spread(
          a, spreadProb = sp,
          iterations = 1,
          spreadState = out,
          returnIndices = TRUE,
          quick = quick
        )
    }
    out
  }

  N <- 5
  ras <- raster(extent(0,1000, 0, 1000), res=1)
  sp <- 0.225
  microbenchmark(
    times = 100,
    iterativeFun(ras, TRUE, N, sp),
  #  iterativeFun(ras, FALSE, N, sp),
  #  nonIterativeFun(ras, TRUE, N, sp),
  #  nonIterativeFun(ras, FALSE, N, sp),
  #  origSpread(ras, TRUE, N, sp),
  #  origSpread(ras, FALSE, N, sp)
    origSpreadIterations(ras, TRUE, N, sp)
  #  origSpreadIterations(ras, FALSE, N, sp)
  )
  # Unit: milliseconds
  #                        expr       min        lq      mean    median         uq       max neval
  #          iterativeFun(TRUE)  5.992042 48.447037 69.718150 66.769768  88.546894 158.19727   100
  #         iterativeFun(FALSE) 12.448292 60.957684 85.318160 85.518054 102.162087 184.39568   100
  #       nonIterativeFun(TRUE)  6.943067 14.495293 21.647874 19.479788  26.892898  49.09266   100
  #           origSpread(FALSE)  3.611198  4.915029  6.195880  5.700317   6.737766  18.41116   200
  #            origSpread(TRUE)  3.304885  4.876557  6.113878  5.817736   6.729926  15.54314   100
  #  origSpreadIterations(TRUE)  7.686603 40.860350 70.656248 61.406394  96.669625 215.38094   100
  # origSpreadIterations(FALSE) 15.800219 54.305615 83.028381 77.198408 104.517771 208.91776   100

  profvis::profvis({
    for(i in 1:30)
    iterativeFun(ras, TRUE, N, sp=0.235)
  })
  profvis::profvis({
    nonIterativeFun()
  })

  # compare original spread and spreadDT -- seems pretty dead on
  NN <- 1000
  outNew <- out <- numeric(NN)
  for(i in 1:NN) {
    outNew[i] <- NROW(nonIterativeFun(ras, TRUE, N, sp))
    out[i] <- NROW(origSpread(ras, TRUE, N, sp))
  }

  library(ggplot2)
  out <- data.table(x=out)
  outNew <- data.table(x=outNew)
  ggplot(out, aes(x)) + geom_histogram() + geom_histogram(data=outNew, mapping=aes(x, fill="transparent"))
  mean(out$x)
  mean(outNew$x)
  sd(out$x)
  sd(outNew$x)

})
