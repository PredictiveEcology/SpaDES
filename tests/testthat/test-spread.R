test_that("spread produces legal RasterLayer", {
  set.seed(123)

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
