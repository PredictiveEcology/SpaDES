test_that("adj results identical to adjacent", {
  a <- raster::raster(raster::extent(0, 1e3, 0, 1e3), res=1)

  # smaller sample (should use matrix)
  s <- sample(1:length(a), 3)

  expect_equal(adj(a, s, directions=4, sort=TRUE, match.adjacent=TRUE),
                     adjacent(a, s, directions=4, sorted=TRUE))

  expect_equal(adj(a, s, directions=8, sort=TRUE, match.adjacent=TRUE),
                     adjacent(a, s, directions=8, sorted=TRUE))

  expect_equal(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=TRUE),
                     adjacent(a, s, directions="bishop", sorted=TRUE))


  expect_equal(adj(a, s, directions=4, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE),
                     adjacent(a, s, directions=4, sorted=TRUE,
                              include=TRUE))

  expect_equal(adj(a, s, directions=8, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE),
                     adjacent(a, s, directions=8, sorted=TRUE,
                              include=TRUE))

  expect_equal(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=TRUE,
                       include=TRUE),
                     adjacent(a, s, directions="bishop", sorted=TRUE,
                              include=TRUE))

  # test match.adjacent - it is just a different order
  # gets ths same cells
  expect_equal(sum(adj(a, s, directions=4, sort=FALSE, match.adjacent=FALSE) -
                     adjacent(a, s, directions=4, sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions=4, sort=FALSE, match.adjacent=FALSE)-
                     adjacent(a, s, directions=4, sorted=FALSE))^2),0)

  # test match.adjacent - primarily for directions=4, or bishop
  # gets ths same cells
  expect_equal(sum(adj(a, s, directions="bishop", sort=FALSE, match.adjacent=FALSE) -
                     adjacent(a, s, directions="bishop", sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions="bishop", sort=FALSE, match.adjacent=FALSE)-
                          adjacent(a, s, directions="bishop", sorted=FALSE))^2),0)

  # gets ths same cells
  expect_equal(sum(adj(a, s, directions=8, sort=FALSE, match.adjacent=FALSE) -
                     adjacent(a, s, directions=8, sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions=8, sort=FALSE, match.adjacent=FALSE)-
                          adjacent(a, s, directions=8, sorted=FALSE))^2),0)


  # Test include=TRUE
  expect_equal(sum(adj(a, s, directions=4, sort=TRUE, match.adjacent=FALSE,
                       include=TRUE) -
                     adjacent(a, s, directions=4, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions=8, sort=TRUE, match.adjacent=FALSE,
                       include=TRUE) -
                     adjacent(a, s, directions=8, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=FALSE,
                       include=TRUE) -
                     adjacent(a, s, directions="bishop", sorted=TRUE,
                              include=TRUE)), 0)

  # Include=TRUE with match.adjacent=TRUE
  expect_equal(sum(adj(a, s, directions=4, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE) -
                     adjacent(a, s, directions=4, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions=8, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE) -
                     adjacent(a, s, directions=8, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=TRUE,
                       include=TRUE) -
                     adjacent(a, s, directions="bishop", sorted=TRUE,
                              include=TRUE)), 0)
################################ data.table portion
  # larger sample (should use data.table)
  s <- sample(1:length(a), 3)

  expect_equal(adj(a, s, directions=4, sort=FALSE, match.adjacent=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions=4, sorted=FALSE))

  expect_equal(adj(a, s, directions=8, sort=FALSE, match.adjacent=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions=8, sorted=FALSE))

  expect_equal(adj(a, s, directions="bishop", sort=FALSE, match.adjacent=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions="bishop", sorted=FALSE))


  expect_equal(adj(a, s, directions=4, sort=FALSE, match.adjacent=TRUE,
                   include=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions=4, sorted=FALSE,
                        include=TRUE))

  expect_equal(adj(a, s, directions=8, sort=FALSE, match.adjacent=TRUE,
                   include=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions=8, sorted=FALSE,
                        include=TRUE))

  expect_equal(adj(a, s, directions="bishop", sort=FALSE, match.adjacent=TRUE,
                   include=TRUE,cutoff.for.data.table = 2),
               adjacent(a, s, directions="bishop", sorted=FALSE,
                        include=TRUE))

  # test match.adjacent - it is just a different order
  # gets ths same cells
  expect_equal(sum(adj(a, s, directions=4, sort=FALSE, match.adjacent=FALSE,
                       cutoff.for.data.table = 2
                       ) -
                     adjacent(a, s, directions=4, sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions=4, sort=FALSE, match.adjacent=FALSE,
                            cutoff.for.data.table = 2)-
                          adjacent(a, s, directions=4, sorted=FALSE))^2),0)

  # test match.adjacent - primarily for directions=4, or bishop
  # gets ths same cells
  expect_equal(sum(adj(a, s, directions="bishop", sort=FALSE, match.adjacent=FALSE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions="bishop", sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions="bishop", sort=FALSE, match.adjacent=FALSE,
                            cutoff.for.data.table = 2)-
                          adjacent(a, s, directions="bishop", sorted=FALSE))^2),0)

  # gets ths same cells
  expect_equal(sum(adj(a, s, directions=8, sort=FALSE, match.adjacent=FALSE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions=8, sorted=FALSE)), 0)
  # but not in the same order
  expect_more_than(sum((adj(a, s, directions=8, sort=FALSE, match.adjacent=FALSE,
                            cutoff.for.data.table = 2)-
                          adjacent(a, s, directions=8, sorted=FALSE))^2),0)


  # Test include=TRUE
  expect_equal(sum(adj(a, s, directions=4, sort=TRUE, match.adjacent=FALSE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions=4, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions=8, sort=TRUE, match.adjacent=FALSE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions=8, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=FALSE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions="bishop", sorted=TRUE,
                              include=TRUE)), 0)

  # Include=TRUE with match.adjacent=TRUE
  expect_equal(sum(adj(a, s, directions=4, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions=4, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions=8, sort=TRUE, match.adjacent=TRUE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions=8, sorted=TRUE,
                              include=TRUE)), 0)

  expect_equal(sum(adj(a, s, directions="bishop", sort=TRUE, match.adjacent=TRUE,
                       include=TRUE,
                       cutoff.for.data.table = 2) -
                     adjacent(a, s, directions="bishop", sorted=TRUE,
                              include=TRUE)), 0)



  Ras <- raster(extent(0,15,0,15), res=1)
  Ras <- randomPolygons(Ras, numTypes=4, speedup=1, p=0.3)
  N <- 2
  caribou <- SpatialPoints(coords=cbind(x=runif(N,xmin(Ras),xmax(Ras)),
                                        y=runif(N,xmin(Ras),xmax(Ras))))
  cirs <- cir(caribou, rep(3,length(caribou)), fullRas, simplify=TRUE)
  expect_is(cirs, "data.table")
#   expect_equal(sum(adj(a, s, directions=4) - adjacent(a, s, directions=4)), 0)
#   expect_equal(sum(adj(a, s, directions=8) - adjacent(a, s, directions=8)), 0)
#   expect_equal(sum(adj(a, s, directions="bishop") - adjacent(a, s, directions="bishop")), 0)
})
