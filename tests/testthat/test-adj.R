test_that("adj results identical to adjacent", {
  a <- raster::raster(raster::extent(0, 1e3, 0, 1e3), res=1)

  # smaller sample (should use matrix)
  s <- sample(1:length(a), 3)
  expect_equal(sum(adj(a, s, directions=8, sort=TRUE, match.adjacent=TRUE) -
                     adjacent(a, s, directions=8, sorted=TRUE)), 0)

  # larger sample (should use data.table)
  s <- sample(1:length(a), 1e5)
  expect_equal(sum(adj(a, s, directions=8) - adjacent(a, s, directions=8)), 0)
})
