test_that("adj results identical to adjacent", {
  a <- raster(extent(0, 1e3, 0, 1e3), res=1)
  
  # smaller sample (should use matrix)
  s <- sample(1:length(a), 1e3)
  expect_that(identical(adj(a, s, directions=8),
                        adjacent(a, s, directions=8)), is_true())
  expect_that(identical(adj(a, s, directions=8, sort=TRUE),
                        adjacent(a, s, directions=8, sorted=TRUE)), is_true())

  # larger sample (should use data.table)
  s <- sample(1:length(a), 1e5)
  expect_that(identical(adj(a, s, directions=8),
                        adjacent(a, s, directions=8)), is_true())
  expect_that(identical(adj(a, s, directions=8, sort=TRUE),
                        adjacent(a, s, directions=8, sorted=TRUE)), is_true())  
})
