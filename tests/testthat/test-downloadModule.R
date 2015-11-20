test_that("downloadModule downloads and unzips module files", {
  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  f <- downloadModule("LccToBeaconsReclassify", tmpdir)

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, "LccToBeaconsReclassify")), 0)
})

test_that("downloadData downloads and unzips module data", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  ## temporarily skip check on non-Windows because checksums for certain files
  ## are different due to different line-endings. See issue #230.
  if (Sys.info()[["sysname"]] == "Windows") {
    testthat::skip("Not on Windows.")
  }

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  moduleName <- "forestAge"
  filename <- "can_age04_1km.tif"
  m <- downloadModule(moduleName, tmpdir)
  d <- downloadData(moduleName, tmpdir)
  result <- checksums(moduleName, tmpdir)$result
  expect_true(file.exists(file.path(tmpdir, moduleName, "data", filename)))

  expect_true(result == "OK")
})
