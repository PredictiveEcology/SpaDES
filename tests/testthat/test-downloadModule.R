test_that("downloadModule downloads and unzips module files", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  f <- downloadModule("forestAge", tmpdir)

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, "forestAge")), 0)
})

test_that("downloadData downloads and unzips module data", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  moduleName <- "test"
  filenames <- c("DEM.tif", "habitatQuality.tif")
  m <- downloadModule(moduleName, tmpdir)
  t1 <- system.time(suppressMessages(downloadData(moduleName, tmpdir)))
  result <- suppressMessages(checksums(moduleName, tmpdir)$results)
  expect_true(all(file.exists(file.path(tmpdir, moduleName, "data", filenames))))
  expect_true(all(result == "OK"))

  # shouldn't need a redownload because file exists
  t2 <- system.time(suppressMessages(downloadData(moduleName, tmpdir)))
  expect_true(t1[3] > t2[3]) # compare elapsed times
})
