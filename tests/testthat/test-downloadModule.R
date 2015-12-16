test_that("downloadModule downloads and unzips module files", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  m <- "test"
  f <- downloadModule(m, tmpdir)

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, m)), 0)
})

test_that("downloadData downloads and unzips module data", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  m <- "test"
  filenames <- c("DEM.tif", "habitatQuality.tif")
  f <- downloadModule(m, tmpdir)
  t1 <- system.time(suppressMessages(downloadData(m, tmpdir)))
  result <- suppressMessages(checksums(m, tmpdir)$results)
  expect_true(all(file.exists(file.path(tmpdir, m, "data", filenames))))
  expect_true(all(result == "OK"))

  # shouldn't need a redownload because file exists
  t2 <- system.time(suppressMessages(downloadData(m, tmpdir)))
  expect_true(t1[3] > t2[3]) # compare elapsed times
})
