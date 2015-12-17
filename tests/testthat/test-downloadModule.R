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

  moduleName <- "forestAge"
  filename <- "can_age04_1km.tif"
  m <- downloadModule(moduleName, tmpdir)
  d <- downloadData(moduleName, tmpdir)
  result <- checksums(moduleName, tmpdir)$result
  expect_true(file.exists(file.path(tmpdir, moduleName, "data", filename)))

  expect_true(result == "OK")
})
