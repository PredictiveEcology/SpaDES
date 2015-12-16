test_that("downloadModule downloads and unzips a single module", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  library(magrittr); on.exit(detach("package:magrittr", unload = TRUE))

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  m <- "test"
  f <- downloadModule(m, tmpdir) %>% unlist() %>% basename()

  f_expected <- c("citation.bib", "CHECKSUMS.txt", "LICENSE",
                  "README.txt", "test.R", "test.Rmd")

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, m)), 0)
  expect_equal(f, f_expected)
})

test_that("downloadModule downloads and unzips a parent module", {
  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  library(magrittr); on.exit(detach("package:magrittr", unload = TRUE))

  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive = TRUE))

  m <- "LCC2005"
  f <- downloadModule(m, tmpdir) %>% unlist()
  d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

  d_expected <- moduleMetadata("LCC2005", tmpdir)$childModules %>%
    c(m, "data") %>% sort()

  expect_equal(length(f), 40)
  expect_equal(d, d_expected)
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
  t1 <- system.time(downloadData(m, tmpdir))
  result <- checksums(m, tmpdir)$result
  expect_true(all(file.exists(file.path(tmpdir, m, "data", filenames))))
  expect_true(all(result == "OK"))

  # shouldn't need a redownload because file exists
  t2 <- system.time(downloadData(m, tmpdir))
  expect_true(t1[3] > t2[3]) # compare elapsed times

  # if one file is missing, will fill in correctly
  unlink(file.path(tmpdir, m, "data", filenames)[1])
  downloadData(m, tmpdir)
  expect_true(all(file.exists(file.path(tmpdir, m, "data", filenames))))

  # if files are there, but one is renamed
  file.rename(file.path(tmpdir, m, "data", filenames[1]),
              to=file.path(tmpdir, m, "data", "test.tif"))
  downloadData(m, tmpdir)
  expect_true(all(file.exists(file.path(tmpdir, m, "data", filenames))))

  # if files are there with correct names, but wrong content
  ras <- raster(file.path(tmpdir, m, "data", filenames[1]))
  ras[4] <- maxValue(ras)+1
  writeRaster(ras, filename=file.path(tmpdir, m, "data", filenames[1]), overwrite=TRUE)
  downloadData(m, tmpdir)
  expect_true(all(file.exists(file.path(tmpdir, m, "data", filenames))))

})
