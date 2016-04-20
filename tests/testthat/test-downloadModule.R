test_that("downloadModule downloads and unzips a single module", {
  skip_on_cran()

  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  library(igraph); on.exit(detach("package:igraph"))

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  f <- downloadModule(m, tmpdir)[[1]] %>% unlist() %>% basename()

  f_expected <- c("citation.bib", "CHECKSUMS.txt", "LICENSE",
                  "README.txt", "test.R", "test.Rmd")

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, m)), 0)
  expect_equal(f, f_expected)
})

test_that("downloadModule downloads and unzips a parent module", {
  skip_on_cran()

  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  library(igraph); on.exit(detach("package:igraph"))

  m <- "LCC2005"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  f <- downloadModule(m, tmpdir)[[1]] %>% unlist() %>% as.character()
  d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

  d_expected <- moduleMetadata("LCC2005", tmpdir)$childModules %>%
    c(m, "data", "testthat") %>% sort()

  expect_equal(length(f), 44)
  expect_equal(d, d_expected)
})

test_that("downloadData downloads and unzips module data", {
  skip_on_cran()

  if (Sys.info()['sysname'] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules")
  datadir <- file.path(tmpdir, m, "data") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  filenames <- c("DEM.tif", "habitatQuality.tif")
  f <- downloadModule(m, tmpdir)
  t1 <- system.time(downloadData(m, tmpdir))
  result <- checksums(m, tmpdir)$result
  expect_true(all(file.exists(file.path(datadir, filenames))))
  expect_true(all(result == "OK"))

  # shouldn't need a redownload because file exists
  t2 <- system.time(downloadData(m, tmpdir))
  expect_true(t1[3] > t2[3]) # compare elapsed times

  # if one file is missing, will fill in correctly
  unlink(file.path(datadir, filenames)[1])
  downloadData(m, tmpdir, quiet = TRUE)
  expect_true(all(file.exists(file.path(datadir, filenames))))

  # if files are there, but one is incorrectly named
  file.rename(from = file.path(datadir, filenames[1]),
              to = file.path(datadir, "test.tif"))
  downloadData(m, tmpdir, quiet = TRUE) # renames the file back to expected
  expect_true(all(file.exists(file.path(datadir, filenames))))

  # if files are there with correct names, but wrong content
  library(raster); on.exit(detach("package:raster"), add = TRUE)
  ras <- raster(file.path(datadir, filenames[2]))
  ras[4] <- maxValue(ras) + 1
  writeRaster(ras, filename = file.path(datadir, filenames[2]), overwrite = TRUE)
  downloadData(m, tmpdir, quiet = TRUE)
  expect_true(all(file.exists(file.path(datadir, filenames))))
})
