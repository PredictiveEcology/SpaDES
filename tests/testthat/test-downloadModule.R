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
