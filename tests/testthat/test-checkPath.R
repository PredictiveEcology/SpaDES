test_that("checkPath: normPath consistency", {
  cwd <- getwd()

  # don't use checkPath here because we are testing normPath!
  tmpdir <- file.path(tempdir(), "test_normPath")
  dir.create(tmpdir, recursive = TRUE) # create dir before normalizePath (#267)
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)

  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  paths <- list("./aaa/zzz",
                "./aaa/zzz/",
                ".//aaa//zzz",
                ".//aaa//zzz/",
                ".\\aaa\\zzz",
                ".\\aaa\\zzz\\",
                paste0(tmpdir, "/aaa/zzz"),
                paste0(tmpdir, "/aaa/zzz/"),
                file.path(tmpdir, "aaa", "zzz"))

  checked <- normPath(paths)
  expect_that(length(unique(checked)), testthat::equals(1))

  # extra checks for missing/NA/NULL
  expect_equal(normPath(), character())
  expect_true(all(is.na(normPath(list(NA, NA_character_)))))
  expect_equal(normPath(NULL), character())
})

test_that("checkPath: checkPath consistency", {
  currdir <- getwd()

  # don't use checkPath here because we are testing checkPath
  tmpdir <- file.path(tempdir(), "test_checkPath")
  dir.create(tmpdir, recursive = TRUE) # create dir before normalizePath (#267)
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)

  on.exit({
    setwd(currdir)
    unlink(tmpdir, recursive = TRUE)
  })
  setwd(tmpdir)


  dir.create("aaa/zzz", recursive = TRUE, showWarnings = FALSE)
  paths <- list("./aaa/zzz",
                "./aaa/zzz/",
                ".//aaa//zzz",
                ".//aaa//zzz/",
                ".\\aaa\\zzz",
                ".\\aaa\\zzz\\",
                paste0(tmpdir, "/aaa/zzz"),
                paste0(tmpdir, "/aaa/zzz/"),
                file.path(tmpdir, "aaa", "zzz"))

  checked <- lapply(paths, checkPath, create = FALSE)
  expect_that(length(unique(checked)), testthat::equals(1))
  unlink(tmpdir, recursive = TRUE)

  # check that length(path)==1
  expect_error(checkPath(unlist(paths)), "path must be a character vector of length 1.")

  # extra checks for missing/NA/NULL
  expect_error(checkPath(), "Invalid path: no path specified.")
  expect_error(checkPath(NULL), "Invalid path: cannot be NULL.")
  expect_error(checkPath(NA_character_), "Invalid path: cannot be NA.")
})
