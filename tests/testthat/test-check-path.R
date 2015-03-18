test_that("checkPath consistency", {
  currdir <- getwd()
  on.exit(setwd(currdir))

  setwd(tmpdir <- tempdir())

  dir.create("aaa/zzz", recursive=TRUE, showWarnings=FALSE)
  paths <- list("./aaa/zzz",
                "./aaa/zzz/",
                ".//aaa//zzz",
                ".//aaa//zzz/",
                ".\\aaa\\zzz",
                ".\\aaa\\zzz\\",
                paste0(tmpdir, "/aaa/zzz"),
                paste0(tmpdir, "/aaa/zzz/"),
                file.path(tmpdir, "aaa", "zzz"))

  checked <- lapply(paths, checkPath, create=FALSE)
  expect_that(length(unique(checked)), testthat::equals(1))
  unlink(tmpdir, recursive=TRUE)
})
