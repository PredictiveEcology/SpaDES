test_that("spelling errors", {
  skip_on_cran()
  skip_if_not_installed("hunspell")

  curDir <- getwd(); warning(curDir)
  pkgDir <- if (basename(curDir) == "testthat") {
    checkPath(file.path(curDir, "..", ".."))
  } else if ("SpaDES" %in% basename(list.dirs(curDir))) {
    checkPath(file.path(curDir, "SpaDES"))
  } else {
    checkPath(file.path(curDir))
  }

  ## ensure that stats terms are included in the word list
  .words.file <- file.path(pkgDir, ".aspell", "words.pws")
  .words <- readLines(.words.file)[-1]
  .en_stats <- hunspell::en_stats
  .complete <- all(.en_stats %in% .words)
  expect_true(.complete)

  ## if needed, add any new stats words to the word list
  if (interactive() && !.complete) {
    ignore <- unique(c(.en_stats, .words, "SpaDES"))
    aspell_write_personal_dictionary_file(ignore, .words.file)
  }

  ## check vignettes
  wrds_Rmd <- aspell_package_vignettes(pkgDir)
  expect_equal(nrow(wrds_Rmd), 0)

  ## check help (Rd) files
  wrds_Rd <- aspell_package_Rd_files(pkgDir, drop = c("\\author", "\\references"))
  #expect_equal(nrow(wrds_Rd), 0)
  wrds_Rd ## TEMPORARY: remave once all spelling errors are fixed

  ## check code files (messages, warnings, etc.)
  wrds_C <- aspell_package_C_files(pkgDir)
  expect_equal(nrow(wrds_C), 0)
})
