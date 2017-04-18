test_that("spelling errors", {
  skip_on_appveyor() ## no suitable spellchecker installed
  skip_on_travis()   ## no suitable spellchecker installed
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("hunspell")

  ## ensure that stats terms are included in the word list
  pkg <- "SpaDES"
  .words.file <- system.file("dict/words.rds", package = pkg)
  .words <- readRDS(.words.file)
  .en_stats <- hunspell::en_stats
  .complete <- all(.en_stats %in% .words)
  expect_true(.complete)

  ## if needed, add any new stats words to the word list
  if (interactive() && !.complete) {
    ignore <- sort(unique(c(.en_stats, .words, pkg)))
    saveRDS(ignore, .words.file)
  }

  pkgDir <- system.file(package = pkg)

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
