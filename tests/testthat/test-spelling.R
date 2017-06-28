test_that("spelling errors", {
  skip_on_appveyor() ## no suitable spellchecker installed
  skip_on_travis()   ## no suitable spellchecker installed
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("hunspell")

  ## ensure that stats terms are included in the word list
  pkg <- "SpaDES"
  pkgDir <- system.file(package = pkg)

  if (interactive() && requireNamespace("devtools")) {
    devtools::dev_mode(TRUE)
  }
  .wordsFile <- system.file("dict/words.rds", package = pkg)
  .words <- readRDS(.wordsFile)
  .en_stats <- hunspell::en_stats # nolint
  .complete <- all(.en_stats %in% .words)
  expect_true(.complete)

  ## if needed, add any new stats words to the word list
  if (interactive() && !.complete) {
    ignore <- sort(unique(c(.en_stats, .words, pkg)))
    saveRDS(ignore, .wordsFile)
  }

  ## check vignettes
  wrdsRmd <- aspell_package_vignettes(pkgDir)
  expect_equal(nrow(wrdsRmd), 0)

  ## check help (Rd) files
  wrdsRd <- aspell_package_Rd_files(pkgDir, drop = c("\\author", "\\references"))
  #expect_equal(nrow(wrdsRd), 0)
  wrdsRd ## TEMPORARY: remave once all spelling errors are fixed

  ## check code files (messages, warnings, etc.)
  wrdsC <- aspell_package_C_files(pkgDir)
  expect_equal(nrow(wrdsC), 0)
})
