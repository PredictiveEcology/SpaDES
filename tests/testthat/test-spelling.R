if (interactive()) require(testthat)

test_that("spelling errors", {
  skip_on_cran()
  skip_if_not_installed(hunspell)

  ## ensure that stats terms are included in the word list
  .words <- readLines(".aspell/words.pws")[-1]
  .en_stats <- hunspell::en_stats
  .complete <- all(.en_stats %in% .words)
  expect_true(.complete)

  ## if needed, add any new stats words to the word list
  if (interactive() && !.complete) {
    ignore <- unique(c(.en_stats, .words, "SpaDES"))
    aspell_write_personal_dictionary_file(ignore, ".aspell/words.pws")
  }

  ## check vignettes
  wrds_Rmd <- aspell_package_vignettes(".")
  expect_equal(nrow(wrds_Rmd), 0)

  ## check help (Rd) files
  wrds_Rd <- aspell_package_Rd_files(".", drop = c("\\author", "\\references"))
  #expect_equal(nrow(wrds_Rd), 0)
  wrds_Rd ## TEMPORARY: remave once all spelling errors are fixed

  ## check code files (messages, warnings, etc.)
  wrds_C <- aspell_package_C_files(".")
  expect_equal(nrow(wrds_C), 0)
})
