test_that("module coverage work", {
  name <- "testModule"
  path <- file.path(tempdir(), "testModule") %>% checkPath(create = TRUE)
  newModule(name = name, path = path)
  moduleCoverageTest <- moduleCoverage(name = name, path = path)
  expect_is(moduleCoverageTest, "list")
  expect_equal(names(moduleCoverageTest),
               c("moduleCoverage", "functionCoverage",
                 "testedFunctions", "untestedFunctions"))
  expect_is(moduleCoverageTest$moduleCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$moduleCoverage)),
               c("names", "class"))

  expect_is(moduleCoverageTest$functionCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$functionCoverage)),
               c("names", "class"))
  expect_equal(covr::percent_coverage(moduleCoverageTest$moduleCoverage),0)
  expect_equal(covr::percent_coverage(moduleCoverageTest$functionCoverage),0)
  expect_is(moduleCoverageTest$testedFunctions, "data.table")
  expect_is(moduleCoverageTest$untestedFunctions, "data.table")
  rm(moduleCoverageTest)

  moduleCoverageTest <- moduleCoverage(name = name, path = path,
                                       byFunctionName = FALSE)
  expect_is(moduleCoverageTest, "list")
  expect_equal(names(moduleCoverageTest),
               c("moduleCoverage", "functionCoverage",
                 "testedFunctions", "untestedFunctions"))
  expect_is(moduleCoverageTest$moduleCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$moduleCoverage)),
               c("names", "class"))
  expect_is(moduleCoverageTest$functionCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$functionCoverage)),
               c("names", "class"))
  expect_equal(covr::percent_coverage(moduleCoverageTest$moduleCoverage),60)
  expect_equal(covr::percent_coverage(moduleCoverageTest$functionCoverage),60)
  expect_equal(moduleCoverageTest$testedFunctions,
               data.table(FunctionName = c("testModuleEvent1", "testModuleEvent2"),
                          Coverage = 100))
  expect_equal(moduleCoverageTest$untestedFunctions,
               data.table(FunctionName = c("testModuleInit", "testModulePlot",
                                           "testModuleSave")))
  unlink(path, recursive = TRUE)
})
