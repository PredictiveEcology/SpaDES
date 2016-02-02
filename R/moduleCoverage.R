################################################################################
#' Calculate module coverage of unit tests
#'
#' Calculate the test coverage by unit tests for the module and its functions.
#'
#' @param name  Character string. The module's name.
#'
#' @param path  Character string. The path to the module directory
#'              (default is the current working directory).
#'
#' @param byFunctionName Logical. Specify whether moduleCoverage scans test files by module's function
#'                       names, i.e., test-functionName.R. Set this argument as TRUE can
#'                       speed up the function with expense of ignoring the test files do not
#'                       match the functions' name. Otherwise, for the function that does not have
#'                       corresponding test file, the moduleCoverage tests all the test files in the test
#'                       folder.
#'                       The default is \code{TRUE}.
#'
#' @return Return two coverage objects and two data tables. The two coverage objects are
#' moduleCoverage and functionCoverage. The moduleCoverage contains percentage of coverage by unit tests for the module.
#' The functioinCoverage contains percentages of coverage by unit tests for functions in the module.
#' The returned two objects are compatible to \code{shine} function in \code{covr} package.
#' Please use \code{shine} to view the information of coverage. Two data tables give the information
#' of the tested and untested functions in module.
#'
#' @note For running this function, the tests file must be restrictly placed in tests/testthat folder under module path.
#'       To automatically generate this folder, please set unitTests = TRUE when develop a new module using \code{\link{newModule}}.
#'
#' @seealso \code{\link{newModule}}.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname moduleCoverage
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#'  library(igraph)
#'  library(SpaDES)
#'  tmpdir <- tempdir()
#'  modulePath <- file.path(tmpdir, "Modules") %>% checkPath(create = TRUE)
#'  moduleName <- "forestAge" # sample module to test
#'  downloadModule(name = moduleName, path = modulePath) # download sample module
#'  testResults <- moduleCoverage(name = moduleName, path = modulePath)
#'  shine(testResults$moduleCoverage)
#'  shine(testResults$functionCoverage)
#'  unlink(tmpdir, recursive = TRUE)
#' }
setGeneric("moduleCoverage", function(name, path, byFunctionName) {
  standardGeneric("moduleCoverage")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "character", byFunctionName = "logical"),
  definition = function(name, path, byFunctionName) {
    tmpdir <- tempdir(); on.exit(unlink(tmpdir, recursive = TRUE))
    fnDir <- file.path(tmpdir, name, "moduleFunctions") %>%
      checkPath(create = TRUE)
    outputDir <- file.path(fnDir, "output")
    testDir <- file.path(path, name, "tests", "testthat")

    if (!requireNamespace("covr", quietly = TRUE) ||
        !requireNamespace("testthat", quietly = TRUE)) {
      stop("Suggested packages `covr` and `testthat` not found. ",
           "Both must be installed to test module coverage.")
    }
    stopifnot(dir.exists(testDir))

    fnCoverage <- list()
    mCoverage <- list()

    # read the module
    mySim <- simInit(times = list(start = 0, end = 1),
                     params = list(),
                     modules = list(paste0(name)),
                     objects = list(),
                     paths = list(modulePath = path, outputPath = outputDir))

    objects <- mget(objects(mySim), envir(mySim))
    objects <- objects[which(lapply(objects, is.function) == TRUE)]
    fnIndex <- which(names(objects) != paste("doEvent.", name, sep=""))

    for (i in fnIndex) {
      fnName <- file.path(fnDir, paste0(names(objects[i]), ".R", sep = ""))
      fnLines <- deparse(objects[i][[1]])
      cat(names(objects[i]), " <- ", fnLines[1:2], "\n", sep = "", file = fnName)
      cat(fnLines[3:length(fnLines)], sep = "\n", file = fnName, append = TRUE)
      source(fnName)
    }
    rm(i)

    untestedFunctions <- data.table(FunctionName = character())
    testedFunctions <- data.table(FunctionName = character(), Coverage = numeric())
    if(byFunctionName){
      # create a dummy test file
      dummyTestFile <- file.path(fnDir, paste("test-dummyTestFile.R", sep=""))
      cat("test_that(\"this is a temperal dummy test file. \", { \n",
          "  expect_equal(1,1) \n",
          "}) \n", file = dummyTestFile, fill = FALSE, sep = "")
    }
    for (i in fnIndex) {
      testfiles <- file.path(testDir, paste0("test-", objects(mySim)[i], ".R"))
      if(byFunctionName){
        if(file.exists(testfiles)){
          mTest <- covr::function_coverage(objects(mySim)[i], env=envir(mySim),
                                           testthat::test_file(testfiles, env = envir(mySim)))
          fnTest <- covr::function_coverage(objects(mySim)[i],
                                            testthat::test_file(testfiles))
          testedFunctions <- rbind(testedFunctions,
                                   data.table(FunctionName = objects(mySim)[i],
                                              Coverage = round(covr::percent_coverage(fnTest),2)))
          mCoverage <- append(mCoverage, mTest)
          fnCoverage <- append(fnCoverage, fnTest)
        } else {
          mTest <- covr::function_coverage(objects(mySim)[i], env=envir(mySim),
                                           testthat::test_file(dummyTestFile, env = envir(mySim)))
          fnTest <- covr::function_coverage(objects(mySim)[i],
                                            testthat::test_file(dummyTestFile))
          untestedFunctions <- rbind(untestedFunctions, data.table(FunctionName = objects(mySim)[i]))
          mCoverage <- append(mCoverage, mTest)
          fnCoverage <- append(fnCoverage, fnTest)
        }
      } else {
        if (file.exists(testfiles)) {
          mTest <- covr::function_coverage(objects(mySim)[i], env = envir(mySim),
                                           testthat::test_file(testfiles, env = envir(mySim)))
          fnTest <- covr::function_coverage(objects(mySim)[i],
                                            testthat::test_file(testfiles))
          testedFunctions <- rbind(testedFunctions,
                                   data.table(FunctionName = objects(mySim)[i],
                                              Coverage = round(covr::percent_coverage(fnTest),2)))
          mCoverage <- append(mCoverage, mTest)
          fnCoverage <- append(fnCoverage, fnTest)
        } else {
          mTest <- covr::function_coverage(objects(mySim)[i], env = envir(mySim),
                                           testthat::test_dir(testDir, env = envir(mySim)))
          fnTest <- covr::function_coverage(objects(mySim)[i],
                                            testthat::test_dir(testDir))
          if(covr::percent_coverage(fnTest)==0){
            untestedFunctions <- rbind(untestedFunctions, data.table(FunctionName = objects(mySim)[i]))
          } else {
            testedFunctions <- rbind(testedFunctions,
                                     data.table(FunctionName = objects(mySim)[i],
                                                Coverage = round(covr::percent_coverage(fnTest),2)))
          }
          mCoverage <- append(mCoverage, mTest)
          fnCoverage <- append(fnCoverage, fnTest)
        }
      }
    }
    class(mCoverage) <- "coverage"
    class(fnCoverage) <- "coverage"
    unlink(fnDir, recursive = TRUE)
    return(list(moduleCoverage = mCoverage, functionCoverage = fnCoverage,
                testedFunctions = testedFunctions, untestedFunctions = untestedFunctions))
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing", byFunctionName = "logical"),
  definition = function(name, byFunctionName){
    moduleCoverage(name = name, path = ".", byFunctionName = byFunctionName)
  })

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "character", byFunctionName = "missing"),
  definition = function(name, path){
    moduleCoverage(name = name, path = path, byFunctionName = TRUE)
  })

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing", byFunctionName = "missing"),
  definition = function(name){
    moduleCoverage(name = name, path = ".", byFunctionName = TRUE)
  })
