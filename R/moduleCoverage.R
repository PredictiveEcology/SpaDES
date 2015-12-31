###############################################################################
#' Calculate module coverage of unit tests
#'
#'
#' Calculate the test coverage by unit tests for the module and functions in module
#'
#'
#' @param name  Character string. The module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @return Return two coverage objects: moduleCoverage and functionCoverage.
#'                The moduleCoverage contains percentage of coverage by unit tests for the module.
#'                The functioinCoverage contains percentages of coverage by unit tests for functions in the module.
#'                The returned two objects are compatible to \code{shine} function in \code{covr} package.
#'                Please use \code{shine} to view the information of coverage.
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
#'  # test module and function coverage for biomassSuccessionLANDIS
#'  name <- "biomassSuccessionLANDIS"
#'  path <- "~/GitHub/nrv-succession/code blitz succession/modules"
#'  testResults <- moduleCoverage(name=name,path=path)
#' }
setGeneric("moduleCoverage", function(name, path) {
  standardGeneric("moduleCoverage")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "character"),
  definition = function(name, path) {
    # read the module into the work space
    mySim <- simInit(times=list(start=0, end=1),
                     params=list(),
                     modules=list(paste0(name)),
                     objects=list(),
                     paths=list(modulePath=path,
                                outputPath="~/output"))

    objects <- mget(objects(mySim),mySim@.envir)
    function_index <- which(lapply(objects,is.function)==TRUE)

    if(dir.exists(file.path(path, name, "tests","testthat"))){
      testFileFolder <- file.path(path, name, "tests","testthat")
      functionFolder <- checkPath(file.path(path, name, "moduleFunctions"), create=TRUE)
      moduleCoverage <- list()
      functionCoverage <- list()
      for (i in function_index){
        functionName <- file.path(functionFolder,paste0(names(objects[i]),".R",sep=""))
        functionLines <- deparse(objects[i][[1]])
        cat(names(objects[i])," <- ",functionLines[1:2],"\n",sep="",file=functionName)
        cat(functionLines[3:length(functionLines)],sep="\n",file=functionName,append = TRUE)
        source(functionName)
      }
      rm(i)

      for(i in function_index){
        testfiles <- file.path(testFileFolder,paste("test-",objects(mySim)[i],".R",sep=""))
        if(file.exists(testfiles)){
          moduleTest <- covr::function_coverage(objects(mySim)[i], env=mySim@.envir,
                                                testthat::test_file(testfiles,env = mySim@.envir))
          functionTest <- covr::function_coverage(objects(mySim)[i],
                                                  testthat::test_file(testfiles))
          moduleCoverage <- append(moduleCoverage,moduleTest)
          functionCoverage <- append(functionCoverage,functionTest)

        } else {
          moduleTest <- covr::function_coverage(objects(mySim)[i], env=mySim@.envir,
                                                testthat::test_dir(testFileFolder,env = mySim@.envir))
          functionTest <- covr::function_coverage(objects(mySim)[i],
                                                  testthat::test_dir(testFileFolder))
          moduleCoverage <- append(moduleCoverage,moduleTest)
          functionCoverage <- append(functionCoverage,functionTest)
        }
      }
      class(moduleCoverage) <- "coverage"
      class(functionCoverage) <- "coverage"
      unlink(functionFolder, recursive=TRUE)
      return(list(moduleCoverage=moduleCoverage,functionCoverage=functionCoverage))
    } else {
      stop("Your test files must be placed in ",file.path(path,name,"tests","testthat"))
    }
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing"),
  definition = function(name){
    moduleCoverage(name = name, path = ".")
})

