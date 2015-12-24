###############################################################################
#' Calculate the percentage of the module codes that are tested by unit tests
#'
#'
#' Calculate the coverage of the module codes that are tested by unit tests
#'
#' @param name  Character string. The module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @return return the persentage coverage of unit tests for a module and for all functions if the
#'
#' @seealso \code{\link{library}}.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname moduleCoverage
#'
#' @author Yong Luo
#'
setGeneric("moduleCoverage", function(name, path) {
  standardGeneric("moduleCoverage")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "character"),
  definition = function(name, path) {
    
    name <- "biomassSuccessionLANDIS"
    path <- "~/GitHub/nrv-succession/code blitz succession/modules"
    # read the module into the work space
    mySim <- simInit(times=list(start=0, end=1),
                     params=list(),
                     modules=list(paste0(name)),
                     objects=list(),
                     paths=list(modulePath=path,
                                outputPath="~/output"))

    objects <- mget(objects(mySim),mySim@.envir)
    function_index <- which(lapply(objects,is.function)==TRUE)

    testFileFolder <- checkPath(paste0(path,"/",name,"/unit tests/tests",sep=""),create=TRUE)
    functionFolder <- checkPath(paste0(path,"/",name,"/moduleFunctions",sep=""),create=TRUE)
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
        moduleTest <- function_coverage(objects(mySim)[i], env=mySim@.envir,
                                  test_file(testfiles,env = mySim@.envir))
        functionTest <- function_coverage(objects(mySim)[i], 
                                        test_file(testfiles))
        moduleCoverage <- append(moduleCoverage,moduleTest)
        functionCoverage <- append(functionCoverage,functionTest)
        
      } else {
        moduleTest <- function_coverage(objects(mySim)[i], env=mySim@.envir,
                                  test_dir(testFileFolder,env = mySim@.envir))
        functionTest <- function_coverage(objects(mySim)[i],
                                        test_dir(testFileFolder))
        moduleCoverage <- append(moduleCoverage,moduleTest)
        functionCoverage <- append(functionCoverage,functionTest)
      }
    }
    class(moduleCoverage) <- "coverage"
    class(functionCoverage) <- "coverage"
    unlink(functionFolder, recursive=TRUE)
    return(list(moduleCoverage=moduleCoverage,functionCoverage=functionCoverage))
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing"),
  definition = function(name){
    moduleCoverage(name = name, path = ".")
})

