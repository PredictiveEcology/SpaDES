################################################################################
#' Run an experiment using spades
#'
#' This is essentially a wrapper around the \code{spades} call that allows for multiple
#' calls to spades. Generally, there are 2 reasons to do this: replication and varying inputs
#' to accomplish some sort of simulation experiment. This function deals with both of these
#' cases. In the case of varying inputs, this function will create a fully factorial experiment
#' among all levels of the variables passed into the function, if they are parameters. If varying
#' modules are passed into the function, then it is likewise fully factorial. However, if it is both
#' varying parameters and varying modules, then it may not make sense to be fully factorial as some
#' parameters that are varying may not be defined in all module sets. The function will fully
#' factorial where it makes sense to do this, and a complete set otherwise. The function
#' requires a \code{simList} object, plus optional params and/or modules and/or replications. Future
#' updates will allow varying objects or inputs.
#'
#' @inheritParams spades
#'
#' @inheritParams simInit
#' @param replicates The number of replicates to run of the same \code{simList}
#'
#' @param substrLength For outputPath, how many characters should be kept from each factor level. See Details.
#'
#' @details
#' This function requires a complete simList. All values passed into this function will
#' override the \code{simList} values.
#'
#' There are a few behaviours that are assumed, notably with output directories. If there
#' are only replications, then a set of subdirectories will be created, one for each replicate.
#' If there are varying parameters and or modules, \code{outputPath} is updated to include
#' a subdirectory for each level of the experiment. Specifically, the module(s) and parameter
#' names and the parameter values are all part of the
#' subdirectory names.
#' The default rule for naming is a concatenation of:
#' 1. The experiment level (arbitrarily starting at 1).
#' 2. The module, parameter name and parameter value, for each parameter that is varying.
#' 3. The module set.
#' This subdirectory name could be long
#' if there are many dimensions to the experiment. The parameter \code{substrLength} determines
#' the level of truncation of the parameter and module names for these subdirectories. For example,
#' The  resulting directory name for a parameter value of 0.225 for the pspread parameter in the
#' fireSpread module and the N parameter in the caribouMovement module would be:
#' \code{1_fir_spr_0.225-car_N_10} if \code{substrLength} is 3, the default.
#'
#' @return Invisibly returns a list of the resulting \code{simList} objects from the fully
#' factorial experiment.
#' Since this may be large, the user is not obliged to return this object (as it is returned invisibly).
#' Clearly, there may be objects saved during simulations. This would be determined as per a
#' normal \code{\link{spades}} call, using \code{outputs}.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}
#'
#' @importFrom raster getCluster returnCluster
#' @importFrom snow clusterApplyLB
#' @export
#' @docType methods
#' @rdname spades
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{
#'
#'  library(raster)
#'  beginCluster(4)
#'
#' # Example of changing parameter values
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      # Turn off interactive plotting
#'      fireSpread = list(.plotInitialTime=NA),
#'      caribouMovement = list(.plotInitialTime=NA),
#'      randomLandscapes = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tempdir()),
#'    # Save final state of landscape and caribou
#'    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
#'  )
#'
#'
#'  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
#'  #    and 2 levels of 1 param in caribouMovement)
#'  experimentParams <- list(fireSpread = list(spreadprob = c(0.2, 0.23),
#'                                              nFires = c(20, 10)),
#'                        caribouMovement = list(N = c(100, 1000)))
#'
#'  sims <- experiment(mySim, params=experimentParams)
#'
#'  # Read in outputs from sims object
#'  FireMaps = do.call(stack, lapply(1:NROW(attr(sims,"experiment")), function(x) sims[[x]]$landscape$Fires))
#'  Plot(FireMaps, new=TRUE)
#'
#'  # Or reload objects from files, useful if sim objects too large to store in RAM
#'  caribouMaps <- lapply(sims, function(sim) {
#'    caribou <- readRDS(outputs(sim)$file[outputs(sim)$objectName=="caribou"])
#'    }
#'  )
#'  names(caribouMaps) <- paste0("caribou",1:8)
#'  # Plot does not plot whole lists (yet)
#'  Plot(caribouMaps[[1]], caribouMaps[[2]], caribouMaps[[3]], caribouMaps[[4]],
#'       caribouMaps[[5]], caribouMaps[[6]], caribouMaps[[7]], caribouMaps[[8]],
#'       size=0.1)
#'
#' #######
#' # Example of changing modules, i.e., caribou with and without fires
#'  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
#'  #    and 2 levels of 1 param in caribouMovement)
#'  experimentModules <- list(
#'      c("randomLandscapes", "fireSpread", "caribouMovement"),
#'      c("randomLandscapes", "caribouMovement")
#'      )
#'
#'  sims <- experiment(mySim, modules=experimentModules, params=experimentParams)
#'
#'  # manipulate directory names
#'  sims <- experiment(mySim, params=experimentParams, dirPrefix=c("expt", "simNum"))
#'
#'  # doing replicate runs - THESE TAKE SOME TIME (minutes if not using a cluster)
#'  sims <- experiment(mySim, replicates = 3)
#'  # putting them in a subdirectory
#'  sims <- experiment(mySim, replicates = 3, dirPrefix = c("expt"))
#'
#'  # Both replication and experiment, both params and modules
#'  # use a sub directory
#'  outputPath(mySim) <- file.path(tempdir(), "myExpt3")
#'  sims <- experiment(mySim, replicates = 5, modules=experimentModules,
#'                     params=experimentParams, dirPrefix=c("expt", "simNum"))
#'
#'
#'
#'  # Use replication to build a probability surface.
#'  # For this to be meaningful, we need to provide a fixed landscape,
#'  #   not a randomLandscape for each experiment level. So requires 2 steps.
#'  # Step 1 - run randomLandscapes once to get a landscape map
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 0.1, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape"),
#'      # Turn off interactive plotting
#'      fireSpread = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("randomLandscapes"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tempdir()),
#'  )
#'  mySim <- spades(mySim)  # Run it
#'  #extract the landscape, which will be passed into next as an object
#'  landscape <- mySim$landscape
#'
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 1, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      # Turn off interactive plotting
#'      fireSpread = list(.plotInitialTime=NA),
#'      caribouMovement = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("fireSpread", "caribouMovement"), # No randomLandscapes modules
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tempdir()),
#'    objects = c("landscape"), # Pass in the object here
#'    # Save final state of landscape and caribou
#'    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
#'  )
#'
#'  outputPath(mySim) <- file.path(tempdir(), "myExpt6")
#'  sims <- experiment(mySim, replicates = 50) # Run experiment
#'  attr(sims, "experiment") # shows the experiment, which in this case is just replicates
#'  # list all files that were saved called 'landscape'
#'  files1 <- dir(outputPath(mySim), recursive=TRUE, pattern="landscape", full.names=TRUE)
#'  # Read them in - alternatively, this could use the sims object directly also, e.g., sims[[1]]$landscape$Fires
#'  landscapes <- lapply(files1, readRDS)
#'  fires1 <- do.call(stack, lapply(landscapes, function(x) x$Fires)) # Extract just Fire layer
#'  fires1[fires1>0] <- 1 # convert to 1s and 0s
#'  fireProb <- sum(fires1)/nlayers(fires1) # sum them and convert to probability
#'  Plot(fireProb, new=TRUE)
#'
#'  endCluster()
#'  library(magrittr)
#'  dir(file.path(tempdir()), pattern="myExpt[.]*", full.names=TRUE) %>%
#'    unlink(recursive=TRUE)
#' }
#'
setGeneric("experiment", function(sim, replicates = 1, params = list(), modules = list(),
                                  objects = list(), inputs = list(),
                                  dirPrefix = "simNum", substrLength=3, ...) {
  standardGeneric("experiment")
})

#' @rdname spades
setMethod(
  "experiment",
  signature(sim = "simList"),
  definition = function(sim, replicates, params, modules, objects, inputs, substrLength, ...) {

    cl <- tryCatch(getCluster(), error=function(x) NULL)
    on.exit(if(!is.null(cl)) returnCluster())

    if(length(modules)==0) modules <- list(modules(sim)[-(1:4)])
    factorialExpList <- lapply(modules, function(x) {
      paramsTmp <- pmatch(x, names(params)) %>% na.omit
      factorsTmp <- if(NROW(paramsTmp)>0) {
        unlist(params[paramsTmp], recursive = FALSE)
      } else {
        params
      }

      factorsX <- append(factorsTmp, list(modules=x))
      factorialExpInner <- expand.grid(factorsTmp, stringsAsFactors = FALSE)
      modulesShort <- paste(substr(x,1,substrLength),collapse=",")
      if(NROW(factorialExpInner)>0) {
        factorialExpInner[["modules"]] <- modulesShort
      } else {
        factorialExpInner <- data.frame(modules=modulesShort, stringsAsFactors = FALSE)
      }
      factorialExpInner
    })
    factorialExp <- rbindlist(factorialExpList, fill=TRUE) %>% data.frame(stringsAsFactors=FALSE)
    numExpLevels <- NROW(factorialExp)
    factorialExp$expLevel <- seq_len(numExpLevels)

    # Add replicates to experiment
    if(replicates>1) {
      if(length(replicates==1)) {
        replicates=seq_len(replicates)
      }
      factorialExp <- do.call(rbind, replicate(length(replicates), factorialExp, simplify = FALSE))
      factorialExp$replicates=rep(replicates, each=numExpLevels)
    }

    FunDef <- function(ind, ...) {
      mod <- strsplit(names(factorialExp), split="\\.") %>% sapply(function(x) x[1])
      param<- strsplit(names(factorialExp), split="\\.") %>% sapply(function(x) x[2])
      param[is.na(param)] <- ""
      paramValues <- factorialExp[ind,]

      whNotExpLevel <- which(colnames(paramValues)!="expLevel")
      if(length(whNotExpLevel)<length(paramValues)) {
        mod <- mod[whNotExpLevel]
        param <- param[whNotExpLevel]
        paramValues <- paramValues[whNotExpLevel]
      }

      whNotRepl <- which(colnames(paramValues)!="replicates")
      if(length(whNotRepl)<length(paramValues)) {
        repl <- paramValues$replicates
        mod <- mod[whNotRepl]
        param <- param[whNotRepl]
        paramValues <- paramValues[whNotRepl]
      }

      notNA <- which(!is.na(paramValues))
      if(length(notNA)<length(mod)) {
        mod <- mod[notNA]
        param <- param[notNA]
        paramValues <- paramValues[notNA]
      }

      sim_ <- sim
      for(x in 1:length(mod)) {
        if(any(mod!="modules")) {
          if(!is.na(factorialExp[ind,x])) {
            params(sim_)[[mod[x]]][[param[[x]]]] <- factorialExp[ind,x]
          }
        }
      }

      # Deal with directory structures
      if(numExpLevels>1) {
        dirName <- paste(collapse = "-",substr(mod,1,substrLength),
                         substr(param, 1,substrLength),
                         paramValues, sep="_")
        dirName <- gsub(dirName, pattern="__", replacement = "_")
        if(any(dirPrefix=="simNum")) {
          exptNum <- paddedFloatToChar(factorialExp$expLevel[ind], ceiling(log10(numExpLevels+1)))
          dirPrefixTmp <- paste0(dirPrefix, collapse="")
          dirPrefix <- gsub(dirPrefixTmp, pattern="simNum", replacement=exptNum)
        }
        if(any(dirPrefix!="")) {
          dirName <- paste(paste(dirPrefix, collapse=""), dirName, sep="_")
        }
      } else {
        if(any(dirPrefix!="")) {
          dirPrefixTmp <- paste0(dirPrefix, collapse="")
          dirName <- gsub(dirPrefixTmp, pattern="simNum", replacement="")
        }
      }

      if(exists("repl", inherits = FALSE)) {
        if(!is.null(dirName)) {
          dirName <- file.path(dirName, paste0("rep",paddedFloatToChar(repl, ceiling(log10(length(replicates)+1)))))
        } else {
          dirName <- file.path(paste0("rep",paddedFloatToChar(repl, ceiling(log10(length(replicates)+1)))))
        }
      }
      newOutputPath <- file.path(paths(sim_)$outputPath,dirName)
      if(!dir.exists(newOutputPath)) dir.create(newOutputPath, recursive = TRUE)
      paths(sim_)$outputPath <- newOutputPath
      outputs(sim_)$file <- file.path(newOutputPath, basename(outputs(sim_)$file))

      sim_ <- spades(sim_, ...)
      return(sim_)
    }

    if(!is.null(cl)) {
      parFun <- "clusterApplyLB"
      args <- list(x=1:NROW(factorialExp), fun=FunDef)
      args <- append(list(cl=cl), args)
    } else {
      parFun <- "lapply"
      args <- list(X=1:NROW(factorialExp), FUN=FunDef)
    }

    sims <- do.call(get(parFun), args)
    attr(sims, "experiment") <- factorialExp
    return(invisible(sims))
  })

#' @rdname spades
setMethod("spades",
          signature(sim = "simList", debug = "missing"),
          definition = function(sim) {
            stopifnot(class(sim) == "simList")
            return(spades(sim, debug = FALSE))
          })
