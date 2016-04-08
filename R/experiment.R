################################################################################
#' Run an experiment using \code{\link{spades}}
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
#' requires a \code{simList} object, plus optional inputs and/or params and/or modules and/or
#' replications. Future updates will allow varying objects.
#'
#' @inheritParams spades
#'
#' @param inputs Like for \code{\link{simInit}}, but a list of \code{inputs} data.frames. See details.
#' @param params Like for \code{\link{simInit}}, but for each parameter, provide a list of alternative values. See examples.
#' @param modules Like for \code{\link{simInit}}, but a list of \code{modules} lists. See details.
#' @param replicates The number of replicates to run of the same \code{simList}
#'
#' @param substrLength Numeric. While making \code{outputPath}, this is the number of characters
#' kept from each factor level. See Details.
#'
#' @param dirPrefix String vector. This will be concatenated as a prefix on the directory names.
#' See Details.
#'
#' @param saveExperiment Logical. Should params, modules, inputs, sim, and resulting
#' experimental design be saved to a file. If TRUE are saved to a single list
#' called \code{experiment}. Default TRUE.
#'
#' @param experimentFile String. Filename if \code{saveExperiment} is TRUE; saved to
#' \code{outputPath(sim)} in \code{.RData} format. See Details.
#'
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list is
#' emptied. This is to reduce RAM load of large return object. Default FALSE.
#'
#' @param ... Passed to \code{spades}. This would only be useful for \code{debug=TRUE}.
#'
#' @details
#' This function requires a complete simList: this simList will form the basis of the
#' modifications as passed by params, modules, inputs. All params, modules, or inputs
#' passed into this function will override the corresponding params, modules or inputs
#' that are in the \code{sim} argument.
#'
#' Output directories are changed using this function: this is one of the dominant
#' side effects of this function. If there are only replications, then a set of
#' subdirectories will be created, one for each replicate.
#' If there are varying parameters and or modules, \code{outputPath} is updated to include
#' a subdirectory for each level of the experiment. These are not nested, i.e., even if there
#' are nested factors, all subdirectories due to the experimental setup will be
#' at the same level. Replicates will be one level below this.
#' The subdirectory names will include the module(s), parameter names, the parameter values,
#' and input index number (i.e., which row of the inputs data.frame).
#' The default rule for naming is a concatenation of:
#'
#' 1. The experiment level (arbitrarily starting at 1). This is padded with zeros if there are
#' many experiment levels.
#'
#' 2. The module, parameter name and parameter experiment level (not the parameter value,
#' as values could be complex), for each parameter that is varying.
#'
#' 3. The module set.
#'
#' 4. The input index number
#'
#' 5. Individual identifiers are separated by a dash.
#'
#' 6. Module - Parameter - Parameter index triplets are separated by underscore.
#'
#' e.g., a folder called: \code{01-fir_spr_1-car_N_1-inp_1} would be the first experiment level
#' (01), the first parameter value for the spr* parameter of the fir* module, the first parameter
#' value of the N parameter of the car* module, and the first input dataset provided.
#'
#' This subdirectory name could be long
#' if there are many dimensions to the experiment. The parameter \code{substrLength} determines
#' the level of truncation of the parameter, module and input names for these subdirectories.
#' For example, the  resulting directory name for changes to the \code{spreadprob} parameter in the
#' \code{fireSpread} module and the \code{N} parameter in the \code{caribouMovement} module would be:
#' \code{1_fir_spr_1-car_N_1} if \code{substrLength} is 3, the default.
#'
#' Replication is treated slightly differently. \code{outputPath} is always 1 level below the
#' experiment level for a replicate.
#' If the call to \code{experiment} is not a factorial experiment (i.e., it is just replication), then the
#' default is to put the replicate subdirectories at the top level of \code{outputPath}. To force
#' this one level down, \code{dirPrefix} can be used or a manual change to \code{outputPath} before
#' the call to experiment.
#'
#' \code{dirPrefix} can be used to give custom names to directories for outputs. There is a special
#' value, \code{"simNum"}, that is used as default, which is an arbitrary number associated with the
#' experiment. This corresponds to the row number in the \code{attr(sims, "experiment")}.
#' This \code{"simNum"} can be used with other strings, such as \code{dirPrefix=c("expt","simNum")}.
#'
#' The experiment structure is kept in two places: the return object has an attribute, and a file
#' named "experiment.rds" (see argument \code{experimentFile}) located in \code{outputPath(sim)}.
#'
#' \code{substrLength}, if \code{0}, this will eliminate the subfolder naming convention and use
#' only \code{dirPrefix}.
#'
#' @return Invisibly returns a list of the resulting \code{simList} objects from the fully
#' factorial experiment. This list has an attribute, which a list with 2 elements: the experimental
#' design provided in a wide data.frame and the experiment values in a long data.frame. There is
#' also a file saved with these two data.frames. It is named whatever is passed into
#' \code{experimentFile}.
#' Since returned list of \code{simList} objects may be large, the user is not obliged to
#' return this object (as it is returned invisibly).
#' Clearly, there may be objects saved during simulations. This would be determined as per a
#' normal \code{\link{spades}} call, using \code{outputs} like, say, \code{outputs(sims[[1]])}.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES}}
#'
#' @importFrom raster getCluster returnCluster
#' @importFrom snow clusterApplyLB clusterEvalQ
#' @export
#' @docType methods
#' @rdname experiment
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{
#'
#'  tmpdir <- file.path(tempdir(), "examples")
#'
#' # Example of changing parameter values
#'  mySimFull <- simInit(
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
#'                 outputPath = tmpdir),
#'    # Save final state of landscape and caribou
#'    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
#'  )
#'
#'
#'  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
#'  #    and 2 levels of 1 param in caribouMovement)
#'
#'  # Here is a list of alternative values for each parameter. They are length one
#'  #   numerics here -- e.g., list(0.2, 0.23) for spreadprob in fireSpread module,
#'  #   but they can be anything, as long as it is a list.
#'  experimentParams <- list(fireSpread = list(spreadprob = list(0.2, 0.23),
#'                                              nFires = list(20, 10)),
#'                        caribouMovement = list(N = list(100, 1000)))
#'
#'  sims <- experiment(mySimFull, params=experimentParams)
#'  exptDesign <- read.csv(file.path(tmpdir, "experiment.csv"))
#'
#'  # see experiment:
#'  attr(sims, "experiment")
#'
#'  # Read in outputs from sims object
#'  FireMaps = do.call(stack, lapply(1:NROW(attr(sims,"experiment")$expDesign),
#'                     function(x) sims[[x]]$landscape$Fires))
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
#' # Remove all temp files
#' unlink(tmpdir, recursive=TRUE)
#' #######
#' # Example of changing modules, i.e., caribou with and without fires
#'  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
#'  #    and 2 levels of 1 param in caribouMovement)
#'  experimentModules <- list(
#'      c("randomLandscapes", "fireSpread", "caribouMovement"),
#'      c("randomLandscapes", "caribouMovement")
#'      )
#'
#'  # Note, this isn't fully factorial because all parameters are not
#'  #   defined inside smaller module list
#'  sims <- experiment(mySimFull, modules=experimentModules, params=experimentParams)
#'
#'  # manipulate directory names - "simNum" is special, it is converted to 1, 2, ...
#'  sims <- experiment(mySimFull, params=experimentParams, dirPrefix=c("expt", "simNum"))
#'
#'  # doing replicate runs - THESE MAY TAKE SOME TIME (minutes if not using a cluster)
#'  sims <- experiment(mySimFull, replicates = 2)
#'  # putting them in a subdirectory
#'  sims <- experiment(mySimFull, replicates = 2, dirPrefix = c("expt"))
#'
#'  # Both replication and experiment, both params and modules
#'  # use a sub directory
#'  outputPath(mySimFull) <- file.path(tmpdir, "myExpt3")
#'  sims <- experiment(mySimFull, replicates = 2, params=experimentParams,
#'                     dirPrefix=c("expt", "simNum"))
#'
#'  #############################################################
#'  #############################################################
#'  # Use replication to build a probability surface.
#'  # For this to be meaningful, we need to provide a fixed landscape,
#'  #   not a randomLandscape for each experiment level. So requires 2 steps.
#'  # Step 1 - run randomLandscapes once to get a landscape map
#'  mySimRL <- simInit(
#'    times = list(start = 0.0, end = 0.1, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape"),
#'      # Turn off interactive plotting
#'      randomLandscapes = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("randomLandscapes"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = file.path(tmpdir, "landscapeMaps1")),
#'    outputs = data.frame(objectName="landscape", saveTime = 0, stringsAsFactors=FALSE)
#'  )
#'  # Run it twice to get two copies of the randomly generated landscape
#'  mySimRLOut <- experiment(mySimRL, replicate=2)
#'  #extract the landscape, which will be passed into next as an object
#'  landscape <- mySimRLOut[[1]]$landscape
#'
#'  mySimCarFir <- simInit(
#'    times = list(start = 0.0, end = 1, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      # Turn off interactive plotting
#'      fireSpread = list(.plotInitialTime=NA),
#'      caribouMovement = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("fireSpread", "caribouMovement"), # No randomLandscapes modules
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tmpdir),
#'    objects = c("landscape"), # Pass in the object here
#'    # Save final state of landscape and caribou
#'    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
#'  )
#'
#'  outputPath(mySimCarFir) <- file.path(tmpdir, "myExpt6")
#'  sims <- experiment(mySimCarFir, replicates = 8) # Run experiment
#'  attr(sims, "experiment") # shows the experiment, which in this case is just replicates
#'  # list all files that were saved called 'landscape'
#'  files1 <- dir(outputPath(mySimCarFir), recursive=TRUE, pattern="landscape", full.names=TRUE)
#'  # Read them in - alternatively, this could use the sims object directly also,
#'  #    e.g., sims[[1]]$landscape$Fires
#'  landscapes <- lapply(files1, readRDS)
#'  fires1 <- do.call(stack, lapply(landscapes, function(x) x$Fires)) # Extract just Fire layer
#'  fires1[fires1>0] <- 1 # convert to 1s and 0s
#'  fireProb <- sum(fires1)/nlayers(fires1) # sum them and convert to probability
#'  Plot(fireProb, new=TRUE)
#'
#'  ### Changing inputs
#'  mySimCarFir2 <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      # Turn off interactive plotting
#'      fireSpread = list(.plotInitialTime=NA),
#'      caribouMovement = list(.plotInitialTime=NA)
#'    ),
#'    modules = list("fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'                 outputPath = tmpdir),
#'    # Save final state of landscape and caribou
#'    outputs = data.frame(objectName=c("landscape", "caribou"), stringsAsFactors=FALSE)
#'  )
#'  landscapeFiles <- dir(tmpdir, pattern="landscape_year0", recursive=TRUE, full.names=TRUE)
#'  experiment(mySimCarFir2, params=experimentParams, modules=experimentModules, replicates = 2,
#'    inputs=lapply(landscapeFiles,function(filenames) {
#'      data.frame(file = filenames, loadTime=0, objectName= "landscape", stringsAsFactors = FALSE) })
#'   )
#'  exptDesign <- read.csv(file.path(tmpdir, "experiment.csv"))
#'  print(exptDesign)
#'
#'  # Use simple outputPath file names
#'  experiment(mySimFull, modules=experimentModules, substrLength=0)
#'  exptDesign <- read.csv(file.path(tmpdir, "experiment.csv"))
#'  print(exptDesign)
#'
#' # Remove all temp files
#' unlink(tmpdir, recursive=TRUE)
#' }
#'
setGeneric("experiment", function(sim, replicates = 1, params, modules,
                                  objects = list(), inputs,
                                  dirPrefix = "simNum", substrLength=3,
                                  saveExperiment=TRUE, experimentFile = "experiment",
                                  clearSimEnv=FALSE, ...) {
  standardGeneric("experiment")
})

#' @rdname experiment
setMethod(
  "experiment",
  signature(sim = "simList"),
  definition = function(sim, replicates, params, modules, objects, inputs,
                        dirPrefix, substrLength, saveExperiment,
                        experimentFile, clearSimEnv, ...) {

    if(missing(params)) params <- list()
    if(missing(modules)) modules <- list(unlist(SpaDES::modules(sim)[-(1:4)]))
    if(missing(inputs)) inputs <- list()

    cl <- tryCatch(getCluster(), error=function(x) NULL)
    on.exit(if(!is.null(cl)) returnCluster())

    #if(length(modules)==0) modules <- list(modules(sim)[-(1:4)])
    factorialExpList <- lapply(seq_along(modules), function(x) {
      paramsTmp <- pmatch(modules[[x]], names(params)) %>% na.omit
      factorsTmp <- if(NROW(paramsTmp)>0) {
        # unlist(params[paramsTmp], recursive = FALSE)
        lapply(params[paramsTmp], function(z) lapply(z, function(y) seq_along(y))) %>% unlist(recursive=FALSE)
      } else {
        params
      }

      if(length(inputs)>0) {
        inputsList <- list(inputs = seq_len(NROW(inputs)))
        factorsTmp <- append(factorsTmp, inputsList)
      }

      factorialExpInner <- expand.grid(factorsTmp, stringsAsFactors = FALSE)

      modulesShort <- paste(modules[[x]],collapse=",")
      if(NROW(factorialExpInner)>0) {
        factorialExpInner[["modules"]] <- x
      } else {
        factorialExpInner <- data.frame(modules=x, stringsAsFactors = FALSE)
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
      #if("modules" %in% mod) {
      #  paramValues$modules <- strsplit(paramValues$modules, split = ",")[[1]] %>%
      #    substr(1,substrLength) %>% paste(collapse=",")
      #}

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

      sim_ <- copy(sim)
      experimentDF <- data.frame(module=character(), param=character(),
                                 val=I(list()), modules=character(),
                                 input=data.frame(),
                                 expLevel=numeric(), stringsAsFactors = FALSE)
      for(x in seq_along(mod[!(mod %in% c("inputs"))])) {
        if(any(mod!="modules")) {
          y <- factorialExp[ind,names(paramValues)[x]]

          if(!is.na(y) & (mod[x]!="modules")) {
            val <- params[[mod[x]]][[param[[x]]]][[y]]
            params(sim_)[[mod[x]]][[param[[x]]]] <- val #factorialExp[ind,x]
            experimentDF <- rbindlist(l=list(experimentDF,
                                             data.frame(module=mod[x],param=param[x],
                                                        val=I(list(val)),
                                                        modules=paste0(unlist(modules[factorialExp[ind,"modules"]]), collapse=","),
                                                        input=if(length(inputs)>0) inputs[[factorialExp[ind,"inputs"]]] else NA,
                                                        expLevel=factorialExp[ind,"expLevel"],
                                                        stringsAsFactors = FALSE)),
                                      use.names = TRUE,
                                      fill=TRUE)
          }

        } else {
          experimentDF <- rbindlist(l=list(experimentDF,
                                           data.frame(modules=paste0(unlist(modules[factorialExp[ind,"modules"]]), collapse=","),
                                                      expLevel=factorialExp[ind,"expLevel"],
                                                      stringsAsFactors = FALSE)),
                                    use.names = TRUE,
                                    fill=TRUE)
        }
        if (!identical(sort(unlist(modules[factorialExp[ind,"modules"]])),
                         sort(unlist(SpaDES::modules(sim)[-(1:4)])))){ # test if modules are different from sim,
                      #  if yes, rerun simInit
          sim_ <- simInit(params=params(sim_),
                          modules=as.list(unlist(modules[factorialExp[ind,"modules"]])),
                          times=append(lapply(times(sim_)[2:3], as.numeric), times(sim_)[4]),
                          paths=paths(sim_),
                          outputs = outputs(sim_))

        }
      }

      # Deal with directory structures
      if(any(dirPrefix=="simNum")) {
        exptNum <- paddedFloatToChar(factorialExp$expLevel[ind], ceiling(log10(numExpLevels+1)))
      }
      dirPrefixTmp <- paste0(dirPrefix, collapse="")

      if((numExpLevels>1) & (substrLength>0)) {
        dirName <- paste(collapse = "-",substr(mod,1,substrLength),
                         substr(param, 1,substrLength),
                         paramValues, sep="_")
        dirName <- gsub(dirName, pattern="__", replacement = "_")
        if(any(dirPrefix=="simNum")) {
          dirPrefix <- gsub(dirPrefixTmp, pattern="simNum", replacement=exptNum)
        }
        if(any(dirPrefix!="")) {
          dirName <- paste(paste(dirPrefix, collapse=""), dirName, sep="_")
        }
      } else if (substrLength==0){
        if(any(dirPrefix!="")) {
          simplePrefix <- if(any(dirPrefix=="simNum")) exptNum else ""
          dirName <- gsub(dirPrefixTmp, pattern="simNum", replacement=simplePrefix)
        }
      } else {
        if(any(dirPrefix!="")) {
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
      newOutputPath <- file.path(paths(sim_)$outputPath,dirName) %>%
        gsub(pattern="/$", replacement="") %>% gsub(pattern="//", replacement="/")
      if(!dir.exists(newOutputPath)) dir.create(newOutputPath, recursive = TRUE)
      paths(sim_)$outputPath <- newOutputPath
      outputs(sim_)$file <- file.path(newOutputPath, basename(outputs(sim_)$file))
      if(length(inputs)>0) {
        inputs(sim_) <- inputs[[factorialExp[ind,"inputs"]]]
      }
      sim3 <- spades(sim_, ...)
      return(list(sim3, experimentDF))
    }

    if(!is.null(cl)) {
      parFun <- "clusterApplyLB"
      args <- list(x=1:NROW(factorialExp), fun=FunDef)
      args <- append(list(cl=cl), args)
      if(!is.na(pmatch("Windows",Sys.getenv("OS")))) {
        clusterEvalQ(cl, library(SpaDES))
      }

    } else {
      parFun <- "lapply"
      args <- list(X=1:NROW(factorialExp), FUN=FunDef)
    }

    expOut <- do.call(get(parFun), args)
    sims <- lapply(expOut, function(x) x[[1]])
    expDFs <- lapply(expOut, function(x) x[[2]])
    experimentDF <- rbindlist(expDFs, fill=TRUE, use.names=TRUE) %>%
      data.frame(stringsAsFactors=FALSE)

    keepCols <- which(apply(!is.na(experimentDF),2,all))

    experimentDF <- experimentDF[,keepCols]

    experiment = list(expDesign=factorialExp, expVals=experimentDF)

    # Factorial Levels are determined at this point. Save file.
    if(saveExperiment) {
      save(experiment, file = file.path(outputPath(sim), paste0(experimentFile, ".RData")))
    }
    attr(sims, "experiment") <- experiment
    if(clearSimEnv) {sims <- lapply(sims, function(x) {
      rm(list=ls(envir(x)), envir=envir(x))
      x
    })
    }
    return(invisible(sims))
  })

