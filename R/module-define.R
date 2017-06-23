################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependencies.
#' Packages are loaded during this call.
#'
#' @section Required metadata elements:
#'
#' \tabular{ll}{
#'    \code{name} \tab Module name. Must match the filename (without the \code{.R} extension).
#'                     This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{description} \tab Brief description of the module.
#'                            This is currently not parsed by SpaDES;
#'                            it is for human readers only. \cr
#'    \code{keywords} \tab Author-supplied keywords.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{childModules} \tab If this contains any character vector, then it will
#'                             be treated as a parent module. If this is a parent module,
#'                             then only this list entry will be read. For normal,
#'                             i.e., 'child modules', this should be \code{character(0)} or
#'                             \code{NA}.
#'                             If a character vector is provided, then these must be the
#'                             names of the modules located in the same file path as this
#'                             parent module that will be loaded during the \code{simInit}.\cr
#'    \code{authors} \tab Module author information (as a vector of \code{\link{person}}
#'                        objects. This is currently not parsed by SpaDES;
#'                        it is for human readers only.\cr
#'    \code{version} \tab Module version number (will be coerced to \code{\link{numeric_version}}
#'                        if a character or numeric are supplied).
#'                        The module developer should update manually this with each change
#'                        that is made to the module. See \url{http://semver.org/}
#'                        for a widely accepted standard for version numering.\cr
#'    \code{spatialExtent} \tab The spatial extent of the module supplied via
#'                              \code{raster::extent}. This is currently unimplemented.
#'                              Once implemented, this should define what spatial region this
#'                              module is scientifically reasonable to be used in.\cr
#'    \code{timeframe} \tab Vector (length 2) of POSIXt dates specifying the temporal extent
#'                          of the module. Currently unimplemented.
#'                          Once implemented, this should define what time frame this
#'                          module is scientifically reasonable to be used for.\cr
#'    \code{timeunit} \tab Time scale of the module (e.g., "day", "year"). This
#'                         MUST be specified. It indicates what '1' unit of time
#'                         means for this module. \code{SpaDES} interprets this
#'                         and if modules have different \code{timeunit} values
#'                         then it will correctly schedule each module, using the
#'                         smallest (currently the default) timeunit as the
#'                         'model' timeunit in the \code{spades} call.\cr
#'    \code{citation} \tab List of character strings specifying module citation information.
#'                         Alternatively, a list of filenames of \code{.bib} or similar files.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only.\cr
#'    \code{documentation} \tab List of filenames referring to module documentation sources.
#'                              This is currently not parsed by SpaDES;
#'                              it is for human readers only.\cr\cr
#'    \code{reqdPkgs} \tab List of R package names required by the module. These
#'                         packages will be loaded when \code{simInit} is called. \cr
#'    \code{parameters} \tab A data.frame specifying the parameters used in the module.
#'                           Usually produced by \code{rbind}-ing the outputs of multiple
#'                           \code{\link{defineParameter}} calls. These parameters indicate
#'                           the default values that will be used unless a module user
#'                           overrides them with the \code{params} argument in the
#'                           \code{\link{simInit}} call. The minimum and maximum are
#'                           currently used
#'                           by the \code{shine} function and the \code{POM} function, and they
#'                           should indicate the range of values that are reasonable
#'                           scientifically.\cr
#'    \code{inputObjects} \tab A \code{data.frame} specifying the data objects expected as
#'                             inputs to the module,
#'                             with columns \code{objectName} (class \code{character}),
#'                             \code{objectClass} (class \code{character}),
#'                             \code{sourceURL} (class \code{character}), and \code{other}
#'                              (currently spades does nothing with this column).
#'                             This data.frame identifies the objects that are expected,
#'                             but does not do any loading of
#'                             that object into the simList. The \code{sourceURL} gives
#'                             the developer the opportunity
#'                             to identify the source of a data file that can be used
#'                             with the model. This URL will be
#'                             used if the user calls \code{downloadData} (or
#'                             \code{downloadModule(..., data = TRUE)}. If the raw data
#'                             must be modified, the developer can use create a
#'                             function called \code{.inputObjects} in their module. That
#'                             function will be run during the \code{simInit} call. The
#'                             developer should ensure that if the object is supplied
#'                             by the module user as an argument in the \code{simInit}, then
#'                             the \code{.inputObjects} should not be run, i.e., use an
#'                             \code{(is.null(sim$xxx)))}.\cr
#'    \code{outputObjects} \tab A \code{data.frame} specifying the data objects output by
#'                              the module, with columns identical to those in
#'                              \code{inputObjects}. Like \code{inputObjects} above,
#'                              this only identifies the objects that this module will output
#'                              into the \code{simList}.
#'                              The module developer must create the necessary functions
#'                              that will cause these objects to be put into the
#'                              \code{simList}.\cr
#' }
#'
#' @inheritParams objs
#'
#' @param x A list with a number of named elements, referred to as the metadata. See details.
#'
#' @return Updated \code{simList} object.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster extent
#' @include simList-class.R
#' @rdname defineModule
#'
#' @examples
#' \dontrun{
#'   ## a default version of the defineModule is created with a call to newModule
#'   newModule("test", path = tempdir())
#'
#'   ## view the resulting module file
#'   #file.edit(file.path(tempdir(), "test", "test.R"))
#'
#'   # The default defineModule created by newModule is currently (SpaDES version 1.3.1.9044):
#'   defineModule(sim, list(
#'     name = "test",
#'     description = "insert module description here",
#'     keywords = c("insert key words here"),
#'     authors = c(person(c("First", "Middle"), "Last",
#'                        email = "email@example.com", role = c("aut", "cre"))),
#'     childModules = character(0),
#'     version = list(SpaDES = "1.3.1.9044", test = "0.0.1"),
#'     spatialExtent = raster::extent(rep(NA_real_, 4)),
#'     timeframe = as.POSIXlt(c(NA, NA)),
#'     timeunit = NA_character_, # e.g., "year",
#'     citation = list("citation.bib"),
#'     documentation = list("README.txt", "test.Rmd"),
#'     reqdPkgs = list(),
#'     parameters = rbind(
#'       #defineParameter("paramName", "paramClass", value, min, max,
#'       "parameter description")),
#'       defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first plot event should occur"),
#'       defineParameter(".plotInterval", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first plot event should occur"),
#'       defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first save event should occur"),
#'       defineParameter(".saveInterval", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first save event should occur")
#'     ),
#'     inputObjects = bind_rows(
#'       expectsInput(objectName = NA_character_, objectClass = NA_character_,
#'         sourceURL = NA_character_, desc = NA_character_, other = NA_character_)
#'     ),
#'     outputObjects = bind_rows(
#'       createsOutput(objectName = NA_character_, objectClass = NA_character_,
#'         desc = NA_character_, other = NA_character_)
#'     )
#'   ))
#'
#' }
#'
setGeneric("defineModule", function(sim, x) {
  standardGeneric("defineModule")
})

#' @export
#' @rdname defineModule
setMethod(
  "defineModule",
  signature(sim = ".simList", x = "list"),
  definition = function(sim, x) {
    # check that all metadata elements are present
    metadataRequired <- slotNames(new(".moduleDeps"))
    metadataProvided <- metadataRequired %in% names(x)
    metadataMissing <- metadataRequired[!metadataProvided]
    if (!all(metadataProvided)) {
      warning(paste0(
        "The \'", x$name, "\' module is missing the metadata for:\n",
        paste(" - ", metadataMissing, collapse = "\n"), "\n",
        "Please see ?defineModule and ?.moduleDeps for more info.\n",
        "All metadata elements must be present and valid."
      ))
    }

    # provide default values for missing metadata elements
    if (identical(x$reqdPkgs, list())) {
      x$reqdPkgs <- list()
    } else if (is.null(na.omit(x$reqdPkgs))) {
      x$reqdPkgs <- list()
    } else if (any(!nzchar(na.omit(x$reqdPkgs)))) {
      x$reqdPkgs <- list()
    } else {
      loadPackages(x$reqdPkgs)
    }

    ## enforce/coerce types for the user-supplied param list
    lapply(c("name", "description", "keywords"), function(z) {
      x[[z]] <<- if ( is.null(x[[z]]) || (length(x[[z]]) == 0) ) {
        NA_character_
      } else {
        as.character(x[[z]])
      }
    })

    x$childModules <- x$childModules %>% as.character() %>% na.omit() %>% as.character()

    x$authors <- if ( is.null(x$authors) || is.na(x$authors) ) {
      person("unknown")
    } else {
      as.person(x$authors)
    }

    ## maintain backwards compatibility with SpaDES versions prior to 1.3.1.9044
    ## where `version` was a single `numeric_version` value instead of named list
    x$version <- if (is.null(names(x$version))) {
      as.numeric_version(x$version) ## SpaDES < 1.3.1.9044
    } else {
      as.numeric_version(x$version[[x$name]]) ## SpaDES >= 1.3.1.9044
    }

    x$spatialExtent <- if (!is(x$spatialExtent, "Extent")) {
      if (is.null(x$spatialExtent)) {
        extent(rep(NA_real_, 4))
      } else {
        if (is.na(x$spatialExtent)) {
          extent(rep(NA_real_, 4))
        } else {
          extent(x$spatialExtent)
        }
      }
    }

    x$timeframe <- if ( is.null(x$timeframe) || is.na(x$timeframe) ) {
      as.POSIXlt(c(NA, NA))
    } else if (!is.numeric.POSIXt(x$timeframe)) {
      as.POSIXlt(x$timeframe)
    } %>% `[`(1:2)

    if ( is.null(x$timeunit) || is.na(x$timeunit) ) {
      x$timeunit <- NA_character_
    }

    lapply(c("citation", "documentation", "reqdPkgs"), function(z) {
      x[[z]] <<- if (is.null(x[[z]])) {
        list()
      } else {
        as.list(x[[z]])
      }
    })
    if ( is.null(x$parameters) ) {
      x$parameters <- defineParameter()
    } else {
      if ( is(x$parameters, "data.frame") ) {
        if ( !all(colnames(x$parameters) %in% colnames(defineParameter())) ||
             !all(colnames(defineParameter()) %in% colnames(x$parameters)) ) {
          stop("invalid data.frame `parameters` in module `", x$name, "`")
        }
      } else {
        x$parameters <- defineParameter()
      }
    }

    if (is.null(x$inputObjects)) {
      x$inputObjects <- .inputObjects()
    } else {
      if (is(x$inputObjects, "data.frame")) {
        if ( !all(colnames(x$inputObjects) %in% colnames(.inputObjects())) ||
             !all(colnames(.inputObjects()) %in% colnames(x$inputObjects)) ) {
          stop("invalid data.frame `inputObjects` in module `", x$name, "`:\n",
               "provided: ", paste(colnames(x$inputObjects), collapse = ", "),
               "expected: ", paste(colnames(.inputObjects()), collapse = ", "))
        }
      } else {
        x$inputObjects <- .inputObjects()
      }
    }
    if (NROW(x$inputObjects)) {
      if (is.null(x$inputObjects$sourceURL)) {
        x$inputObjects$sourceURL <- rep(NA_character_, NROW(x$inputObjects))
      }
      ids <- which(x$inputObjects$sourceURL == "")
      if (length(ids)) {
        x$inputObjects$sourceURL[ids] <- NA_character_
      }
    }

    if (is.null(x$outputObjects)) {
      x$outputObjects <- .outputObjects()
    } else {
      if (is(x$outputObjects, "data.frame")) {
        if ( !all(colnames(x$outputObjects) %in% colnames(.outputObjects())) ||
             !all(colnames(.outputObjects()) %in% colnames(x$outputObjects)) ) {
          stop("invalid data.frame `outputObjects` in module `", x$name, "`:",
               "provided: ", paste(colnames(x$outputObjects), collapse = ", "), "\n",
               "expected: ", paste(colnames(.outputObjects()), collapse = ", "))
        }
      } else {
        x$outputObjects <- .outputObjects()
      }
    }

    ## check that documentation actually exists locally
    docs <- sapply(x$documentation, na.omit) %>%
      (function(x) if (length(x)) character(0) else as.character(x))
    if (length(docs)) {
      lapply(docs, function(y) {
        if (!file.exists(file.path(modulePath(sim), y))) {
          stop("Module documentation file ", y, " not found in modulePath.")
        }
      })
    }

    ## check that children actually exist locally, and add to list of child modules
    if (length(x$childModules)) {
      lapply(x$childModules, function(y) {
        if (file.exists(file.path(modulePath(sim), y))) {
          z <- y %>% lapply(., `attributes<-`, list(type = "child"))
          modules(sim) <- append_attr(sim@modules, z)
        } else {
          stop("Module ", y, "(a child module of ", x$name, ") not found in modulePath.")
        }
      })
    }

    ## create module deps object and add to sim deps
    m <- do.call(new, c(".moduleDeps", x))
    return(.addDepends(sim, m))
})

################################################################################
#' Define a parameter used in a module
#'
#' Used to specify a parameter's name, value, and set a default.
#'
#' @param name      Character string giving the parameter name.
#' @param class     Character string giving the parameter class.
#' @param default   The default value to use when none is specified by the user.
#'                  Non-standard evaluation is used for the expression.
#' @param min       With \code{max}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#' @param max       With \code{min}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#' @param desc      Text string providing a brief description of the parameter.
#'
#' @return data.frame
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname defineParameter
#'
#' @examples
#' parameters = rbind(
#'   defineParameter("lambda", "numeric", 1.23, desc = "intrinsic rate of increase"),
#'   defineParameter("P", "numeric", 0.2, 0, 1, "probability of attack")
#' )
#'
setGeneric("defineParameter", function(name, class, default, min, max, desc) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character", default = "ANY",
                    min = "ANY", max = "ANY", desc = "character"),
          definition = function(name, class, default, min, max, desc) {

            # coerce `default`, `min`, and `max` to same specified type
            # These next lines commented out because it doesn't allow for character e.g.,
            #   start(sim)
            #default <- as(default, class)
            #min <- as(min, class)
            #max <- as(max, class)

            # previously used `substitute()` instead of `I()`,
            # but it did not allow for a vector to be passed with `c()`
            df <- data.frame(
              paramName = name, paramClass = class, default = I(list(default)),
              min = I(list(min)), max = I(list(max)), paramDesc = desc,
              stringsAsFactors = FALSE)
            return(df)
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character",
                    default = "ANY", min = "missing", max = "missing",
                    desc = "character"),
          definition = function(name, class, default, desc) {
            NAtypes <- c("character", "complex", "integer", "logical", "numeric")
            if (class %in% NAtypes) {
              # coerce `min` and `max` to same type as `default`
              min <- as(NA, class)
              max <- as(NA, class)
            } else {
              min <- NA
              max <- NA
            }

            df <- data.frame(
              paramName = name, paramClass = class, default = I(list(default)),
              min = I(list(substitute(min))), max = I(list(substitute(max))),
              paramDesc = desc, stringsAsFactors = FALSE
            )
            return(df)
})

#' @rdname defineParameter
setMethod(
  "defineParameter",
  signature(name = "missing", class = "missing", default = "missing",
            min = "missing", max = "missing", desc = "missing"),
  definition = function() {
    df <- data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE)
    return(df)
})

################################################################################
#' Define an input object that the module expects.
#'
#' Used to specify an input object's name, class, description, source url and
#' other specifications.
#'
#' @param objectName   Character string to define the input object's name.
#'
#' @param objectClass  Character string to specify the input object's class.
#'
#' @param desc         Text string providing a brief description of the input object.
#'
#' @param sourceURL    Character string to specify an URL to reach the input object,
#'                     default is \code{NA}.
#'
#' @param ...          Other specifications of the input object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{inputObjects} in a
#' module's metadata.
#'
#' @author Yong Luo
#' @docType methods
#' @export
#' @rdname expectsInput
#'
#' @examples
#' inputObjects <- dplyr::bind_rows(
#'   expectsInput(objectName = "inputObject1", objectClass = "character",
#'                desc = "this is for example", sourceURL = "not available"),
#'   expectsInput(objectName = "inputObject2", objectClass = "numeric",
#'                desc = "this is for example", sourceURL = "not available",
#'                otherInformation = "I am the second input object")
#' )
#'
setGeneric("expectsInput",
           function(objectName, objectClass, desc, sourceURL, ...) {
             standardGeneric("expectsInput")
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY", sourceURL = "ANY"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {
    return(expectsInput(as.character(objectName), as.character(objectClass),
                        as.character(desc), as.character(sourceURL), ...))
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "character"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {
    returnDataframe <- data.frame(cbind(objectName, objectClass, desc, sourceURL),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (length(templist) > 0) {
      for (i in 1:length(templist)) {
        returnDataframe <- data.frame(cbind(returnDataframe, I(list(templist[[i]])),
                                            stringsAsFactors = FALSE))
        names(returnDataframe)[ncol(returnDataframe)] <- names(templist)[i]
      }
    }
    return(returnDataframe)
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "missing"),
  definition = function(objectName, objectClass, desc, ...) {
    return(expectsInput(objectName, objectClass, desc, sourceURL = NA_character_, ...))
})

################################################################################
#' Define an output object of a module
#'
#' Used to specify an output object's name, class, description and other specifications.
#'
#' @param objectName   Character string to define the output object's name.
#'
#' @param objectClass  Character string to specify the output object's class.
#'
#' @param desc         Text string providing a brief description of the output object.
#'
#' @param ...          Other specifications of the output object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{outputObjects} in
#' a module's metadata.
#'
#' @author Yong Luo
#' @docType methods
#' @export
#' @rdname createsOutput
#'
#' @examples
#' outputObjects <- dplyr::bind_rows(
#'   createsOutput(objectName = "outputObject1", objectClass = "character",
#'                 desc = "this is for example"),
#'   createsOutput(objectName = "outputObject2", objectClass = "numeric",
#'                 desc = "this is for example",
#'                 otherInformation = "I am the second output object")
#' )
#'
setGeneric(
  "createsOutput",
  function(objectName, objectClass, desc, ...) {
    standardGeneric("createsOutput")
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY"),
  definition = function(objectName, objectClass, desc, ...) {
    return(createsOutput(as.character(objectName), as.character(objectClass),
                         as.character(desc)))
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character"),
  definition = function(objectName, objectClass, desc, ...) {
    returnDataframe <- data.frame(cbind(objectName, objectClass, desc),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (length(templist) > 0) {
      for (i in 1:length(templist)) {
        returnDataframe <- data.frame(cbind(returnDataframe, I(list(templist[[i]])),
                                            stringsAsFactors = FALSE))
        names(returnDataframe)[ncol(returnDataframe)] <- names(templist)[i]
      }
    }
    return(returnDataframe)
})

#' An internal function for coercing a data.frame to inputs()
#'
#' @param inputDF A data.frame with partial columns to pass to inputs<-
#' @param startTime Numeric time. The start(sim).
#'
#' @keywords internal
#' @rdname fillInputRows
.fillInputRows <- function(inputDF, startTime) {
  factorCols <- sapply(inputDF, is.factor)
  if (any(factorCols)) {
    inputDF[, factorCols] <- sapply(inputDF[, factorCols], as.character)
  }
  #fileTable <- .fileTableInCols ## not used??
  needRenameArgs <- grepl(names(inputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(inputDF)[needRenameArgs] <- .fileTableInCols[pmatch("arg", .fileTableInCols)]
  }
  columns <- pmatch(.fileTableInCols, names(inputDF))
  setnames(inputDF, old = colnames(inputDF)[na.omit(columns)],
           new = .fileTableInCols[!is.na(columns)])
  #columns2 <- pmatch(names(inputDF), .fileTableInCols) ## not used??
  if (any(is.na(columns))) {
    inputDF[, .fileTableInCols[is.na(columns)]] <- NA
  }

  if (any(is.na(inputDF[, "loadTime"]))) {
    inputDF[is.na(inputDF$loadTime), "loadTime"] <- startTime
  }

  if (any(is.na(inputDF[, "objectName"]))) {
    inputDF[is.na(inputDF$objectName), "objectName"] <- fileName(inputDF[is.na(inputDF$objectName), "file"])
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(inputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- inputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    inputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    inputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  objectsOnly <- is.na(inputDF[, "file"])
  if (!all(objectsOnly)) {
    inputDF2 <- inputDF[!objectsOnly, ]
    if (any(is.na(inputDF2[, "fun"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- na.omit(match(fileExt(fl), .fileExts[, "exts"]) )
      inputDF2$fun[is.na(inputDF2$fun)] <- .fileExts[exts, "fun"]
    }

    if (any(is.na(inputDF2[, "package"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- match(fileExt(fl), .fileExts[, "exts"])
      inputDF2$package[is.na(inputDF2$package)]  <- .fileExts[exts, "package"]
    }
    inputDF[!objectsOnly, ] <- inputDF2
  }
  return(inputDF)
}

#' An internal function for coercing a data.frame to outputs()
#'
#' @param outputDF A data.frame with partial columns to pass to outputs<-
#' @param endTime Numeric time. The end(sim).
#'
#' @keywords internal
#' @rdname fillOutputRows
.fillOutputRows <- function(outputDF, endTime) {
  needRenameArgs <- grepl(names(outputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(outputDF)[needRenameArgs] <-
      .fileTableOutCols[pmatch("arg", .fileTableOutCols)]
  }
  columns <- pmatch(.fileTableOutCols, names(outputDF))
  setnames(outputDF, old = colnames(outputDF)[na.omit(columns)],
           new = .fileTableOutCols[!is.na(columns)])
  #columns2 <- pmatch(names(outputDF), .fileTableOutCols)
  #object@outputs <- rbind(outputDF[,na.omit(columns), drop = FALSE], .fileTableOut()[,columns2])

  if (any(is.na(columns))) {
    outputDF[, .fileTableOutCols[is.na(columns)]] <- NA
  }
  if (any(is.na(outputDF[, "saveTime"]))) {
    outputDF[is.na(outputDF$saveTime), "saveTime"] <- endTime
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(outputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- outputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    outputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    outputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  if (any(is.na(outputDF[, "fun"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA=TRUE))) {
      outputDF$fun[is.na(fl) | (!nzchar(exts, keepNA=TRUE))] <- .fileExts$fun[1]
    }
    if (any(is.na(outputDF[, "fun"]))) {
      exts <- na.omit(match(exts, .fileExts[, "exts"]) )
      outputDF$fun[is.na(outputDF$fun)] <- .fileExts[exts, "fun"]
    }
  }

  if (any(is.na(outputDF[, "package"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA=TRUE))) {
      outputDF$package[is.na(fl) | (!nzchar(exts, keepNA=TRUE))] <- .fileExts$package[1]
    }
    if (any(is.na(outputDF[, "package"]))) {
      exts <- na.omit(match(fileExt(fl), .fileExts[, "exts"]) )
      outputDF$package[is.na(outputDF$package)] <- .fileExts[exts, "package"]
    }
  }
  return(outputDF)
}

