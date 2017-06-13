################################################################################
#' Find the number of layers in a Spatial Object
#'
#' There are already methods for \code{Raster*} in the raster package.
#' Adding methods for \code{list}, \code{SpatialPolygons}, \code{SpatialLines},
#' and \code{SpatialPoints}, \code{gg}, \code{histogram}, \code{igraph}.
#' These latter classes return \code{1}.
#'
#' @param x A \code{.spadesPlotObjects} object or list of these.
#'
#' @return The number of layers in the object.
#'
#' @export
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @rdname numLayers
#' @export
setGeneric("numLayers", function(x) {
  standardGeneric("numLayers")
})

#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "list",
  definition = function(x) {
    y <- sum(sapply(x, function(x) {
      if (is(x, "RasterStack")) {
        x <- numLayers(x)
      } else {
        x <- 1L
      }
      return(x)
    }))
    return(y)
})

#' @rdname numLayers
setMethod(
  "numLayers",
  signature = ".spadesPlot",
  definition = function(x) {
    return(length(x@arr@extents))
})

#' @importFrom raster nlayers
#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "Raster",
  definition = function(x) {
    return(nlayers(x))
})

#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "Spatial",
  definition = function(x) {
    return(1L)
})

#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "ANY",
  definition = function(x) {
    return(1L)
})

################################################################################
#' Extract the layer names of Spatial Objects
#'
#' There are already methods for \code{Raster*} objects. This adds methods for
#' \code{SpatialPoints*}, \code{SpatialLines*}, and \code{SpatialPolygons*},
#' returning an empty character vector of length 1.
#' This function was created to give consistent, meaningful results for all
#' classes of objects plotted by \code{Plot}.
#'
#' @param object  A \code{Raster*}, \code{SpatialPoints*}, \code{SpatialLines*},
#'                or \code{SpatialPolygons*} object; or list of these.
#'
#' @rdname layerNames
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @export
setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "list",
  definition = function(object) {
    unlist(lapply(object, layerNames))
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "ANY",
  definition = function(object) {
    return("")
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "Raster",
  definition = function(object) {
    names(object)
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = ".spadesPlot",
  definition = function(object) {
    return(sapply(object@spadesGrobList, function(x) {
      sapply(x, function(y)
        y@plotName)[[1]]
    }))
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "igraph",
  definition = function(object) {
    return("")
})

################################################################################
#' Assess whether a list of extents are all equal
#'
#' @param extents list of extents objects
#' @rdname equalExtent
#' @author Eliot McIntire
#' @export
setGeneric("equalExtent", function(extents) {
  standardGeneric("equalExtent")
})

#' @export
#' @rdname equalExtent
setMethod(
  "equalExtent",
  signature = "list",
  definition = function(extents) {
    all(
      c(
        sapply(extents, function(x) x@xmin) == extents[[1]]@xmin,
        sapply(extents, function(x) x@xmax) == extents[[1]]@xmax,
        sapply(extents, function(x) x@ymin) == extents[[1]]@ymin,
        sapply(extents, function(x) x@ymax) == extents[[1]]@ymax
      )
    )
})

################################################################################
#' Make a \code{.spadesPlot} class object
#'
#' Builds a \code{.spadesPlot} object from a list of objects.
#'
#' @param plotObjects list. Any plot objects.
#'
#' @param plotArgs list. Any arguments that the the grid package can accept for
#' the specific grob types, e.g., rasterGrob, polygonGrob, etc.
#'
#' @param whichSpadesPlottables  Logical indicating which objects in the
#' \code{Plot} call can be plotted by \code{Plot}.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @return A \code{\link{.spadesPlot}} object, which has 2 slots, one for the plot arrangement
#' (i.e., layout and dimensions) and onefor all of the \code{spadesGrobs}
#' (stored as a spadesGrobList of lists \code{.spadesGrob} objects).
#'
#' @include plotting-classes.R
#' @include plotting-helpers.R
#' @export
#' @author Eliot McIntire
#' @docType methods
#' @keywords internal
#' @rdname makeSpadesPlot
#'
setGeneric(".makeSpadesPlot", function(plotObjects, plotArgs, whichSpadesPlottables, ...) {
  standardGeneric(".makeSpadesPlot")
})

#' @export
#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "list", plotArgs = "list"),
  definition = function(plotObjects, plotArgs, whichSpadesPlottables, ...) {

    isSpatialObjects <- sapply(plotObjects, function(x) {
      is(x, "spatialObjects")
    })

    env <- list(...)$env
    suppliedNames <- names(plotObjects)
    if (any(!nzchar(suppliedNames, keepNA=TRUE))) {
      suppliedNames <- NULL
    }
    if (is.null(suppliedNames)) {
      objs <- objectNames()[whichSpadesPlottables]
    } else {
      objs <- lapply(suppliedNames, function(x) {
        list(objs = x, envs = env)
      })
    }

    names(plotObjects) <- sapply(objs, function(x)
      x$objs)

    if (!is.null(suppliedNames)) {
      if (all(sapply(suppliedNames, nzchar, keepNA=TRUE) )) {
        names(plotObjects)[!is.na(suppliedNames)] <- suppliedNames
      }
    }
    numberLayers <- pmax(1, sapply(plotObjects, numLayers))

    lNamesPlotObj <- layerNames(plotObjects)

    isSpadesPlot <- sapply(plotObjects, function(x) is(x, ".spadesPlot"))
    isStack <- sapply(plotObjects, function(x) is(x, "RasterStack"))

    # Stacks are like lists in that they are a single object, with many
    # layers.  Plot must treat these as any other layers, except that
    # they are stored in single objects. The following set of objects
    # are the "long" versions of the layers, i.e,. a call to say
    # Plot(stack1, layerB) would have two objects, but maybe 5 layers,
    # if the stack had 4 layers in it.
    isSpadesPlotLong <- rep(isSpadesPlot, numberLayers)
    isStackLong <- rep(isStack, numberLayers)
    isSpatialObjects <- rep(isSpatialObjects, numberLayers)

    lN <- rep(names(plotObjects), numberLayers)
    lN[isSpadesPlotLong] <- layerNames(plotObjects[isSpadesPlot])
    objectNamesLong <- rep(names(plotObjects), numberLayers)

    # Full layer names, including object name.
    # If layer name is same as object name omit it, and if layer name
    # is "layer", omit it if within a RasterLayer
    lN[isStackLong] <- paste(objectNamesLong[isStackLong],
                             lNamesPlotObj[isStackLong],
                             sep = "$")
    names(lN) <- rep(names(plotObjects), numberLayers)
    names(lN)[isSpadesPlotLong] <- layerNames(plotObjects)[isSpadesPlotLong]

    # Create long version of environments
    lEnvs <- rep(sapply(objs, function(x) x$envs), numberLayers)

    plotArgs <- .makeList(plotArgs, length(lN))

    # Make new .spadesPlot object.
    # This will be merged to existing later.
    newPlots <- new(".spadesPlot")

    newPlots@arr <- new(".arrangement")

    newPlots@spadesGrobList <- lapply(1:length(lN), function(x) {
      spadesGrobList <- list()

      if (isSpadesPlotLong[x]) {
        spadesGrobList[[lN[x]]] <-
          plotObjects[[match(
            names(isSpadesPlotLong)[x],
            names(plotObjects)
          )]]@spadesGrobList[[match(
            lN[x], lNamesPlotObj[isSpadesPlotLong]
          )]][[1]]
      } else {
        spadesGrobList[[lN[x]]] <- new(".spadesGrob")
        spadesGrobList[[lN[x]]]@plotArgs <- lapply(plotArgs, function(y) y[[x]])
        spadesGrobList[[lN[x]]]@plotArgs$gpText <- plotArgs$gpText[x]
        spadesGrobList[[lN[x]]]@plotArgs$gpAxis <- plotArgs$gpAxis[x]
        spadesGrobList[[lN[x]]]@plotArgs$gp <- plotArgs$gp[x]
        spadesGrobList[[lN[x]]]@plotName <- lN[x]
        spadesGrobList[[lN[x]]]@objName <- objectNamesLong[x]
        spadesGrobList[[lN[x]]]@envir <- lEnvs[[x]]
        spadesGrobList[[lN[x]]]@layerName <- lNamesPlotObj[x]
        spadesGrobList[[lN[x]]]@objClass <- class(
          eval(parse(text = objectNamesLong[x]), lEnvs[[x]])
        )
        spadesGrobList[[lN[x]]]@isSpatialObjects <- isSpatialObjects[x]
      }
      return(spadesGrobList)
    })

    names(newPlots@spadesGrobList) <- lN
    return(newPlots)
})

#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "list", plotArgs = "missing"),
  definition = function(plotObjects, ...) {
    plotArgs <- formals("Plot")[-1]
    newPlots <- .makeSpadesPlot(plotObjects, plotArgs, ...)
    return(newPlots)
})

#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "missing", plotArgs = "missing"),
  definition = function(...) {
    newPlots <- new(".spadesPlot")
    newPlots@spadesGrobList <- lapply(1:1, function(x) {
      spadesGrobList <- list()
      spadesGrobList[[1]] <- new(".spadesGrob")
      return(spadesGrobList)
    })
    return(newPlots)
})

################################################################################
#' Convert \code{plotArgs} to list of lists
#'
#' Internal function. Take the inputs as plotArgs to the Plot function, and make
#' them a list of length \code{numSpadesPlotObjects} entries of lists.
#'
#' @inheritParams .makeSpadesPlot
#'
#' @param numSpadesPlotObjects Numeric. The number of \code{.spadesPlotObjects}.
#'                 This can't easily be deduced from the \code{plotArgs} because
#'                 of the \code{RasterStack}s. So passed manually.
#'
#' @author Eliot McIntire
#' @docType methods
#' @include plotting-classes.R
#' @keywords internal
#' @rdname makeList
#'
setGeneric(".makeList", function(plotArgs, numSpadesPlotObjects) {
  standardGeneric(".makeList")
})

#' @rdname makeList
setMethod(
  ".makeList",
  signature = c("list"),
  definition = function(plotArgs, numSpadesPlotObjects) {
    p <- plotArgs
    n <- numSpadesPlotObjects

    p$new <- if (is.list(p$new)) {
      if (length(p$new) != n) {
        rep(p$new, length.out = n)
      } else {
        p$new
      }
    } else {
      if (length(p$new) == n) {
        as.list(p$new)
      } else {
        rep(list(p$new), length.out = n)
      }
    }

    # character or logical or numeric of length 1 per entry
    p$addTo <- if (is.list(p$addTo)) {
      if (length(p$addTo) != n) {
        rep(p$addTo, length.out = n)
      } else {
        p$addTo
      }
    } else {
      if (length(p$addTo) == n) {
        as.list(p$addTo)
      } else {
        rep(list(p$addTo), length.out = n)
      }
    }

    p$gp <- if (is(p$gp, "gpar")) {
      rep(list(p$gp), n)
    } else {
      if (is.list(p$gp)) {
        rep(p$gp, n)
      }
    }

    p$gpText <- if (is(p$gpText, "gpar")) {
      rep(list(p$gpText), n)
    } else {
      if (is.list(p$gpText)) {
        rep(p$gpText, n)
      }
    }

    p$gpAxis <- if (is(p$gpAxis, "gpar")) {
      rep(list(p$gpAxis), n)
    } else {
      if (is.list(p$gpAxis)) {
        rep(p$gpAxis, n)
      }
    }

    p$axes <- if (is.list(p$axes)) {
      if (length(p$axes) != n) {
        rep(p$axes, length.out = n)
      } else {
        p$axes
      }
    } else {
      if (length(p$axes) == n) {
        as.list(p$axes)
      } else {
        rep(list(p$axes), length.out = n)
      }
    }

    p$speedup <- if (is.list(p$speedup)) {
      if (length(p$speedup) != n) {
        rep(p$speedup, length.out = n)
      }
      else {
        p$speedup
      }
    } else {
      if (length(p$speedup) == n) {
        as.list(p$speedup)
      } else {
        rep(list(p$speedup), length.out = n)
      }
    }

    p$size <- if (is.list(p$size)) {
      if (length(p$size) != n) {
        rep(p$size, length.out = n)
      } else {
        p$size
      }
    } else {
      if (length(p$size) == n) {
        as.list(p$size)
      } else {
        rep(list(p$size), length.out = n)
      }
    }

    p$visualSqueeze <- if (is.list(p$visualSqueeze)) {
      if (length(p$visualSqueeze) != n) {
        rep(p$visualSqueeze, length.out = n)
      } else {
        p$visualSqueeze
      }
    } else {
      if (length(p$visualSqueeze) == n) {
        as.list(p$visualSqueeze)
      } else {
        rep(list(p$visualSqueeze), length.out = n)
      }
    }

    p$legend <- if (is.list(p$legend)) {
      if (length(p$legend) != n) {
        rep(p$legend, length.out = n)
      } else {
        p$legend
      }
    } else {
      if (length(p$legend) == n) {
        as.list(p$legend)
      } else {
        rep(list(p$legend), length.out = n)
      }
    }

    p$pch <- if (is.list(p$pch)) {
      if (length(p$pch) != n) {
        rep(p$pch, length.out = n)
      } else {
        p$pch
      }
    } else {
      if (length(p$pch) == n) {
        as.list(p$pch)
      } else {
        rep(list(p$pch), length.out = n)
      }
    }

    p$title <- if (is.list(p$title)) {
      if (length(p$title) != n) {
        rep(p$title, length.out = n)
      } else {
        p$title
      }
    } else {
      if (length(p$title) == n) {
        as.list(p$title)
      } else {
        rep(list(p$title), length.out = n)
      }
    }

    p$na.color <- if (is.list(p$na.color)) {
      if (length(p$na.color) != n) {
        rep(p$na.color, length.out = n)
      } else {
        p$na.color
      }
    } else {
      if (length(p$na.color) == n) {
        as.list(p$na.color)
      } else {
        rep(list(p$na.color), length.out = n)
      }
    }

    p$zero.color <- if (is.list(p$zero.color)) {
      if (length(p$zero.color) != n) {
        rep(p$zero.color, length.out = n)
      } else {
        p$zero.color
      }
    } else {
      if (length(p$zero.color) == n) {
        as.list(p$zero.color)
      } else {
        rep(list(p$zero.color), length.out = n)
      }
    }

    p$cols <- if (is.list(p$cols)) {
      p$cols
    } else {
      rep(list(p$cols), length.out = n)
    }

    p$zoomExtent <- if (is.list(p$zoomExtent)) {
      p$zoomExtent
    } else {
      rep(list(p$zoomExtent), length.out = n)
    }

    p$legendText <- if (is.list(p$legendText)) {
      p$legendText
    } else {
      rep(list(p$legendText), length.out = n)
    }

    p$legendRange <- if (is.list(p$legendRange)) {
      p$legendRange
    } else {
      rep(list(p$legendRange), length.out = n)
    }

    p$plotFn <- if (is.list(p$plotFn)) {
      p$plotFn
    } else {
      rep(list(p$plotFn), length.out = n)
    }

    return(p)
})

################################################################################
#' Make \code{SpatialLines} object from two \code{SpatialPoints} objects
#'
#' The primary conceived usage of this is to draw arrows following the
#' trajectories of agents.
#'
#' @param from  Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to    Ending spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @return A \code{SpatialLines} object. When this object is used within a
#'         \code{Plot} call and the \code{length} argument is specified, then
#'         arrow heads will be drawn. See examples.
#'
#' @include plotting-classes.R
#' @importFrom raster crs
#' @importFrom sp coordinates Line Lines SpatialLines
#' @export
#' @docType methods
#' @rdname makeLines
#' @author Eliot McIntire
#'
#' @examples
#' library(sp)
#' # Make 2 objects
#' caribou1 <- SpatialPoints(cbind(x = stats::runif(10, -50, 50), y = stats::runif(10, -50, 50)))
#' caribou2 <- SpatialPoints(cbind(x = stats::runif(10, -50, 50), y = stats::runif(10, -50, 50)))
#'
#' caribouTraj <- makeLines(caribou1, caribou2)
#'   if (interactive()) {
#'     clearPlot()
#'     Plot(caribouTraj, length = 0.1)
#'   }
#'
#' # or  to a previous Plot
#' \dontrun{
#' filelist <- data.frame(files =
#'      dir(file.path(find.package("SpaDES", quiet = FALSE), "maps"),
#'          full.names = TRUE, pattern = "tif"),
#'      functions = "rasterToMemory",
#'      packages = "SpaDES")
#'
#' # Load files to memory (using rasterToMemory)
#' sim1 <- loadFiles(filelist = filelist)
#' caribouTraj <- makeLines(caribou1, caribou2)
#'
#'   if (interactive()) {
#'     clearPlot()
#'     Plot(sim1$DEM)
#'     Plot(caribouTraj, addTo = "sim1$DEM", length = 0.1)
#'   }
#' }
#'
setGeneric("makeLines", function(from, to) {
  standardGeneric("makeLines")
})

#' @export
#' @rdname makeLines
setMethod(
  "makeLines",
  signature = c("SpatialPoints", "SpatialPoints"),
  definition = function(from, to) {
    SpatialLines(lapply(seq_len(length(from)), function(x) {
      Lines(list(Line(
        coords = rbind(coordinates(from)[x, ], coordinates(to)[x, ])
      )), ID = x)
    }), proj4string = crs(from))
})

################################################################################
#' Parse arguments and find environments
#'
#' Internal function used within objectNames.
#'
#' @param y  A character representation of the arguments passed to a function,
#'           e.g., \code{Plot}.
#'
#' @param e  Environment in which the function (e.g., \code{Plot}) was called.
#'
#' @param eminus1  The parent environment of \code{e}.
#'
#' @return A list of length 2, with names \code{objs} and \code{envs} giving the
#' standardized representation (i.e., replacing \code{[[]]} with \code{$}
#' notation for objects) of objects and their layers (if \code{RasterStacks}).
#'
#' @docType methods
#' @importFrom grDevices dev.cur
#' @include plotting-classes.R
#' @keywords internal
#' @rdname parseArgs
#' @author Eliot McIntire and Alex Chubaty
#'
# igraph exports %>% from magrittr
.parseArgs <- function(y, e, eminus1) {
  elems <- list()
  i <- 1
  parseTxt <- parse(text = y)[[1]]
  elems[[i]] <- parseTxt
  lastOneDone <- TRUE

  while (length(parse(text = deparse(parseTxt))[[1]]) != 1) {
    if (length(parseTxt) == 2) {
      stop("Please pass an object directly, or use get(x, envir = envName) or eval(x, envir = envName). ",
           "Plot can not yet accept functions or complex objects internally.")
    }

    lastOneDone <- FALSE
    if (grepl(deparse(parseTxt[[1]]), pattern = "^eval")) {
      callEnv <- tryCatch(
        eval(
          match.call(definition = eval, call = parseTxt)$envir,
          envir = eminus1
        ),
        error = function(x) {
          tryCatch(
            eval(
              match.call(definition = eval, call = parseTxt)$envir,
              envir = e
            ),
            error = function(x) .GlobalEnv
          )
        }
      )

      parseTxt[[3]] <- match.call(definition = eval, call = parseTxt)$expr
      if (is.name(match.call(definition = parse, call = parseTxt[[3]])$text)) {
        parseTxt <- parseTxt[[3]]
        parseTxt[[3]] <- match.call(definition = parse, call = parseTxt)$text
      }
      lastOneDone <- TRUE
    }
    if (is.call(parseTxt[[3]])) {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (as.character(parseTxt[[1]]) == "[[") {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (grepl(deparse(parseTxt[[1]]), pattern = "^get")) {
      callEnv <- tryCatch(
        eval(
          match.call(definition = eval,
                     call = parseTxt)$envir,
          envir = eminus1
        ),
        error = function(x) {
          tryCatch(
            eval(
              match.call(definition = eval, call = parseTxt)$envir,
              envir = e
            ),
            error = function(x) .GlobalEnv
          )
        }
      )
      parseTxt[[3]] <- match.call(definition = get, call = parseTxt)$x
      tmpParseTxt3 <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )

      lastOneDone <- TRUE
      parseTxt[[3]] <- tmpParseTxt3
    }
    if (is.character(parseTxt[[3]])) {
      parseTxt[[3]] <- as.name(parseTxt[[3]])
    }
    if (is.numeric(parseTxt[[3]])) {
      if (!is.null(names(eval(parseTxt[[2]], envir = e)))) {
        parseTxt[[3]] <- names(eval(parseTxt[[2]], envir = e))[parseTxt[[3]]]
        if (is.na(parseTxt[[3]])) {
          stop("Please pass an object directly, or use get(x, envir = envName) or eval(x, envir = envName). ",
               "Plot can not yet accept functions or complex objects internally.")
        }
      }

    }

    # override previous elems entry if length(parseTxt)>1:
    elems[[i]] <- parseTxt[[3]]

    # if evaluating the parsed text is a character,
    # then this is likely then name we want to keep:
    isChar <- tryCatch(
      is(eval(elems[[i]], envir = eminus1), "character"),
      error = function(x) FALSE
    )
    if (isChar) {
      elems[[i]] <- as.name(eval(elems[[i]], envir = eminus1))
    }
    parseTxt <- parse(text = deparse(parseTxt[[2]]))[[1]]
    i <- i + 1
  }

  deparsedTxt <- deparse(parseTxt)
  sframes <- sys.frames()
  envs <- append(.GlobalEnv, sframes) %>%
    .[c(TRUE, sapply(sframes, function(x) {
      exists(deparsedTxt, envir = x, inherits = FALSE)
    }))] %>%
    .[[length(.)]]

  inGlobal <- identical(envs, .GlobalEnv)
  if (is(eval(parse(text = deparsedTxt), envir = envs), "environment")) {
    envs <- eval(parse(text = deparsedTxt), envir = envs)
  } else {
    if (!lastOneDone) elems[[i]] <- parseTxt
  }
  if (exists("callEnv", inherits = FALSE)) {
    envs <- callEnv
  }

  if (!inGlobal) {
    if (!exists(paste0("dev", dev.cur()), envir = .spadesEnv)) {
      .spadesEnv[[paste0("dev", dev.cur())]] <- new.env(parent = emptyenv())
    }

    tmp <- get(deparse(rev(elems)[[1]]), envir = envs) ## the sim object
    if (is(tmp, "simList")) {
      useElem <- 1
      # If the user is passing a sub-element to say a Raster Stack
      if (length(rev(elems)[-1]) > 1) {
        # Only RasterStack implemented yet
        if (is(get(deparse(rev(elems)[[2]]), envir = envir(tmp)), "RasterStack")) {
          useElem <- 2
        }
      }
      changeObjEnv(deparse(elems[[useElem]]),
                   fromEnv = envir(tmp),
                   toEnv = .spadesEnv[[paste0("dev", dev.cur())]])
    } else {
      ## if it is NOT a simList.
      changeObjEnv(paste(sapply(rev(elems), deparse), collapse = "$"),
                   fromEnv = envs, toEnv = .spadesEnv[[paste0("dev", dev.cur())]])
    }
  }

  if (sapply(elems[[1]], is.numeric)) {
    return(list(objs = paste0(paste0(sapply(rev(elems), deparse),
                                     collapse = "[["), "]]"),
                envs = envs))
  }
  return(list(objs = paste(sapply(rev(elems), deparse), collapse = "$"),
              envs = envs))

}

################################################################################
#' Extracts the object names
#'
#' Internal function primarily used from \code{Plot}.
#'
#' @param calledFrom  character vector of length 1, indicating which function
#'                    call is desired. Defaults to \code{Plot}.
#'
#' @param argClass    character vector of length 1, indicating which class is
#'                    being searched for among the arguments.
#'                    Defaults to \code{.spadesPlotObjects}.
#'
#' @param argName     character vector of length 1, or \code{NULL}, indicating
#'                    if the arguments to select have a name, no name (empty
#'                    string), or do not use name (\code{NULL}).
#'
#' @return \code{NULL}. This function is invoked for its side effects.
#'
#' @include plotting-classes.R
#' @docType methods
#' @keywords internal
#' @rdname objectNames
#' @author Eliot McIntire
#'
objectNames <- function(calledFrom = "Plot",
                        argClass = ".spadesPlotObjects",
                        argName = "") {
  scalls <- sys.calls()
  # Extract from the sys.calls only the function "calledFrom"
  frameCalledFrom <- which(sapply(scalls, function(x) {
    grepl(x, pattern = paste0("^", calledFrom, "$"))[1]
  }))
  e <- sys.frame(frameCalledFrom[1])
  eminus1 <- sys.frame(frameCalledFrom - 1)

  if (!nzchar(argName, keepNA=TRUE)) {
    callNamedArgs <- as.character(substitute(list(...), env = e))[-1]
  } else {
    callNamedArgs <- as.character(substitute(parse(text = sim), env = e))[-1]
  }
  objs <- lapply(callNamedArgs, .parseArgs, e, eminus1)
  return(objs)
}

#' Importing some grid functions
#'
#' Currently only the gpar function is imported. This is a convenience so that users
#' can change \code{Plot} arguments without having to load the entire grid package.
#'
#' @inheritParams grid::gpar
#' @name gpar
#' @aliases gpar
#' @importFrom grid gpar
#' @export
#' @rdname grid-functions
#' @seealso \code{\link[grid]{gpar}}
setGeneric("gpar", function(...) {
  standardGeneric("gpar")
})

#' @export
#' @rdname grid-functions
setMethod("gpar",
          definition = function(...) {
            return(grid::gpar(...))
})

################################################################################
#' Internal functions used by Plot
#'
#' Not meant to be used by user.
#'
#' @param grobToPlot Graphical object to plot
#' @param sGrob spadesGrob object
#' @param takeFromPlotObj Logical indicating whether data for grobToPlot should be found in
#'        current call to Plot or from disk
#' @param arr An arrangement object
#' @param newArr Logical, whether there is a new arrangement happening
#' @param spadesGrobCounter Numeric. A counter. No meaning outside Plot function.
#' @param subPlots Character. Name of plot area.
#' @param cols Color vector.
#'
#' @include plotting-classes.R
#' @docType methods
#' @keywords internal
#' @name .convertSpatialToPlotGrob
#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @author Eliot McIntire
#'
setGeneric(".convertSpatialToPlotGrob", function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                                                 spadesGrobCounter, subPlots, cols) {
  standardGeneric(".convertSpatialToPlotGrob")
})

#' @aliases PlotHelpers
#' @keywords internal
#' @rdname Plot-internal
setMethod(
  ".convertSpatialToPlotGrob",
  signature = c("spatialObjects", ".spadesGrob"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        spadesGrobCounter, subPlots, cols) {
    if (is(grobToPlot, "Raster")) {
      # Rasters may be zoomed into and subsampled and have unique legend
      #            if (sGrob@plotArgs$new)
      pR <- .prepareRaster(grobToPlot, sGrob@plotArgs$zoomExtent,
                           sGrob@plotArgs$legendRange, takeFromPlotObj,
                           arr, sGrob@plotArgs$speedup, newArr = newArr)
      zMat <- .makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                               pR$legendRange,
                               na.color = sGrob@plotArgs$na.color,
                               zero.color = sGrob@plotArgs$zero.color,
                               cols = sGrob@plotArgs$cols,
                               skipSample = pR$skipSample)
    } else if (is(grobToPlot, "SpatialPoints")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }
      # This handles SpatialPointsDataFrames with column "color"
      if (any(grepl(pattern = "color", colnames(grobToPlot))) & is.null(cols))
        sGrob@plotArgs$cols <- getColors(grobToPlot)

      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    } else if (is(grobToPlot, "SpatialPolygons")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }
      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)

    } else if (is(grobToPlot, "SpatialLines")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }
      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    }
    return(zMat)
})

################################################################################
#' @param whPlotFrame Numeric. Which plot within the spadesGrobPlots object.
#'
#' @include plotting-classes.R
#' @docType methods
#' @aliases PlotHelpers
#' @keywords internal
#' @name .xyAxes
#' @rdname Plot-internal
#'
setGeneric(".xyAxes", function(sGrob, arr, whPlotFrame) {
  standardGeneric(".xyAxes")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @keywords internal
setMethod(
  ".xyAxes",
  signature = c(".spadesGrob", ".arrangement"),
  definition = function(sGrob, arr, whPlotFrame) {
    if (sGrob@plotArgs$axes == "L") {
      if (sGrob@objClass == "Raster" &
          (arr@extents[(whPlotFrame - 1) %% arr@columns + 1][[1]] ==
           arr@extents[max(
             which(
               (1:length(arr@names) - 1) %% arr@columns + 1 ==
               (whPlotFrame - 1) %% arr@columns + 1
             ))][[1]])) {
        if (whPlotFrame > (length(arr@names) - arr@columns)) {
          xaxis <- TRUE
        } else {
          xaxis <- FALSE
        }
      } else {
        # not the same extent as the final one in the column
        xaxis <- TRUE
      }
    } else {
      xaxis <- sGrob@plotArgs$axes
    }

    if (sGrob@plotArgs$axes == "L") {
      if (sGrob@objClass == "Raster" & (arr@extents[whPlotFrame][[1]] ==
                                      arr@extents[(ceiling(whPlotFrame / arr@columns) - 1) *
                                                  arr@columns + 1][[1]])) {
        if ((whPlotFrame - 1) %% arr@columns == 0) {
          yaxis <- TRUE
        } else {
          yaxis <- FALSE
        }
      } else {
        yaxis <- TRUE
      }
    } else {
      yaxis <- sGrob@plotArgs$axes
    }

    return(list(x = xaxis, y = yaxis))
})

#' @param spadesSubPlots List of many spadesGrobs
#' @param isBaseSubPlot Logical. Is the currently being plotted object a base layer
#' @param isNewPlot Logical. Is the currently being plotted object a new, additional plot
#' @param isReplot Logical. Is the currently being plotted object a replot due to something
#'                 like a rearrangement
#' @param zMat List resulting from \code{.convertSpatialToPlotGrob}
#' @param wipe Logical. Is the currently being plotted object require a white rectangle to
#'             be plotted first, and subsequent other changes.
#' @param xyAxes List of length 2, resulting from \code{.xyAxes}
#' @inheritParams Plot
#' @param vps A viewport tree resulting from \code{.makeViewports}
#' @param nonPlotArgs Arguments passed to \code{Plot} that are not \code{.spadesPlottables},
#'                    but are passed along with \code{.spadesPlottables}.
#'
#' @include plotting-classes.R
#' @importFrom grid seekViewport grid.text
#' @docType methods
#' @aliases PlotHelpers
#' @keywords internal
#' @name .Plot
#' @rdname Plot-internal
#'
setGeneric(".Plot", function(sGrob, grobToPlot, subPlots, spadesSubPlots, spadesGrobCounter,
                             isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                             vps, nonPlotArgs) {
  standardGeneric(".Plot")
})

#' @rdname Plot-internal
#' @importFrom raster crop is.factor
#' @aliases PlotHelpers
setMethod(
  ".Plot",
  signature = c(".spadesGrob"),
  definition = function(sGrob, grobToPlot, subPlots, spadesSubPlots, spadesGrobCounter,
                        isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                        vps, nonPlotArgs) {
    seekViewport(subPlots, recording = FALSE)

    if (is.list(grobToPlot)) {
      # This is for base plot calls... the grobToPlot is a call i.e,. a name
      # Because base plotting is not set up to overplot,
      # must plot a white rectangle
      par(fig = gridFIG())
      sGrob@plotArgs[names(grobToPlot)] <- grobToPlot

      # clear out all arguments that don't have meaning in plot.default
      if (is(grobToPlot, "gg")) {
        print(grobToPlot, vp = subPlots)
        a <- try(seekViewport(subPlots, recording = FALSE))
      } else {
        # plot y and x axes should use deparse(substitute(...)) names
        if (!identical(FALSE, sGrob@plotArgs$axes)) {
          if (!is.na(sGrob@plotArgs$axisLabels["x"])) {
            sGrob@plotArgs$xlab <- sGrob@plotArgs$axisLabels["x"]
          } else {
            if (!is.na(sGrob@plotArgs$axisLabels[1]))
              sGrob@plotArgs$xlab <- sGrob@plotArgs$axisLabels[1]
            else
              sGrob@plotArgs$xlab <- NULL
          }
          if (!is.na(sGrob@plotArgs$axisLabels["y"])) {
            sGrob@plotArgs$ylab <- sGrob@plotArgs$axisLabels["y"]
          } else {
            if (!is.na(sGrob@plotArgs$axisLabels[2]))
              sGrob@plotArgs$ylab <- sGrob@plotArgs$axisLabels[2]
            else
              sGrob@plotArgs$ylab <- NULL
          }
        } else {
          sGrob@plotArgs$xlab <- ""
          sGrob@plotArgs$ylab <- ""
        }

        isHist <- FALSE
        if (!is.null(grobToPlot$x)) {
          if (is(grobToPlot$x, "histogram")) {
            isHist <- TRUE
            sGrob@plotArgs$ylab <- if (is.null(sGrob@plotArgs$ylab)) "Frequency"
          } else if (is(grobToPlot$x, "numeric")) {
            if (length(sGrob@plotArgs$axisLabels) == 1) {
              sGrob@plotArgs$ylab <- sGrob@plotArgs$xlab
              sGrob@plotArgs$xlab <- "Index"
            }
          }
        }

        args_plot1 <- sGrob@plotArgs[!(names(sGrob@plotArgs) %in%
                                         c("new", "addTo", "gp", "gpAxis", "axisLabels",
                                         "zoomExtent", "gpText", "speedup", "size",
                                         "cols", "visualSqueeze", "legend", "legendRange",
                                         "legendText", "zero.color", "length", "arr",
                                         "na.color", "title", "userProvidedPlotFn"))]
        args_plot1$axes <- isTRUE(sGrob@plotArgs$axes)
        makeSpaceForAxes <- as.numeric(
          !identical(FALSE, spadesSubPlots[[subPlots]][[1]]@plotArgs$axes)
        )
        par(plt = c(0.18 + makeSpaceForAxes * 0.05, # left
                    0.95,                          # right
                    0.25 + makeSpaceForAxes * 0.05, # bottom
                    0.9))                         # top

        plotFn <- args_plot1$plotFn
        args_plot1$plotFn <- NULL

        # The actuall plot calls for base plotting
        if (is(grobToPlot, "igraph")) {
          # this next is a work around that I can't understand
          if (names(dev.cur()) == "null device") {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            clearPlot()
          }
          #a <- try(seekViewport(subPlots, recording = FALSE))
          suppressWarnings(par(new = TRUE))
          #plotCall <- append(list(x = grobToPlot), nonPlotArgs)
          plotCall <- list(x = grobToPlot)
          suppressWarnings(do.call(plot, args = plotCall))

        } else if (spadesGrobCounter == 1 | wipe | isHist) {
          suppressWarnings(par(new = TRUE))
          #args_plot1 <- lapply(args_plot1, unname)


          # This is a work around because I am not able to generically
          #  assess the formals of a function to remove any that aren't
          #  defined for that method... i.e., plot is the generic, but
          #  plot.igraph has different formals. Some of the functions
          #  are not exported, so their formals can't be found algorithmically
          tryCatch(do.call(plotFn, args = args_plot1), error = function(x) {
            parsRm <- unlist(strsplit(gsub(x,
                                           pattern = ".*Unknown plot parameters: ",
                                           replacement = ""), split = ", "))
            parsRm <- gsub(parsRm, pattern = "\n", replacement = "")
            args_plot1 <- args_plot1[!(names(args_plot1) %in% parsRm)]
            do.call(plotFn, args = args_plot1)
          })
          #suppressWarnings(do.call(plotFn,
          #                         args = args_plot1))

        } else {
          # adding points to a plot
          #suppressWarnings(par(new = TRUE))
          tmpPlotFn <- if (plotFn == "plot") "points" else plotFn
          args_plot1[c("axes", "xlab", "ylab", "plotFn")] <- NULL
          suppressWarnings(do.call(tmpPlotFn, args = args_plot1))
        }
      }

      if (any(unlist(xyAxes)) & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
        if (xyAxes$x | xyAxes$y & ((isBaseSubPlot & (isNewPlot | isReplot) | wipe))) {
          axesArgs <- sGrob@plotArgs
          axesArgs$side <- 1
          axesArgs <- axesArgs[names(axesArgs) %in% c("at", "labels", "tick", "line", "pos", "outer", "font",
                                                      "lty", "lwd", "lwd.ticks", "col.ticks", "hadj", "padj")]
        }

        if (xyAxes$x & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          axesArgsX <- append(list(side = 1), axesArgs)
          suppressWarnings(do.call(axis, args = axesArgsX))
        }
        if (xyAxes$y & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          axesArgsY <- append(list(side = 2), axesArgs)
          suppressWarnings(do.call(axis, args = axesArgsY))
        }
      }
    } else {
      # This is for Rasters and Sp objects only
      # Extract legend text if the raster is a factored raster
      if (is.null(legendText)) {
        if (is.null(sGrob@plotArgs$legendTxt)) {
          if (any(raster::is.factor(grobToPlot))) {
            sGrob@plotArgs$legendTxt <- raster::levels(grobToPlot)[[1]]
          }
        }
      } else {
        sGrob@plotArgs$legendTxt <- legendText
        cTxt <-
          legendText
      }

      if (!isBaseSubPlot ) {
        sGrob@plotArgs$legendTxt <- NULL
      }

      plotGrobCall <- list(grobToPlot = zMat$z, col = zMat$cols,
                           size = unit(sGrob@plotArgs$size, "points"),
                           real = zMat$real,
                           minv = zMat$minz, maxv = zMat$maxz,
                           pch = sGrob@plotArgs$pch, name = subPlots,
                           vp = vps,
                           legend = #sGrob@plotArgs$legend  &  isBaseSubPlot &
                             #isReplot |
                             sGrob@plotArgs$legend & (isBaseSubPlot &
                                                        (isNewPlot | wipe | isReplot)),
                           legendText = sGrob@plotArgs$legendTxt,
                           gp = sGrob@plotArgs$gp,
                           gpText = sGrob@plotArgs$gpText,
                           speedup = sGrob@plotArgs$speedup,
                           length = sGrob@plotArgs$length
      ) %>% append(., nonPlotArgs)

      seekViewport(subPlots, recording = FALSE)
      suppressWarnings(do.call(.plotGrob, args = plotGrobCall))

      if (any(unlist(xyAxes)) & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
        seekViewport(paste0("outer", subPlots), recording = FALSE)
        if (xyAxes$x & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          grid.xaxis(name = "xaxis", gp = sGrob@plotArgs$gpAxis)
        }
        if (xyAxes$y & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          grid.yaxis(name = "yaxis", gp = sGrob@plotArgs$gpAxis, vp = vps$wholeVp$children[[paste0("outer", subPlots)]])
        }
        seekViewport(subPlots, recording = FALSE)
      }
    } #gg vs histogram vs spatialObject
    # print Title on plot
    if (!identical(FALSE, sGrob@plotArgs$title) & (isBaseSubPlot & (isNewPlot | isReplot))) {
      plotName <- if (isTRUE(sGrob@plotArgs$title)) sGrob@plotName else sGrob@plotArgs$title
      a <- try(seekViewport(paste0("outer", subPlots), recording = FALSE))
      suppressWarnings(grid.text(plotName, name = "title",
                                 y = 1.08 - is.list(grobToPlot) * 0.02,
                                 vjust = 0.5, # tweak... not good practice.
                                              # Should find original reason why this is
                                              # not same y for rasters and all others
                                 gp = sGrob@plotArgs$gpText))
      a <- try(seekViewport(subPlots, recording = FALSE))
    }
    return(sGrob)
})

#' @param nColumns Numeric, length 1, indicating how many columns are in the device arrangement
#' @param whPlotObj Numeric. Length 1, indicating which of the currently objects passed into
#'                  \code{Plot} is currently being plotted, i.e., a counter of sorts.
#'
#' @include plotting-classes.R
#' @inheritParams .makeSpadesPlot
#' @docType methods
#' @aliases PlotHelpers
#' @keywords internal
#' @name .refreshGrob
#' @rdname Plot-internal
#'
setGeneric(".refreshGrob", function(sGrob, subPlots, legendRange,
                                    grobToPlot, plotArgs, nColumns, whPlotObj) {
  standardGeneric(".refreshGrob")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @keywords internal
setMethod(
  ".refreshGrob",
  signature = c(".spadesGrob"),
  definition = function(sGrob, subPlots, legendRange,
                        grobToPlot, plotArgs, nColumns, whPlotObj) {
    seekViewport(paste0("outer", subPlots), recording = FALSE)
    grid.rect(x = 0, width = unit(1 + is(grobToPlot, "Raster") * 0.20 / (nColumns / 2), "npc"),
              gp = gpar(fill = "white", col = "white"), just = "left")
    plotArgsByPlot <- lapply(plotArgs, function(x) {
      if (is.list(x)) {
        if (length(x) > 1) {
          return(x[whPlotObj])
        }}
      x
    })
    sGrob@plotArgs[names(plotArgs)] <- plotArgsByPlot
    sGrob@plotArgs$new <- FALSE
    sGrob@plotArgs$legendRange <- if (is.null(legendRange)) {
      NULL
    } else if (is.list(legendRange) & length(legendRange) > 1) {
      legendRange[[whPlotObj]]
    } else {
      legendRange
    }
    seekViewport(subPlots, recording = FALSE)
    return(sGrob)
})


#' @include plotting-classes.R
#' @docType methods
#' @aliases PlotHelpers
#' @keywords internal
#' @name .updateGrobGPTextAxis
#' @rdname Plot-internal
setGeneric(".updateGrobGPTextAxis", function(sGrob, arr, newArr) {
  standardGeneric(".updateGrobGPTextAxis")
})

#' @aliases PlotHelpers
#' @rdname Plot-internal
setMethod(
  ".updateGrobGPTextAxis",
  signature = c(".spadesGrob"),
  definition = function(sGrob, arr, newArr) {

    if (!is(sGrob@plotArgs$gpText, "gpar")) {
      sGrob@plotArgs$gpText <- as(sGrob@plotArgs$gpText, "gpar")
    }
    if (!is(sGrob@plotArgs$gpAxis, "gpar")) {
      sGrob@plotArgs$gpAxis <- as(sGrob@plotArgs$gpAxis, "gpar")
    }
    if (!is(sGrob@plotArgs$gp, "gpar")) {
      sGrob@plotArgs$gp <- as(sGrob@plotArgs$gp, "gpar")
    }

    if (is.null(sGrob@plotArgs$gpText$cex) | newArr) {
      # pipe won't work here :S
      sGrob@plotArgs$gpText$cex <- max(
        0.6,
        min(1.2, sqrt(prod(arr@ds) / prod(arr@columns, arr@rows)) * 0.3)
      )
    }
    if (is.null(sGrob@plotArgs$gpAxis$cex) | newArr) {
      # pipe won't work here :S
      sGrob@plotArgs$gpAxis$cex <- max(
        0.6,
        min(1.2, sqrt(prod(arr@ds) / prod(arr@columns, arr@rows)) * 0.3)
      )
    }
    return(sGrob)
})

################################################################################
#' Identify where to get the grob from
#'
#' Internal function.
#'
#' Because the Plot function can use the global environment as a source of
#' objects to plot, not just the call itself, this function identifies where
#' the data for the grob should come from, the current call or the global
#' environment.
#'
#' @param grobNamesi name of the object to plot
#'
#' @param toPlot list containing the objects to plot, made as a call to the
#'               \code{Plot} function
#'
#' @param takeFromPlotObj Logical. Should the data come from the argument passed
#'                        into Plot (\code{TRUE}), or from the (\code{.spadesEnv})
#'                        (\code{FALSE}).
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @keywords internal
#' @rdname identifyGrobToPlot
setGeneric(".identifyGrobToPlot", function(grobNamesi, toPlot, takeFromPlotObj) {
  standardGeneric(".identifyGrobToPlot")
})

#' @rdname identifyGrobToPlot
setMethod(
  ".identifyGrobToPlot",
  signature = c(".spadesGrob", "list"),
  function(grobNamesi, toPlot, takeFromPlotObj) {
    ## get the object name associated with this grob

    if (length(toPlot) == 0) takeFromPlotObj <- FALSE

    # Does it already exist on the plot device or not
    if (nzchar(grobNamesi@layerName, keepNA=TRUE)) {
      # means it is in a raster
      if (takeFromPlotObj) {
        grobToPlot <- unlist(toPlot[[1]], recursive = FALSE)[[grobNamesi@layerName]]
      } else {
        grobToPlot <- eval(parse(text = grobNamesi@objName),
                           grobNamesi@envir)[[grobNamesi@layerName]]
      }
    } else {
      if (takeFromPlotObj) {
        if (!is(toPlot[[1]], "gg") & !is(toPlot[[1]], "igraph")) {
          grobToPlot <- unlist(toPlot[[1]], recursive = FALSE)
        } else {
          grobToPlot <- toPlot[[1]]
        }
      } else {
        grobToPlot <- eval(parse(text = grobNamesi@objName), grobNamesi@envir)
      }
    }
    return(grobToPlot)
})

################################################################################
#' Prepare raster for plotting
#'
#' Internal function. Takes a raster .spadesGrob, and converts zoomExtent into
#' a zoom, and legendRange into a legend.
#' Then calculates the maxpixels to plot for speed.
#'
#' @param grobToPlot .spadesGrob
#' @param zoomExtent an extent object
#' @param legendRange a numeric vector of length >=2 indicating the desired legend range.
#' @param takeFromPlotObj logical. Should the object be found in the Plot call or .GlobalEnv
#' @param arr an \code{.arrangement} object
#' @param speedup numeric, greater than 1 will usually speed up plotting at the expense of resolution
#' @param newArr logical, whether this is a new arrangement or just adding to a previous one
#'
#' @include plotting-classes.R
#' @keywords internal
#' @rdname prepareRaster
#' @author Eliot McIntire
# igraph exports %>% from magrittr
.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
                           takeFromPlotObj, arr, speedup, newArr) {

  if (is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- ncell(crop(grobToPlot, zoom))
  }
  if (is.null(legendRange)) {
    legendRange <- NA
  }

  if (speedup > 0.1) {
    maxpixels <- min(5e5, 3e4 / (arr@columns * arr@rows) * prod(arr@ds)) %>%
      `/`(., speedup) %>%
      min(., npixels)
  } else {
    maxpixels <- npixels
  }
  skipSample <- if (is.null(zoomExtent)) {
    maxpixels >= npixels
  } else {
    FALSE
  }

  return(list(maxpixels = maxpixels, skipSample = skipSample,
              legendRange = legendRange, zoom = zoom))
}


################################################################################
#' Merge two SpaDES Plot objects
#'
#' Merges two \code{.spadesPlot} objects
#'
#' @param newSP  The "new" \code{.spadesPlot} object.
#'               I.e., the new merges and overwrites into current.
#'
#' @param curr   The "current" \code{.spadesPlot} object.
#'               I.e., the one to be merged into.
#'
#' @param ...    Additional arguments. Currently none implemented.
#'
#' @importFrom stats na.omit
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
#' @keywords internal
#' @rdname updateSpadesPlot
setGeneric(".updateSpadesPlot", function(newSP, curr, ...) {
  standardGeneric(".updateSpadesPlot")
})

#' @rdname updateSpadesPlot
setMethod(
  ".updateSpadesPlot",
  signature = c(newSP = ".spadesPlot", curr = "list"),
  definition = function(newSP, curr, ...) {
    newNames <- names(newSP@spadesGrobList)
    currNames <- names(curr$curr@spadesGrobList)

    addToPlots <- sapply(newSP@spadesGrobList, function(x) {
      !is.null(x[[1]]@plotArgs$addTo)
    })

    addToPlotsNames <- sapply(newSP@spadesGrobList, function(x) {
      x[[1]]@plotArgs$addTo
    }) %>% unlist

    if (length(addToPlotsNames) == length(newNames)) {
      overplots <- integer(0)
    } else {
      overplots <- na.omit(match(currNames, newNames))
      addToPlots1 <- na.omit(match(currNames, addToPlotsNames))
      overplots <- overplots[!(overplots %in% addToPlots1)]
    }

    needNew <- -c(overplots, which(addToPlots))
    if (length(needNew) == 0) {
      needNew <- 1:length(newNames)
    }

    whichParamsChanged <- lapply(newNames[overplots], function(x) {
      sapply(names(newSP@spadesGrobList[[x]][[1]]@plotArgs), function(y) {
        if (!is.null(newSP@spadesGrobList[[x]][[1]]@plotArgs[[y]])) {
          !identical(newSP@spadesGrobList[[x]][[1]]@plotArgs[[y]],
                     curr$curr@spadesGrobList[[x]][[1]]@plotArgs[[y]])
        } else {
          FALSE
        }
      })
    })
    names(whichParamsChanged) <- newNames[overplots]

    # # Set FALSE as default for needPlotting
    needPlotting <- lapply(curr$curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })
    #
    # # Set FALSE as default for isReplot
    isReplot <- lapply(curr$curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })
    #
    # # Set TRUE as default for isBaseLayer
    # isBaseLayer <- lapply(curr$curr@spadesGrobList, function(x) {
    #   lapply(x, function(y) { TRUE })
    # })
    #
    isNewPlot <- lapply(curr$curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })

    #needPlotting <- curr$needPlotting
    #isReplot <- curr$isReplot
    isBaseLayer <- curr$isBaseLayer
    #isNewPlot <- curr$isNewPlot

    # For overplots
    for (plots in newNames[overplots]) {
      # update only those plotArgs that have changed.
      curr$curr@spadesGrobList[[plots]][[1]]@plotArgs[names(whichParamsChanged[[plots]])[whichParamsChanged[[plots]]]] <-
        newSP@spadesGrobList[[plots]][[1]]@plotArgs[names(whichParamsChanged[[plots]])[whichParamsChanged[[plots]]]]

      needPlotting[[plots]][[plots]] <- TRUE
      isReplot[[plots]][[plots]] <- FALSE
      #isBaseLayer[[plots]][[plots]] <- isBaseLayer[[plots]][[plots]]
      isNewPlot[[plots]][[plots]] <- FALSE
    }

    # put addTo plots into list of spadesGrobs that it will be added to
    if (!is.null(addToPlotsNames)) {
      for (plots in 1:length(addToPlotsNames)) {
        #len <- length(curr$curr@spadesGrobList[[addToPlotsNames[plots]]])
        curr$curr@spadesGrobList[[addToPlotsNames[plots]]][names(addToPlotsNames[plots])] <-
          newSP@spadesGrobList[[names(addToPlotsNames[plots])]]
        # change the name of the plotName to the parent object
        curr$curr@spadesGrobList[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]]@plotName <-
          curr$curr@spadesGrobList[[addToPlotsNames[plots]]][[1]]@plotName
        needPlotting[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- TRUE
        isReplot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
        isBaseLayer[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
        isNewPlot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
      }
    }

    # for new plots
    for (plots in newNames[needNew]) {
      curr$curr@spadesGrobList[[plots]] <- newSP@spadesGrobList[[plots]]
      needPlotting[[plots]] <- TRUE
      isReplot[[plots]] <- FALSE
      isBaseLayer[[plots]] <- TRUE
      isNewPlot[[plots]] <- TRUE
    }
    return(
      list(
        curr = curr$curr, whichParamsChanged = whichParamsChanged,
        needPlotting = needPlotting, isReplot = isReplot,
        isBaseLayer = isBaseLayer, isNewPlot = isNewPlot
      )
    )
})

#' @rdname updateSpadesPlot
setMethod(
  ".updateSpadesPlot",
  signature = c(newSP = ".spadesPlot", curr = "missing"),
  definition = function(newSP, ...) {

    return(list(
      curr = newSP, whichParamsChanged = NULL,
      needPlotting = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) TRUE)
      }),
      isReplot = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) FALSE)
      }),
      isNewPlot = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) TRUE)
      }),
      isBaseLayer = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) TRUE)
      })
    ))
  })

################################################################################
#' Determine optimal plotting arrangement of plot objects
#'
#' Internal function. Assesses the device geometry, the map geometry, and the
#' number of spatial objects to plot and builds an object that will be used by
#' the Plot functions to plot them efficiently.
#'
#' @param sPlot A \code{.spadesPlot} object.
#' @inheritParams Plot
#'
#' @rdname arrangeViewports
#' @include plotting-classes.R
#' @importFrom grDevices dev.cur dev.new dev.size
#' @importFrom sp bbox
#' @export
#' @keywords internal
#' @author Eliot McIntire
#' @docType methods
# igraph exports %>% from magrittr
setGeneric(".arrangeViewports", function(sPlot, arr=NULL) {
  standardGeneric(".arrangeViewports")
})

#' @export
#' @rdname arrangeViewports
setMethod(
  ".arrangeViewports",
  signature = c(".spadesPlot"),
  definition = function(sPlot, arr) {

    ds <- dev.size()
    ds.ratio <- ds[1] / ds[2]

    sgl <- sPlot@spadesGrobList

    dimx <- apply(do.call(
      rbind, sapply(1:length(sgl), function(x) {
        lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
          if (z == TRUE) {
            # for spatial objects
            apply(
              bbox(
                eval(
                  parse(text = sgl[[x]][[1]]@objName),
                  envir = sgl[[x]][[1]]@envir
                )
              ),
              1,
              function(y) {
                diff(range(y))
              }
            )
          } else {
            # for non spatial objects
            c(1,1)
          }
        })
      })), 2, max)

    dimensionRatio <- dimx[1] / dimx[2]

    ds.dimensionRatio <- ds.ratio / dimensionRatio
    if (is.null(arr)) {

      nPlots <- length(sgl)
      names <- names(sgl)

      if (dev.cur() == 1) {
        dev.new(height = 8, width = 10)
      }

      col.by.row <- data.frame(matrix(ncol = 2, nrow = nPlots))

      col.by.row[, 1] <- ceiling(nPlots / (1:nPlots))
      col.by.row[, 2] <- ceiling(nPlots / col.by.row[, 1])

      # wh.best <- which.min(abs(apply(col.by.row, 1, function(x) { x[1]/x[2] }) - ds.dimensionRatio))
      # rewritten for clarity/brevity with pipes below
      wh.best <- apply(col.by.row, 1, function(x) x[1] / x[2]) %>%
        `-`(., ds.dimensionRatio) %>%
        abs() %>%
        which.min()

      columns <- col.by.row[wh.best, 1]
      rows <- col.by.row[wh.best, 2]
    } else {
      columns <- arr[2]
      rows <- arr[1]
    }

    actual.ratio <- columns / rows

    out <- new(
      ".arrangement", rows = rows, columns = columns,
      actual.ratio = actual.ratio,
      ds.dimensionRatio = ds.dimensionRatio,
      ds = ds
    )
    return(out)
})


################################################################################
#' Plot spatial grobs (using \code{grid} package)
#'
#' Internal function. Plot a raster Grob, a points Grob, polygon Grob.
#'
#' \code{speedup} is only used for \code{SpatialPolygons}, \code{SpatialPoints},
#' and \code{SpatialLines} in this function.
#' Attempts have been made to subsample at a good level that optimizes speed of
#' plotting, without losing visible quality. Nevertheless, to force all points to
#' be plotted, use a speedup value less than 0.1.
#' From a speed perspective, there appears to be an optimal subsampling when
#' using \code{thin} from the \code{fastshp} package.
#' Presumably, too much thinning requires large distance matrices to be
#' calculated, slowing plotting down.
#' Too little thinning causes an overabundance of points to be plotted, slowing
#' plotting down.
#'
#' The suggested package \code{fastshp} can be installed with:
#' \code{install.packages("fastshp", repos = "http://rforge.net", type = "source")}.
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system.
#' For building on Windows, you'll need to install \code{Rtools} from
#' \url{https://cran.r-project.org/bin/windows/Rtools/}.
#'
#' @param grobToPlot  \code{Raster*}, \code{SpatialLines*},
#'                    \code{SpatialPoints*}, or \code{SpatialPolygons*} object.
#'
#' @param col     Currently only used for the legend of a \code{Raster*} object.
#'
#' @param size    The size of the \code{SpatialPoints}.
#'
#' @param gp      \code{grid} parameters, usually the output of a call to
#'                \code{\link{gpar}}.
#'
#' @param gpText  \code{gpar} object for legend label text.
#'
#' @param legend  Logical indicating whether a legend should be drawn.
#'                Default \code{TRUE}.
#'
#' @param legendText  Vector of values to use for legend value labels.
#'                    Defaults to \code{NULL} which results in a pretty numeric
#'                    representation. If \code{Raster*} has a Raster Attribute
#'                    Table (rat; see \code{raster} package), this will be used
#'                    by default. Currently, only a single vector is accepted.
#'
#' @param length  Numeric.
#'
#' @param minv    The minimum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param maxv    The maximum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param pch     Point character for \code{SpatialPoints}, as \code{par}.
#'
#' @param real    Logical indicating whether the data are \code{real} numbers
#'                (i.e., as opposed to \code{integer} or \code{factor}).
#'
#' @param speedup Numeric. The factor by which the number of vertices in
#'                \code{SpatialPolygons} and \code{SpatialLines*} will be
#'                subsampled. The vertices are already subsampled by default to
#'                make plotting faster.
#'
#' @param vp      whole viewport tree of \code{spadesGrob}
#'
#' @param name    Character string of name of object being plotted.
#'
#' @param ...     Additional arguments. None currently implemented.
#'
#' @docType methods
#' @keywords internal
#' @rdname plotGrob
#'
#' @importFrom data.table ':=' data.table
#' @importFrom raster extent pointDistance xmin xmax ymin ymax
#'
#' @importFrom sp proj4string
#' @importFrom grid gpar gTree gList rasterGrob textGrob grid.draw
#' @importFrom grDevices as.raster
#'
#' @author Eliot McIntire
setGeneric(".plotGrob", function(grobToPlot, col = NULL, real = FALSE,
                                 size = unit(5, "points"), minv, maxv,
                                 legend = TRUE, legendText = NULL,
                                 length = NULL,
                                 gp = gpar(), gpText = gpar(), pch = 19,
                                 speedup = 1, name = character(), vp = list(), ...) {
  standardGeneric(".plotGrob")
})

############## SpatialPoints - thin
#' @rdname plotGrob
#' @importFrom grid pointsGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPoints"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, name, vp, ...) {
    speedupScale <- 40
    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / (4.8e5 * speedupScale)
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) /
        speedupScale
    }
    xyOrd <- coordinates(grobToPlot)

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if greater than 1000 pts
      if (speedup > 0.1) {
        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          #thinned[, groups := rep(1:NROW(idLength), idLength$V1)]
          #idLength <- thinned[, sum(thin),by = groups]
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    pntGrob <- gTree(
      grobToPlot = grobToPlot,
      children = gList(
        pointsGrob(
          x = xyOrd[, 1], y = xyOrd[, 2],
          pch = pch, size = size
        )
      ),
      gp = gp,
      cl = "plotPoint"
    )
    grid.draw(pntGrob)
    return(invisible(pntGrob))
})

#' @rdname plotGrob
setMethod(
  ".plotGrob",
  signature = c("matrix"),
  definition = function(grobToPlot, col, real, size, minv, maxv,
                        legend, legendText, gp, gpText, pch, name, vp, ...) {

    pr <- if (real) {
      pretty(range(minv, maxv))
    } else {
      if (!is.null(legendText)) {
        nrowLegText <- NROW(legendText)
        if (NCOL(legendText) > 1) { # means it was a factor
          legendText$contigValue <- 1:nrowLegText
          if (nrowLegText > 20) {
            pr <- pretty(legendText$contigValue)
            pr <- pr - min(pr) + 1 # start it at one
          } else {
            legendText$contigValue
          }

        } else {
          unique(round(pretty(range(minv, maxv), n = length(legendText))))
        }
      } else {
        unique(round(pretty(range(minv, maxv))))
      }
    }

    if (NCOL(legendText) == 1) { # means it was not a factor
      pr <- pr[pr <= maxv & pr >= minv]
    } else {
      pr <- pr[pr <= nrowLegText & pr >= 1]
    }
    if (length(pr) == 0) pr <- seq(minv, maxv, by = 2)
    #maxNumCols = 100
    maxcol <- length(col)
    mincol <- 2

    gpText$cex <- gpText$cex * 0.6
    if (length(gpText) == 0)
      gpText <- gpar(col = "black", cex = 0.6)

    rastGrob <- gTree(
      grobToPlot = grobToPlot, pr = pr, col = col,
      children = gList(
        rasterGrob(
          as.raster(grobToPlot),
          interpolate = FALSE,
          name = "raster"
        )),
      gp = gp, cl = "plotRast")
    grid.draw(rastGrob)

    rastGrob2 <- gTree(
      grobToPlot = grobToPlot, pr = pr, col = col,
      children = gList(
        rasterGrob(
          as.raster(grobToPlot),
          interpolate = FALSE,
          name = "raster"
        ),
        if (legend) {
          if (NCOL(legendText) > 1) {
            # for factors
            colForLegend <- col[rev(legendText$contigValue - min(legendText$contigValue) + 2)]
          } else {
            colForLegend <- col[(maxcol):mincol]
          }
          rasterGrob(#vp = vp[[1]][[2]][[paste0("outer",name)]],
            as.raster(colForLegend),
            x = 1.04, y = 0.5,
            height = 0.5, width = 0.03,
            interpolate = FALSE,
            name = "legend"
          )
        },
        if (legend) {
          txt <- if (is.null(legendText)) {
            pr
          } else {
            if (NCOL(legendText) > 1) { # for factor legends
              legendIndex <- pr
              legendText[legendIndex, 2]
            } else {
              legendIndex <- pr - min(pr) + 1
              legendText[legendIndex]
            }
          }
          textGrob(
            txt, #vp = vp[[1]][[2]][[paste0("outer",name)]],
            x = 1.08,
            y = if (!real) {
              if (NCOL(legendText) > 1) { # factors
                maxv <- legendText$contigValue[nrowLegText]
                minv <- legendText$contigValue[1]
              }
              ((pr - minv) / ((maxv + 1) - minv)) / 2 + 0.25 + 1 /
                (diff(range(minv, maxv)) + 1) / 4
            } else {
              ((pr - minv) / ((maxv) - minv)) / 2 + 0.25
            },
            gp = gpText,
            just = "left", check.overlap =
              TRUE,
            name = "legendText"
          )
        }
      ),
      gp = gp, cl = "plotRast2"
    )

    seekViewport(paste0("outer", name), recording = FALSE)
    grid.draw(rastGrob2)

    return(invisible(rastGrob))
})

################################################################################
#' @rdname plotGrob
#' @importFrom grid polygonGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPolygons"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, name, vp, ...) {

    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / 1.2e10
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
    }
    # For speed of plotting
    xy <- lapply(1:length(grobToPlot), function(i) {
      lapply(grobToPlot@polygons[[i]]@Polygons, function(j) {
        j@coords
      })
    })

    hole <- lapply(1:length(grobToPlot), function(x) {
      lapply(grobToPlot@polygons[[x]]@Polygons, function(x)
        x@hole)
    }) %>% unlist()

    ord <- grobToPlot@plotOrder

    ordInner <- lapply(1:length(grobToPlot), function(x) {
      grobToPlot@polygons[[x]]@plotOrder
    })

    xyOrd.l <- lapply(ord, function(i) {
      xy[[i]][ordInner[[i]]]
    })

    # idLength <- data.table(V1=unlist(lapply(xyOrd.l, function(i) lapply(i, length)))/2)
    idLength <- lapply(xyOrd.l, function(i) { lapply(i, length) }) %>%
      unlist %>%
      `/`(., 2) %>%
      data.table(V1 = .)

    xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) { do.call(rbind, i) }))

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if fewer than 1000 pts
      if (speedup > 0.1) {

        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          thinned[, groups := rep(1:NROW(idLength), idLength$V1)]
          idLength <- thinned[, sum(thin), by = groups]
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    gp$fill[hole] <- "#FFFFFF00"
    polyGrob <- gTree(children = gList(
      polygonGrob(
        x = xyOrd[, 1], y = xyOrd[, 2], id.lengths = idLength$V1,
        gp = gp, default.units = "native"
      )
    ),
    gp = gp,
    cl = "plotPoly")
    grid.draw(polyGrob)
    return(invisible(polyGrob))
})

#' @rdname plotGrob
#' @importFrom grid polylineGrob arrow
setMethod(
  ".plotGrob",
  signature = c("SpatialLines"),
  definition = function(grobToPlot, col, size,
                        legend, length, gp = gpar(), pch, speedup, name, vp, ...) {
    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / 1.2e10
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
    }

    # For speed of plotting
    xy <- lapply(1:length(grobToPlot), function(i) {
      grobToPlot@lines[[i]]@Lines[[1]]@coords
    })
    idLength <- unlist(lapply(xy, length)) / 2
    xy <- do.call(rbind,xy)

    if (NROW(xy) > 1e3) {
      # thin if fewer than 1000 pts
      if (speedup > 0.1) {

        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- fastshp::thin(xy[, 1], xy[, 2],
                                   tolerance = speedupScale * speedup)

          # keep first and last points of every polyline,
          # if there are fewer than 10,000 vertices
          if (sum(thinned) < 1e4) {
            lastIDs <- cumsum(idLength)

            # Ensure first and last points of each line are kept:
            thinned[c(1, lastIDs + 1)[-(1 + length(lastIDs))]] <- TRUE
            thinned[lastIDs] <- TRUE
          }
          xy <- xy[thinned, ]
          idLength <- tapply(thinned, rep(1:length(idLength), idLength), sum)
        } else {
          message(
            paste(
              "To speed up Lines plotting using Plot, install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")"
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                "  https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    if (is.null(length)) {
      lineGrob <- gTree(children = gList(
        polylineGrob(
          x = xy[, 1], y = xy[, 2], id.lengths = idLength,
          gp = gp, default.units = "native"
        )
      ),
      gp = gp,
      cl = "plotLine")
    } else {
      lineGrob <- gTree(children = gList(
        polylineGrob(
          x = xy[, 1], y = xy[, 2], id.lengths = idLength,
          gp = gp, default.units = "native",
          arrow = arrow(length = unit(length, "inches"))
        )
      ),
      gp = gp,
      cl = "plotLine")
    }

    grid.draw(lineGrob)
    return(invisible(lineGrob))
})

################################################################################
#' Make an optimal layout of plots
#'
#' Internal function. Using the size of the current device, and number and
#' dimension ratios of the plots, place them optimally in the plotting region.
#'
#' @param arr an object of class \code{.arrangement}.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether legend should be included as part of
#'               layout calculation. Default is \code{TRUE}.
#'
#' @param axes Logical indicating whether the axes should be included as part of
#'             layout calculation. Default is \code{TRUE}.
#'
#' @param title Logical indicating whether the names of each plot should be
#'              written above plots and should be included as part of layout
#'               calculation. Default is \code{TRUE}.
#'
#' @docType methods
#' @include plotting-classes.R
#' @importFrom grid unit unit.c
#' @keywords internal
#' @rdname makeLayout
#' @author Eliot McIntire
#'
.makeLayout <- function(arr, visualSqueeze,
                        legend = TRUE, axes = TRUE, title = TRUE) {
  columns <- arr@columns
  rows <- arr@rows

  # Reduce by 40% of remaining space if each of the following is not wanted
  if (legend == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }
  if (axes == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }
  if (title == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }

  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w <- min(
    visualSqueeze / columns,
    visualSqueeze / columns * arr@actual.ratio / arr@ds.dimensionRatio
  )

  wdth <- unit.c(unit(0.2, "null"),
                 unit(rep(c(0.875, vS.w, 0.875), columns),
                      rep(c("null", "npc", "null"), columns)),
                 unit(0.2, "null"))

  # calculate the visualSqueeze for the height (i.e., vS.h)
  vS.h <- min(visualSqueeze / rows,
              visualSqueeze / rows * arr@ds.dimensionRatio / arr@actual.ratio)
  ht <- unit.c(unit(0.2, "null"),
               unit(rep(c(0.875, vS.h, 0.875), rows),
                    rep(c("null", "npc", "null"), rows)),
               unit(0.2, "null"))

  return(list(wdth = wdth, ht = ht, wdthUnits = vS.w, htUnits = vS.h,
              visualSqueeze = visualSqueeze))
}

################################################################################
#' Make viewports
#'
#' Given a set of extents, and a layout for these extents, this function will
#' output a viewport tree to allow plotting.
#'
#' This function will either create a totally new set of viewports, or simply
#' add some nested viewports to an existing arrangement, i.e., is there still
#' white space available to plot.
#'
#' @param sPlot An object of class \code{.spadesPlot}.
#'
#' @param newArr  Logical indicating whether this function will create a
#'                completely new viewport. Default \code{FALSE}.
#'
#' # @importFrom NetLogoRClasses extent
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @importFrom grid viewport vpTree vpList
#' @importFrom raster xmin xmax ymin ymax extent
#' @keywords internal
#' @rdname makeViewports
#'
.makeViewports <- function(sPlot, newArr = FALSE) {

  arr <- sPlot@arr
  sgl <- sPlot@spadesGrobList

  extents <- unlist(sapply(sgl, function(x) {
    unname(lapply(x[[1]]@isSpatialObjects, function(z) {
      if (z == TRUE) {
        # for spatial objects
        if (!is.null(x[[1]]@plotArgs$zoomExtent)) {
          x[[1]]@plotArgs$zoomExtent
        } else {
          extent(eval(parse(text = x[[1]]@objName), envir = x[[1]]@envir))
        }
      } else {
        # for non spatial objects
        extent(c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
      }
    }))
  }))

  columns <- arr@columns
  rows <- arr@rows
  gl1 <- grid.layout(
    nrow = rows * 3 + 2, ncol = columns * 3 + 2,
    widths = arr@layout$wdth, heights = arr@layout$ht
  )
  topVp <- viewport(layout = gl1,
                    name = "top"
  )
  plotVps <- list()

  nam <- names(extents)

  # This is the biggest of the extents, and is used in .makeLayout
  #  Need to replicate it here because all plots are scaled to this
  biggestDims <- apply(do.call(rbind, sapply(1:length(sgl), function(x) {
    lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
      if (z == TRUE) {
        # for spatial objects
        apply(bbox(extents[[x]]), 1, function(y) diff(range(y)))
      } else {
        # for non spatial objects
        c(2, 2)
      }
    })
  })), 2, max)

  for (extentInd in 1:length(extents)) {
    posInd <- match(nam[extentInd], names(sgl))
    lpc <- ceiling((posInd - 1) %% columns + 1) * 3
    lpr <- ceiling(posInd / columns) * 3

    if (!sgl[[posInd]][[1]]@isSpatialObjects) {
      lpc <- c((lpc - 1):(lpc + 1))
      lpr <- c((lpr):(lpr + 1))
    }
    # makes equal scale
    yrange <- extents[[extentInd]]@ymax - extents[[extentInd]]@ymin
    if (yrange > 0) {
      if (abs((yrange / (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin)) -
              (biggestDims[1] / biggestDims[2])) > (getOption("spades.tolerance"))) {
        dimensionRatio <- arr@layout$wdthUnits * arr@ds[1] /
          (arr@layout$htUnits * arr@ds[2])
        plotScaleRatio <-
          (extents[[extentInd]]@xmin - extents[[extentInd]]@xmax) /
          (extents[[extentInd]]@ymin - extents[[extentInd]]@ymax)

        vS.w <- min(1, plotScaleRatio / dimensionRatio)

        vS.h <- min(1, dimensionRatio / plotScaleRatio)

        addY <-
          abs(extents[[extentInd]]@ymax - extents[[extentInd]]@ymin -
                (extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) /
                vS.h) / 2
        addX <-
          abs(extents[[extentInd]]@xmax - extents[[extentInd]]@xmin -
                (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin) /
                vS.w) / 2
      } else {
        addY <- addX <- 0
      }
    } else {
      addX <- extents[[extentInd]]@xmin * 0.05
      addY <- extents[[extentInd]]@ymin * 0.05
    }
    # end equal scale
    plotVps[[nam[extentInd]]] <- viewport(
      clip = "on",
      name = nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]@xmin - addX, extents[[extentInd]]@xmax + addX),
      yscale = c(extents[[extentInd]]@ymin - addY, extents[[extentInd]]@ymax + addY)
    )
    plotVps[[paste0("outer", nam[extentInd])]] <- viewport(#clip = "on",
      name = paste0("outer", nam[extentInd]),
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]@xmin - addX, extents[[extentInd]]@xmax + addX),
      yscale = c(extents[[extentInd]]@ymin - addY, extents[[extentInd]]@ymax + addY)
    )
  }

  wholeVp <- vpTree(topVp, do.call(vpList, plotVps))

  return(list(wholeVp = wholeVp, extents = extents))
}
