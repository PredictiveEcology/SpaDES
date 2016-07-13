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
#' @importFrom raster nlayers
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @rdname nlayers
setMethod(
  "nlayers",
  signature = "list",
  definition = function(x) {
    y <- sum(sapply(x, function(x) {
      if (is(x, "RasterStack")) {
        x <- nlayers(x)
        } else {
          x <- 1L
        }
      return(x)
      }))
    return(y)
  }
)


#' @rdname nlayers
setMethod(
  "nlayers",
  signature = ".spadesPlot",
  definition = function(x) {
    return(length(x@arr@extents))
  }
)

#' @rdname nlayers
setMethod(
  "nlayers",
  signature = "ANY",
  definition = function(x) {
    return(1L)
  }
)

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
  }
)

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "ANY",
  definition = function(object) {
    return("")
  }
)

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "Raster",
  definition = function(object) {
    names(object)
  }
)

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
  }
)

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "igraph",
  definition = function(object) {
    return("")
  }
)

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
  }
)

################################################################################
#' Convert \code{plotArgs} to list of lists
#'
#' Internal function. Take the inputs as plotArgs to the Plot function, and make
#' them a list of length \code{numSpadesPlotObjects} entries of lists.
#'
#' @param plotArgs The arguments passed to \code{Plot} as a \code{list}.
#'
#' @param numSpadesPlotObjects Numeric. The number of \code{.spadesPlotObjects}.
#'                 This can't easily be deduced from the \code{plotArgs} because
#'                 of the RasterStacks. So passed manually.
#'
#' @rdname makeList
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
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

    return(p)
  }
)

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
#' if (interactive()) Plot(caribouTraj, new = TRUE, length = 0.1)
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
#' Plot(sim1$DEM, new = TRUE)
#' Plot(caribouTraj, addTo = "sim1$DEM", length = 0.1)
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
        coords = rbind(coordinates(from)[x,], coordinates(to)[x,])
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
#' @param eminus1  Environment. The parent of \code{e}.
#'
#' @return A list of length 2, with names \code{objs} and \code{envs} giving the
#' standardized representation (i.e., replacing \code{[[]]} with \code{$}
#' notation for objects) of objects and their layers (if \code{RasterStacks}).
#'
#' @docType methods
#' @importFrom grDevices dev.cur
#' @include plotting-classes.R
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
              match.call(definition=eval, call=parseTxt)$envir,
              envir = e
            ),
            error = function(x) { .GlobalEnv }
          )
        }
      )

      parseTxt[[3]] <- match.call(definition=eval, call=parseTxt)$expr
      if (is.name(match.call(definition=parse, call=parseTxt[[3]])$text)) {
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
              match.call(definition=eval, call=parseTxt)$envir,
              envir = e
            ),
            error = function(x) { .GlobalEnv }
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


      # if the XYZ of `get(x = XYZ)` is the same as an evaluated version of XYZ
      #       if (identical(
      #         tmpParseTxt3,
      #       parseTxt[[3]])) {
      #         lastOneDone = TRUE
      #       }
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
      error = function(x) { FALSE }
    )
    if (isChar) {
      elems[[i]] <- as.name(eval(elems[[i]], envir = eminus1))
    }
    parseTxt <- parse(text = deparse(parseTxt[[2]]))[[1]]
    i = i + 1
  }

  #   envs <- append(.GlobalEnv, sys.frames())[c(TRUE, sapply(sys.frames(), function(x)
  #     exists(deparse(parseTxt), envir = x, inherits = FALSE)))] %>%
  #     .[[length(.)]]
  envs <- append(.GlobalEnv, sys.frames()) %>%
    .[c(TRUE, sapply(sys.frames(), function(x) {
      exists(deparse(parseTxt), envir = x, inherits = FALSE)
    }))] %>%
    .[[length(.)]]

  inGlobal <- identical(envs, .GlobalEnv)
  if (is(eval(parse(text = deparse(parseTxt)), envir = envs), "environment")) {
    envs <- eval(parse(text = deparse(parseTxt)), envir = envs)
  } else {
    if (!lastOneDone) { elems[[i]] <- parseTxt }
  }
  if (exists("callEnv", inherits = FALSE)) {
    envs <- callEnv
  }

  if (!inGlobal) {
    if (!exists(paste0("dev", dev.cur()), envir = .spadesEnv)) {
      .spadesEnv[[paste0("dev", dev.cur())]] <- new.env(parent = emptyenv())
    }

    if (is(get(deparse(rev(elems)[[1]]), envir = envs), "simList")) { # If it is a simList
      useElem <- 1
      if (length(rev(elems)[-1]) > 1) { # If the user is passing a sub-element to say a Raster Stack
        if (is(get(deparse(rev(elems)[[2]]), envir = envs), "RasterStack")) { # Only Raster Stack implemented yet
          useElem <- 2
        }
      }
      changeObjEnv(deparse(elems[[useElem]]),
                   fromEnv = envir(get(deparse(rev(elems)[[1]]), envir = envs)),
                   toEnv = .spadesEnv[[paste0("dev", dev.cur())]])
    } else { # If it is NOT a simList.
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
#' @rdname objectNames
#' @export
#' @author Eliot McIntire
#'
objectNames <- function(calledFrom = "Plot",
                        argClass = ".spadesPlotObjects",
                        argName = "") {
  scalls <- sys.calls()
  # Extract from the sys.calls only the function "calledFrom"
  frameCalledFrom <- which(sapply(scalls, function(x) {
    grepl(x, pattern = paste0("^", calledFrom,"$"))[1]
  }))
  e <- sys.frame(frameCalledFrom[1])
  eminus1 <- sys.frame(frameCalledFrom - 1)

  if (nchar(argName) == 0) {
    callNamedArgs <- as.character(substitute(list(...), env = e))[-1]
  } else {
    #  callNamedArgs <- as.character(substitute(parse(text = argName)))[-1]
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
#' Internal function to convert a SpatialObject to a Graphical Object
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
#' @rdname .convertSpatialToPlotGrob
#' @author Eliot McIntire
#'
setGeneric(".convertSpatialToPlotGrob", function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                                                 spadesGrobCounter, subPlots, cols) {
  standardGeneric(".convertSpatialToPlotGrob")
})

#' @export
#' @rdname .convertSpatialToPlotGrob
setMethod(
  ".convertSpatialToPlotGrob",
  signature = c("spatialObjects"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        spadesGrobCounter, subPlots, cols) {
    if (is(grobToPlot, "Raster")) {
      # Rasters may be zoomed into and subsampled and have unique legend
      #            if(sGrob@plotArgs$new)
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
        grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
      }
      # This handles SpatialPointsDataFrames with column "color"
      if (any(grepl(pattern = "color", colnames(grobToPlot))) & is.null(cols))
        sGrob@plotArgs$cols <- getColors(grobToPlot)

      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    } else if (is(grobToPlot, "SpatialPolygons")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
      }
      #z <- grobToPlot
      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)

    } else if (is(grobToPlot, "SpatialLines")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
      }
      #z <- grobToPlot
      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    }
    zMat

  })


################################################################################
#' Internal function to determine which x and y axes to add to plots
#'
#' Not intended for normal user.
#'
#' @inheritParams .convertSpatialToPlotGrob
#' @param whPlotFrame Numeric. Which plot within the spadesGrobPlots object.
#'
#' @include plotting-classes.R
#' @docType methods
#' @rdname .xyAxes
#' @author Eliot McIntire
#'
setGeneric(".xyAxes", function(sGrob, arr, whPlotFrame) {
  standardGeneric(".xyAxes")
})

#' @export
#' @rdname .xyAxes
setMethod(
  ".xyAxes",
  signature = c(".spadesGrob", ".arrangement"),
  definition = function(sGrob, arr, whPlotFrame) {

    if (sGrob@plotArgs$axes== "L") {
      if (sGrob@objClass=="Raster" & (arr@extents[(whPlotFrame - 1) %% arr@columns + 1][[1]] ==
                                      arr@extents[max(
                                        which(
                                          (1:length(arr@names) - 1) %% arr@columns + 1 ==
                                          (whPlotFrame - 1) %% arr@columns + 1
                                        )
                                      )][[1]])) {
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

    if (sGrob@plotArgs$axes== "L") {
      if (sGrob@objClass=="Raster" & (arr@extents[whPlotFrame][[1]] ==
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

    return(list(x=xaxis, y=yaxis))
  })
