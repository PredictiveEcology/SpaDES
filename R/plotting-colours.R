################################################################################
#' Get colours for plotting Raster* objects.
#'
#' @param object     A \code{Raster*} object.
#'
#' @return Returns a named list of colors.
#'
#' @export
#' @docType methods
#' @aliases getColours
#' @rdname getColors
#'
#' @seealso \code{\link{setColors<-}},
#'          \code{\link[RColorBrewer]{brewer.pal}}
#'
#' @author Alex Chubaty
#'
setGeneric("getColors", function(object) {
  standardGeneric("getColors")
})

#' @rdname getColors
setMethod("getColors",
          signature = "Raster",
          definition = function(object) {
            cols <- lapply(names(object), function(x) {
              as.character(object[[x]]@legend@colortable)
            })
            names(cols) <- names(object)
            return(cols)
})

################################################################################
#' Set colours for plotting Raster* objects.
#'
#' \code{setColors} works as a replacement method or a normal function call.
#'
#' @param object     A \code{Raster*} object.
#'
#' @param ...   Additional arguments to \code{colorRampPalette}.
#'
#' @param n     An optional vector of values specifiying the number
#'              of levels from which to interpolate the color palette.
#'
#' @param value  Named list of hex color codes (e.g., from
#'               \code{RColorBrewer::brewer.pal}), corresponding to the names
#'               of RasterLayers in \code{x}.
#'
#' @return Returns a Raster with the \code{colortable} slot set to \code{values}.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @docType methods
#' @aliases setColours
#' @rdname setColors
#'
#' @seealso \code{\link[RColorBrewer]{brewer.pal}},
#'          \code{\link[grDevices]{colorRampPalette}}.
#'
#' @author Alex Chubaty
#'
#' @examples
#'   library(raster)
#'   library(igraph) # need pipe for one example below
#'
#'   on.exit({
#'     detach("package:raster")
#'     detach("package:igraph")
#'   })
#'   ras <- raster(matrix(c(0,0,1,2), ncol=2, nrow=2))
#'
#'   # Use replacement method
#'   setColors(ras, n=3) <- c("red", "blue", "green")
#'   Plot(ras, new=TRUE)
#'
#'   # Use function method
#'   ras <- setColors(ras, n=3, c("red", "blue", "yellow"))
#'   Plot(ras, new=TRUE)
#'
#'   # Using the wrong number of colors, e.g., here 2 provided,
#'   # for a raster with 3 values... causes interpolation, which may be surprising
#'   ras <- setColors(ras, c("red", "blue"))
#'   Plot(ras, new=TRUE)
#'
#'   # Real number rasters - interpolation is used
#'   ras <- raster(matrix(runif(9), ncol=3, nrow=3)) %>%
#'     setColors(c("red", "yellow")) # interpolates when real numbers, gives warning
#'   Plot(ras, new=TRUE)
#'
#'   # Factor rasters, can be contiguous (numerically) or not, in this case not:
#'   ras <- raster(matrix(sample(c(1,3,6), size=9, replace=TRUE), ncol=3, nrow=3))
#'   levels(ras) <- data.frame(ID=c(1,3,6), Names=c("red", "purple", "yellow"))
#'   ras <- setColors(ras, n=3, c("red", "purple", "yellow"))
#'   Plot(ras, new=TRUE)
#'
#'   # if a factor rastere, and not enough labels are provided, then a warning
#'   #   will be given, and colors will be interpolated
#'   #   The level called purple is not purple, but interpolated betwen red and yellow
#'   ras <- setColors(ras, c("red", "yellow"))
#'   Plot(ras, new=TRUE)
setGeneric("setColors<-",
           function(object, ..., n, value) {
             standardGeneric("setColors<-")
})

#' @rdname setColors
#' @importFrom raster is.factor
setReplaceMethod(
  "setColors",
  signature("RasterLayer", "numeric", "character"),
  function(object, ..., n, value) {
    if(is.na(n)) {
      object <- setColors(object=object, value=value)
      return(object)
    }
    if (raster::is.factor(object)) {
      if (n != NROW(object@data@attributes[[1]])) {
        message("Number of colors not equal number of values: interpolating")
        pal <- colorRampPalette(value, alpha = TRUE, ...)
        n <- NROW(object@data@attributes[[1]])
        object@legend@colortable <- pal(n)
      } else {
        object@legend@colortable <- value
      }
    } else {
      pal <- colorRampPalette(value, alpha = TRUE, ...)
      object@legend@colortable <- pal(n)
    }
    if (!is.character(object@legend@colortable)) stop("setColors needs color character values")
    return(object)
})

#' @rdname setColors
#' @importFrom raster is.factor
setReplaceMethod(
  "setColors",
  signature("RasterLayer", "missing", "character"),
  function(object, ..., value) {
    if (!raster::is.factor(object)) {
      n <- round((maxValue(object) - minValue(object))) + 1
    } else {
      n <- length(value)
    }
    setColors(object, n=n) <- value
    if (!is.character(object@legend@colortable)) stop("setColors needs color character values")
    return(object)
})

#' @rdname setColors
setReplaceMethod(
  "setColors",
   signature("RasterStack", "numeric", "list"),
   function(object, ..., n, value) {
     i <- which(names(object) %in% names(value))
     if (length(i) == 0) stop("Layer names do not match stack layer names")
     whValNamed <- names(value)[i] %in% names(n)
     whNNamed <- names(n) %in% names(value)[i]
     nFull <- n
     if (length(n) != length(i)) { # not enough n values}
       if (sum(nchar(names(n)) == 0) > 0) { # are there unnamed ones
         nFull <- rep(n[!whNNamed], length.out = length(i))
         nFull[whValNamed] <- n[whNNamed]
         names(nFull)[whValNamed] <- names(n)[whNNamed]
       } else if (is.null(names(n))) {
         nFull <- rep(n, length.out = length(i))
       }
     }

     for (x in i) {
       if ((x %in% i[whValNamed])) {
         setColors(object[[names(value)[x]]], ..., n = nFull[[names(value)[x]]]) <- value[[names(value)[x]]]
       } else {
         setColors(object[[names(value)[x]]], ..., n = nFull[x]) <- value[[names(value)[x]]]
       }
     }
     return(object)
})

#' @rdname setColors
setReplaceMethod(
  "setColors",
   signature("Raster", "missing", "list"),
   function(object, ..., value) {
     i <- which(names(object) %in% names(value))
     for (x in names(object)[i]) {
       setColors(object[[x]], ...) <- value[[x]]
     }
     return(object)
})

#' @export
#' @rdname setColors
setGeneric("setColors", function(object, value, n) {
  standardGeneric("setColors")
})

#' @rdname setColors
setMethod(
  "setColors",
  signature("RasterLayer", "character", "numeric"),
  function(object, value, n) {
    setColors(object = object, n = n) <- value
    return(object)
})

#' @rdname setColors
setMethod(
  "setColors",
  signature("RasterLayer", "character", "missing"),
  function(object, value) {
    setColors(object = object) <- value
    return(object)
})

################################################################################
#' Convert Raster to color matrix useable by raster function for plotting
#'
#' Internal function.
#'
#' @param grobToPlot   A \code{SpatialObject}.
#'
#' @param zoomExtent   An \code{Extent} object for zooming to.
#'                     Defaults to whole extent of \code{grobToPlot}.
#'
#' @param maxpixels    Numeric. Number of cells to subsample the complete
#'                     \code{grobToPlot}.
#'
#' @param legendRange  Numeric vector giving values that, representing the lower
#'                     and upper bounds of a legend (i.e., \code{1:10} or
#'                     \code{c(1,10)} will give same result) that will override
#'                     the data bounds contained within the \code{grobToPlot}.
#'
#' @param cols         Colours specified in a way that can be understood directly
#'                     or by \code{\link{colorRampPalette}}.
#'
#' @param na.color     Character string indicating the color for \code{NA} values.
#'                     Default transparent.
#'
#' @param zero.color   Character string indicating the color for zero values,
#'                     when zero is the minimum value.
#'                     Otherwise, it is treated as any other color.
#'                     Default transparent.
#'                     Use \code{NULL} if zero should be the value given to it
#'                     by the colortable associated with the Raster.
#'
#' @param skipSample   Logical. If no downsampling is necessary, skip.
#'                     Default \code{TRUE}.
#'
#' @rdname makeColorMatrix
#' @aliases makeColourMatrix
#' @include plotting-classes.R
#' @importFrom grDevices colorRampPalette terrain.colors
#' @importFrom raster minValue getValues sampleRegular is.factor
#' @importFrom stats na.omit
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @docType methods
#' @author Eliot McIntire
#'
setGeneric(".makeColorMatrix",
           function(grobToPlot, zoomExtent, maxpixels, legendRange,
                    cols = NULL, na.color = "#FFFFFF00", zero.color = NULL,
                    skipSample = TRUE) {
  standardGeneric(".makeColorMatrix")
})

#' @rdname makeColorMatrix
setMethod(
  ".makeColorMatrix",
  signature = c("Raster", "Extent", "numeric", "ANY"),
  definition = function(grobToPlot, zoomExtent, maxpixels, legendRange,
                        cols, na.color, zero.color, skipSample = TRUE) {
    zoom <- zoomExtent
    # It is 5x faster to access the min and max from the Raster than to
    # calculate it, but it is also often wrong... it is only metadata
    # on the raster, so it is possible that it is incorrect.
    if (!skipSample) {
      colorTable <- getColors(grobToPlot)[[1]]
      if (!is(try(minValue(grobToPlot)), "try-error")) {
        minz <- minValue(grobToPlot)
      }
      grobToPlot <- sampleRegular(
        x = grobToPlot, size = maxpixels,
        ext = zoom, asRaster = TRUE, useGDAL = TRUE
      )
      if (length(colorTable) > 0) {
        cols <- colorTable
      }
    }
    z <- getValues(grobToPlot)

    # If minValue is defined, then use it, otherwise, calculate them.
    #  This is different than maxz because of the sampleRegular.
    # If the low values in the raster are missed in the sampleRegular,
    #  then the legend will be off by as many as are missing at the bottom;
    #  so, use the metadata version of minValue, but use the max(z) to
    #  accomodate cases where there are too many legend values for the
    # number of raster values.
    if (any(is.na(legendRange))) {
      if (!exists("minz")) {
        minz <- suppressWarnings(min(z, na.rm = TRUE))
      }
      if (is.na(minz)) {
        minz <- suppressWarnings(min(z, na.rm = TRUE))
      }
      if (is.infinite(minz)) {
        minz <- 0
      }
      #
      maxz <- suppressWarnings(max(z, na.rm = TRUE))
      if (is.infinite(maxz)) {
        maxz <- 0
      }
    } else {
      minz <- min(legendRange)
      maxz <- max(legendRange)
    }

    real <- any(na.omit(z) %% 1 != 0) # Test for real values or not

    # Deal with colors - This gets all combinations, real vs. integers,
    #  with zero, with no zero, with NA, with no NA, not enough numbers,
    #  too many numbers
    maxNumCols <- 100

    if (any(is.na(legendRange))) {
      nValues <- ifelse(real, maxNumCols + 1, maxz - minz + 1)
    } else {
      #realRange <- any(legendRange %% 1 != 0) # Test for real values or not
      nValues <- ifelse(real, maxNumCols + 1, length(seq(legendRange[1], legendRange[length(legendRange)])))
    }

    colTable <- NULL
    if (is.null(cols)) {
      # i.e., contained within raster or nothing
      if (length(getColors(grobToPlot)[[1]]) > 0) {
        colTable <- getColors(grobToPlot)[[1]]
        lenColTable <- length(colTable)

        cols <- if ((nValues > lenColTable) & !raster::is.factor(grobToPlot)) {
          # not enough colors, use colorRamp
          colorRampPalette(colTable)(nValues)
        } else if ( (nValues <= lenColTable) | raster::is.factor(grobToPlot) ) {
          # one more color than needed:
          #   assume bottom is NA
          if (raster::is.factor(grobToPlot)) {
            factorValues <- grobToPlot@data@attributes[[1]][,1] %>%
              unique() %>% na.omit() %>% sort()
            if (length(factorValues) == length(colTable)) {
              colTable[seq.int(length(factorValues))]
            } else {
              colTable[c(1, 1 + factorValues)] # CHANGE HERE
            }
          } else {
            colTable
          }
        } else if (nValues <= (lenColTable - 1)) {
          # one more color than needed:
          #  assume bottom is NA
          na.color <- colTable[1]
          colTable[minz:maxz - minz + 2]
        } else if (nValues <= (lenColTable - 2)) {
          # two more colors than needed,
          #  assume bottom is NA, second is white
          na.color <- colTable[1]
          zero.color <- colTable[2]
          colTable[minz:maxz - minz + 3]
        } else {
          colTable
        }
      } else {
        # default color if nothing specified:
        cols <- rev(terrain.colors(nValues))
      }
    } else {
      if (is.character(cols) & (length(cols) == 1)) {
        if (cols %in% rownames(brewer.pal.info)) {
          suppressWarnings(cols <- brewer.pal(nValues, cols))
        } else {
          warning("Color not recognized. Try RColorBrewer or default R colors")
        }
      }
      cols <- if (nValues > length(cols)) {
        colorRampPalette(cols)(nValues)
      } else if (nValues < length(cols)) {
        cols[minz:maxz + max(0, 1 - minz)]
      } else {
        cols
      }
    }

    # Colors are indexed from 1, as with all objects in R, but there
    # are generally zero values on the rasters, so shift according to
    # the minValue value, if it is below 1.
    # Shift it by 2, 1 to make the zeros into two, the other for the
    # NAs to be ones.

    # If object is real numbers, the default above is to discretize.
    # This is particularly bad for numbers below 10.
    # Here, numbers below maxNumCols that are reals will be rescaled
    #  to max = 100.
    # These are, of course, only used for the color matrix, not the
    #  values on the Raster.

    # Plotting works by making a maxNumCols value raster if
    #  it is a real value original raster. So, we need to keep
    #  the original minz and maxz for legend purposes, but
    #  work with the new rescaled minz and maxz
    minzOrig <- minz
    maxzOrig <- maxz
    whichZero <- numeric()
    whichZeroLegend <- numeric()
    if (!is.null(zero.color)){
      whichZero <- which(z==0)
      whichZeroLegend <- which(seq(minz, maxz, length.out=nValues)==0)
    }

    # Here, rescale so it is between 0 and maxNumCols or nValues
    if (real) {#& (maxz <= maxNumCols) ) {
      z <- maxNumCols / (maxz - minz) * (z - minz)
      if (length(whichZero)) {
        zeroValue <- maxNumCols / (maxz - minz) * (0 - minz)
      }
      # rescale so the minimum is 1, not <1:
      #z <- z + (((maxNumCols / maxz * minz) < 1) *
      #            (-(maxNumCols / maxz * minz) + 1))
    } else {
      # rescale so that the minimum is 1, not <1:
      z <- (nValues - 1) /  (maxz - minz) * (z - minz) + 1
      if (length(whichZero)) {
        zeroValue <- (nValues - 1) / (maxz - minz) * (0 - minz) + 1
      }
    }
    minz <- suppressWarnings(min(z, na.rm = TRUE))
    maxz <- suppressWarnings(max(z, na.rm = TRUE))
    if (is.infinite(minz)) {
      maxz <- 0
    }
    if (is.infinite(minz)) {
      minz <- 0
    }

    if (any(!is.na(legendRange))) {
      if ((max(legendRange) - min(legendRange) + 1) < length(cols)) {
      } else {
        #minz <- min(legendRange)
        #maxz <- max(legendRange)
        #minzOrig <- minz
        #maxzOrig <- maxz
        if (is.null(colTable)) {
          #cols <- colorRampPalette(cols)(maxz - minz + 1)
        } else {
          if (length(getColors(grobToPlot)[[1]]) > 0) {
            cols <- colorRampPalette(colTable)(maxzOrig - minzOrig + 1)
          } else {
            # default color if nothing specified
            cols <- rev(terrain.colors(maxzOrig - minzOrig + 1))
          }
        }
      }
    }

    # here, the default color (transparent) for zero:
    # if it is the minimum value, can be overridden.

    # if range of values is not within the legend range, then give them NA
    if (minz < 0) z[z < 0] <- 0
    if (real) {
      if (maxz > maxNumCols) z[z > maxNumCols] <- 0
    } else {
      if (maxz > nValues) z[z > nValues] <- 0
    }

    z <- z + 1 # for the NAs
    z[is.na(z)] <- 1 # max(1, minz)

    if (raster::is.factor(grobToPlot) & !is.null(colTable)) {
      cols <- rep(na.color,max(factorValues))
      cols[factorValues] <- colTable
    }
    if (length(whichZeroLegend)) {
      cols[whichZeroLegend] <- zero.color
    }
    cols <- c(na.color, cols) # make first index of colors be transparent

    # Convert numeric z to a matrix of hex colors
    z <- matrix(
      cols[z], nrow = NROW(grobToPlot),
      ncol = ncol(grobToPlot), byrow = TRUE
    )

    list(
      z = z, minz = minzOrig, maxz = maxzOrig,
      cols = cols, real = real
    )
  }
)

#' Divergent colour palette
#'
#' Creates a palette for the current session for a divergent-color graphic with
#' a non-symmetric range.
#' Based on ideas from Maureen Kennedy, Nick Povak, and Alina Cansler.
#'
#' @param start.color  Start colour to be passed to \code{colorRampPalette}.
#' @param end.color    End colour to be passed to \code{colorRampPalette}.
#' @param min.value    Numeric minimum value corresponding to \code{start.colour}.
#' @param max.value    Numeric maximum value corresponding to \code{end.colour}.
#' @param mid.value    Numeric middle value corresponding to \code{mid.colour}.
#'                     Default is \code{0}.
#' @param mid.color    Middle colour to be passed to \code{colorRampPalette}.
#'                     Defaults to \code{"white"}.
#'
#' @return A diverging colour palette.
#'
#' @seealso \code{\link{colorRampPalette}}
#' @docType methods
#' @aliases divergentColours
#' @importFrom  grDevices colorRampPalette
#' @export
#' @author Eliot McIntire and Alex Chubaty
#'
#' @examples
#' divergentColors("darkred", "darkblue", -10, 10, 0, "white")
setGeneric("divergentColors",
           function(start.color, end.color, min.value, max.value,
                    mid.value = 0, mid.color = "white") {
             standardGeneric("divergentColors")
})

#' @rdname divergentColors
#' @aliases divergentColours
setMethod(
  "divergentColors",
  signature = c("character", "character", "numeric", "numeric",
                "numeric", "character"),
  definition = function(start.color, end.color, min.value, max.value,
                        mid.value = 0, mid.color = "white") {
  ramp1 <- colorRampPalette(c(start.color, mid.color))
  ramp2 <- colorRampPalette(c(mid.color, end.color))

  # now specify the number of values on either side of "mid.value"
  max.breaks <- floor((max.value - mid.value) + 1)
  min.breaks <- floor((mid.value - min.value) + 1)

  # num.breaks <- max(max.breaks, min.breaks)
  low.ramp <- ramp1(min.breaks)
  high.ramp <- ramp2(max.breaks)
  if (min.breaks == 1) { low.ramp <- mid.color }

  # now create a combined ramp from the higher values of "low.ramp" and
  # the lower values of "high.ramp", with the longer one using all values
  # high.ramp starts at 2 to avoid duplicating zero

  myColors <- c(low.ramp[1:min.breaks], high.ramp[2:max.breaks])

  return(myColors)
})
