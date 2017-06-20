
if (!isGeneric("Plot")) {
  setGeneric("Plot", function(..., new, addTo, gp, gpText, gpAxis, axes,
                              speedup, size, cols, zoomExtent, visualSqueeze,
                              legend, legendRange, legendText, pch, title,
                              na.color, zero.color, length){
    standardGeneric("Plot")
  })
}

#' Plot method for \code{simList} objects
#'
#' Extends \code{\link[quickPlot]{Plot}} for \code{simList} objects.
#'
#' Plot for simList class objects
#'
#' See \code{\link[quickPlot]{Plot}}. This method strips out stuff
#' from a simList class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems,
#' or machines. This will likely still not allow identical digest
#' results across R versions.
#'
#' @importFrom quickPlot Plot gpar
#' @importMethodsFrom quickPlot Plot
#' @inheritParams quickPlot::Plot
#' @include simList-class.R
#' @seealso \code{\link[quickPlot]{Plot}}
#' @exportMethod Plot
#' @rdname Plot
#' @export
setMethod(
  "Plot",
  signature("simList"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes,
                        speedup, size, cols, zoomExtent, visualSqueeze,
                        legend, legendRange, legendText, pch, title,
                        na.color, zero.color, length) {
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.
    sim <- list(...)[[1]]
    plotList <- ls(sim@.envir, all.names = TRUE)
    plotables <- sapply(plotList, function(x)
      is(get(x, envir = sim@.envir), ".spadesPlottables"))
    if (any(plotables)) {
      plotObjects <- mget(plotList[plotables], sim@.envir) %>%
        append(., list(env = sim@.envir))
      do.call(Plot, plotObjects)
    }
  })



if (!isGeneric(".parseElems")) {
  setGeneric(".parseElems", function(object, objects,
                                     compareRasterFileLength = 1e6,
                                     algo = "xxhash64") {
    standardGeneric(".parseElems")
  })
}

#' .parseElems for simList class objects
#'
#' See \code{\link[quickPlot]{.parseElems}}.
#'
#' @importFrom quickPlot .parseElems
#' @importMethodsFrom quickPlot .parseElems
#' @inheritParams quickPlot::.parseElems
#' @rdname parseElems
#' @include simList-class.R
#' @seealso \code{\link[quickPlot]{.parseElems}}
#' @exportMethod .parseElems
#' @export
setMethod(
  ".parseElems",
  signature = "simList",
  definition = function(tmp, elems, envir) {
    useElem <- 1
    # If the user is passing a sub-element to say a Raster Stack
    if (length(rev(elems)[-1]) > 1) {
      # Only RasterStack implemented yet
      if (is(get(deparse(rev(elems)[[2]]), envir = tmp@.envir), "RasterStack")) {
        useElem <- 2
      }
    }
    eval(parse(text=deparse(elems[[useElem]])), envir=tmp@.envir)

  })
