if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".N", "row_number"))
}

###############################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced \code{data.frame} or \code{data.table} that has at least one
#' column of codes that are represented in the \code{fullRaster}.
#'
#' @param fullRaster \code{RasterLayer} of codes used in \code{reduced} that represents
#' a spatial representation of the data
#'
#' @param newRasterCols Character vector, length 1 or more, with the name(s) of the column(s) 
#'                      in \code{reduced} whose value will be returned as a Raster or list of 
#'                      Rasters
#'
#' @param mapcode a character, length 1, with the name of the column in \code{reduced} that
#' is represented in \code{fullRaster}
#'
#' @param ... Other arguments. None used yet.
#'
#' @return A \code{RasterLayer} or list of \code{RasterLayer} of with same 
#' dimensions as \code{fullRaster} representing
#' \code{newRasterCols} spatially, according to the join between the \code{mapcode} contained within
#' \code{reduced} and \code{fullRaster}
#'
#' @seealso \code{\link{raster}}
#'
#' @rdname rasterizeReduce
#' @docType methods
#' @export
#' @importFrom data.table data.table key setkeyv setnames ':='
#' @importFrom raster extent getValues raster res
#' @include environment.R
#' @author Eliot McIntire
#' @examples
#' library(data.table)
#' library(raster)
#' Ras <- raster(extent(0,15,0,15), res=1)
#' fullRas <- randomPolygons(Ras, numTypes=2)
#' names(fullRas) <- "mapcodeAll"
#' uniqueComms <- unique(fullRas)
#' reducedDT <- data.table(mapcodeAll=uniqueComms,
#'    communities=sample(1:1000,length(uniqueComms)),
#'    biomass=rnbinom(length(uniqueComms),mu=4000,0.4))
#' biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
#'
#' # The default key is the layer name of the fullRas, so even
#' # if the reducedDT is miskeyed
#' setkey(reducedDT, biomass)
#'
#' communities <- rasterizeReduced(reducedDT, fullRas, "communities")
#' setColors(communities) <- c("blue", "orange", "red")
#' if (interactive()) {
#'   clearPlot()
#'   Plot(biomass, communities, fullRas)
#' }
rasterizeReduced <- function(reduced, fullRaster, newRasterCols, mapcode = names(fullRaster), ...) {

  reduced <- data.table(reduced)
  if (!is.null(key(reduced))) {
    if (key(reduced) != mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }
  fullRasterVals <- data.table(getValues(fullRaster))
  setnames(fullRasterVals, 1, new = mapcode)
  fullRasterVals <- fullRasterVals[, row_number := 1L:.N]
  setkeyv(fullRasterVals, mapcode)

  BsumVec <- reduced[fullRasterVals] # join
  if(length(newRasterCols)>1) {
    for(i in seq_along(newRasterCols)) {
      BsumVec[is.na(get(newRasterCols[i])), c(newRasterCols[i]) := NA]
    }
  } else {
    BsumVec[is.na(get(newRasterCols)), c(newRasterCols) := NA]
  }
  setkey(BsumVec, row_number)
  if(length(newRasterCols)>1) {
    ras <- list()
    for(i in newRasterCols) {
      #ras[[i]] <- as.character(match.call(expand.dots = TRUE)$reduced)
      ras[[i]] <- raster(res = res(fullRaster), ext = extent(fullRaster),
                               vals = BsumVec[[i]])
    }
  } else {
    #ras <- as.character(match.call(expand.dots = TRUE)$reduced)
    ras <- raster(res = res(fullRaster), ext = extent(fullRaster),
                                    vals = BsumVec[[newRasterCols]])
    
  }
  return(ras)
}
