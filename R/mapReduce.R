if(getRversion() >= "3.1.0") utils::globalVariables("row_number")

#####################
#' rasterizeReduced - convert reduced representation to full raster
#'
#' @param reduced \code{data.frame} or \code{data.table} that has at least one column
#' of codes that are represented in the \code{fullRaster}
#'
#' @param fullRaster \code{RasterLayer} of codes used in \code{reduced} that represents
#' a spatial representation of the data
#'
#' @param plotCol a character, length 1, with the name of the column in \code{reduced} that
#' whose value will be plotted
#'
#' @param mapcode a character, length 1, with the name of the column in \code{reduced} that
#' is represented in \code{fullRaster}
#'
#' @param ... Other arguments.
#'
#' @return A \code{RasterLayer} of with same dimensions as \code{fullRaster} representing
#' \code{plotCol} spatially, according to the join between the \code{mapcodeAll} contained within
#' \code{reduced} and \code{fullRaster}
#'
#' @seealso \code{\link{raster}}
#'
#' @rdname rasterizeReduce
#' @docType methods
#' @export
#' @import data.table
#' @import raster
#' @include environment.R
#' @author Eliot McIntire
#' @examples
#' \dontrun{
#' Ras <- raster(extent(0,100,0,100), res=1)
#' fullRas <- randomPolygons(Ras, numTypes=5, speedup=1, p=0.3)
#' names(fullRas) <- "mapcodeAll"
#' uniqueComms <- unique(fullRas)
#' reducedDT <- data.table(mapcodeAll=uniqueComms,
#'    communities=sample(1:1000,length(uniqueComms)),
#'    biomass=rnbinom(length(uniqueComms),mu=4000,0.4),
#'    ANPP=rpois(length(uniqueComms), 200))
#' biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
#' communities <- rasterizeReduced(reducedDT, fullRas, "communities")
#' ANPP <- rasterizeReduced(reducedDT, fullRas, "ANPP")
#' setColors(communities) <- c("blue","orange","red")
#' setColors(ANPP) <- c("light green", "dark green")
#' Plot(biomass, communities, ANPP, fullRas, new=T)
#' }
rasterizeReduced <- function(reduced, fullRaster, plotCol, mapcode=names(fullRaster), ...) {
  reduced <- data.table(reduced)
  if(!is.null(key(reduced))){
    if(key(reduced)!=mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }
  fullRasterVals <- data.table(getValues(fullRaster))# %>% data.frame
  setnames(fullRasterVals,1,new=mapcode)
  fullRasterVals <- fullRasterVals[, row_number:=1L:.N] # %>% mutate(row_number=1L:nrow(.)) %>% data.table
  if(!is.null(key(fullRasterVals))){
    if(key(fullRasterVals)!=mapcode) {
      setkeyv(fullRasterVals, mapcode)
    }
  } else {
    setkeyv(fullRasterVals, mapcode)
  }

  BsumVec <- reduced[fullRasterVals]
  BsumVec[is.na(get(plotCol)), c(plotCol):=0]
  setkey(BsumVec, row_number)
  ras <- as.character(match.call(expand.dots=TRUE)$reduced)
  assign(ras,
         value = raster(res=res(fullRaster),ext=extent(fullRaster), vals=BsumVec[[plotCol]])
  )
  return(get(ras))
  #Plot(get(ras), ...)
}
