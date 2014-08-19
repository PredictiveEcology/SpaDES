##############################################################
#' Get colours for plotting Raster* objects.
#'
#' @param x     A \code{Raster*} object.
#'
#' @return Returns a named list of colors.
#'
#' @export
#' @docType methods
#' @rdname getColors-method
#'
#' @seealso \code{\link{setColors}}, \code{\link{brewer.pal}}
#'
#' @author Alex Chubaty
#'
setGeneric("getColors", function(object) {
  standardGeneric("getColors")
})

#' @rdname getColors-method
setMethod("getColors",
          signature="Raster",
          definition=function(object) {
            cols <- lapply(names(object), function(x) {
              as.character(object[[x]]@legend@colortable)
            })
            names(cols) <- names(object)
            return(cols)
})

##############################################################
#' Set colours for plotting Raster* objects.
#'
#' @param x     A \code{Raster*} object.
#'
#' @param value  Named list of hex color codes (e.g., from \code{RColorBrewer::brewer.pal}),
#'              corresponding to the names of RasterLayers in \code{x}.
#'
#' @param n     An optional vector of values specifiying the number
#'              of levels from which to interpolate the color palette.
#'
#' @param ...   Additional arguments to \code{colorRampPalette}.
#'
#' @return Returns a Raster with the colortable slot set to values.
#'
#' @import RColorBrewer
#' @export
#' @docType methods
#' @rdname setColors-method
#'
#' @seealso \code{\link{brewer.pal}}, \code{\link{colorRampPalette}}.
#'
#' @author Alex Chubaty
#'
setGeneric("setColors<-",
           function(object, value, ..., n) {
             standardGeneric("setColors<-")
})

#' set colortable of a raster object
#' @name setColors<-
#' @aliases setColors<-RasterLayer,character,numeric-method
#' @rdname setColors-method
setReplaceMethod("setColors",
                 signature("RasterLayer", "character", "numeric"),
                 function(object, value, ..., n) {
                   pal <- colorRampPalette(value)
                   object@legend@colortable <- pal(n)
                   validObject(object)
                   return(object)
})

#' set colortable of a raster object
#' @name setColors<-
#' @aliases setColors<-RasterLayer,character,missing-method
#' @rdname setColors-method
setReplaceMethod("setColors",
                 signature("RasterLayer", "character", "missing"),
                 function(object, value, ...) {
                   n <- round((maxValue(object)-minValue(object)))+1
                   pal <- colorRampPalette(value)
                   object@legend@colortable <- pal(n)
                   object@legend@colortable <- value
                   validObject(object)
                   return(object)
})

#' set colortable of a raster object
#' @name setColors<-
#' @aliases setColors<-Raster,list,numeric-method
#' @rdname setColors-method
setReplaceMethod("setColors",
                 signature("Raster", "list", "numeric"),
                 function(object, value, ..., n) {
                   for(x in names(object)) {
                     setColors(object[[x]], ..., n=n) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})

#' set colortable of a raster object
#' @name setColors<-
#' @aliases setColors<-Raster,list,missing-method
#' @rdname setColors-method
setReplaceMethod("setColors",
                 signature("Raster", "list", "missing"),
                 function(object, value, ...) {
                   for(x in names(object)) {
                     setColors(object[[x]], ...) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})
