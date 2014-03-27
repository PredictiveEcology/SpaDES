#' The \code{agent} class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#'              
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note add additional notes here.
#' 
#' @name agent
#' @rdname agent-class
#' @aliases agent-class
#' @exportClass agent
#' 
setClass("agent", slots=list(ID="character", other="list"),
         prototype=list(ID=NA_character_))

#' The \code{rasterAgent} class.
#'
#' Extends the \code{agent} class by making it spatial.
#' Describes spatial agents whose  spatial components are characterized by
#' a raster (i.e., in contrast to points or a polygon).
#' 
#' This is a virtual class and not intended to be used directly.
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#'              
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note This is a virtual class and not intended to be used directly.
#' 
#' @name rasterAgent
#' @rdname rasterAgent-class
#' @aliases rasterAgent-class
#' @exportClass rasterAgent
#' 
setClass("rasterAgent", slots=list(ID="character", other = "list"), contains="agent")


#' The \code{vectorAgent} class.
#'
#' Extends the \code{agent} class by making it spatial.
#' Describes spatial agents whose  spatial components are characterized by
#' points or a polygon (i.e., in contrast to a raster).
#' 
#' This is a virtual class and not intended to be used directly.
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#'              
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note This is a virtual class and not intended to be used directly.
#' 
#' @name vectorAgent
#' @rdname vectorAgent-class
#' @aliases vectorAgent-class
#' @exportClass vectorAgent
#' 
setClass("vectorAgent", slots=list(ID="character", other = "list"), contains="agent")


#' The \code{polygonAgent} class.
#'
#' Extends the \code{vectorAgent} class. Describes spatial agents whose spatial
#' components are characterized by polygons (i.e., in contrast to points or a raster).
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#' 
#' @slot spatial    A \code{SpatialPolygons} object.
#'          
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note add additional notes here.
#' 
#' @name polygonAgent
#' @rdname polygonAgent-class
#' @aliases polygonAgent-class
#' @importClassesFrom sp SpatialPolygons
#' @exportClass polygonAgent
#' 
setClass("polygonAgent", slots=list(spatial="SpatialPolygons"), contains="vectorAgent")


#' The \code{pointAgent} class.
#'
#' Extends the \code{vectorAgent} class. Describes spatial agents whose spatial
#' components are characterized by points (i.e., in contrast to a polygon).
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#' 
#' @slot spatial    A \code{SpatialPoints} object.
#' 
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note add additional notes here.
#' 
#' @name pointAgent
#' @rdname pointAgent-class
#' @aliases pointAgent-class
#' @importClassesFrom sp SpatialPoints
#' @exportClass pointAgent
#' 
setClass("pointAgent", slots=list(spatial="SpatialPoints"), contains="vectorAgent")


#' The \code{spreadAgent} class.
#'
#' Extends the \code{rasterAgent} class. Describes spatial agents whose
#' spatial component ("area") is characterized by a raster and are capable
#' of spreading (growing/shrinking).
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#' 
#' @slot NumPix    A numeric object describing the number of raster pixels occupied.
#' 
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note add additional notes here.
#' 
#' @name spreadAgent
#' @rdname spreadAgent-class
#' @aliases spreadAgent-class
#' @exportClass spreadAgent
#' 
setClass("spreadAgent", slots=list(NumPix="numeric"),
         prototype=list(NumPixels=NA_integer_), contains="rasterAgent")


#' The \code{mobileAgent} class.
#'
#' Extends the \code{pointAgent} class. Describes non-stationary spatial agents whose
#' spatial #' components are characterized by points (i.e., in contrast to a polygon).
#'
#' @slot ID     A character string that identifies the individual agent.
#'              By default, the numbers 1...n are used.
#' 
#' @slot spatial    A \code{SpatialPoints} object.
#' 
#' @slot distance   A numeric describing the agent's heading (in degrees) from it's previous position.
#' 
#' @slot heading    A numeric describing the agent's distance from previous position.
#' 
#' @slot other  A list for storing other (user-specific) agent attributes.
#' 
#' @note add additional notes here.
#' 
#' @name mobileAgent
#' @rdname mobileAgent-class
#' @aliases mobileAgent-class
#' @importClassesFrom sp SpatialPoints
#' @exportClass mobileAgent
#' 
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"),
         prototype=list(heading=NA_real_, distance=NA_real_), contains="pointAgent")
