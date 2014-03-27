#' The \code{agent} class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name agent-class
#' @rdname agent-class
#' @exportClass agent
setClass("agent", slots=list(ID="character", other="list"),
         prototype=list(ID=NA_character_))

#' The \code{rasterAgent} class.
#'
#' Extends the \code{agent} class by making it spatial.
#' Describes spatial agents whose  spatial components are characterized by
#' a raster (i.e., in contrast to points or a polygon).
#' This is a virtual class and not intended to be used directly.
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name rasterAgent-class
#' @rdname rasterAgent-class
#' @exportClass rasterAgent
setClass("rasterAgent", slots=list(ID="character", other = "list"), contains="agent")


#' The \code{vectorAgent} class.
#'
#' Extends the \code{agent} class by making it spatial.
#' Describes spatial agents whose  spatial components are characterized by
#' points or a polygon (i.e., in contrast to a raster).
#' This is a virtual class and not intended to be used directly.
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name vectorAgent-class
#' @rdname vectorAgent-class
#' @exportClass vectorAgent
setClass("vectorAgent", slots=list(ID="character", other = "list"), contains="agent")


#' The \code{polygonAgent} class.
#'
#' Extends the \code{vectorAgent} class. Describes spatial agents whose spatial
#' components are characterized by polygons (i.e., in contrast to points or a raster).
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{spatial}{A \code{SpatialPolygons} object.}
#'    
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name polygonAgent-class
#' @rdname polygonAgent-class
#' @exportClass polygonAgent
setClass("polygonAgent", slots=list(spatial="SpatialPolygons"), contains="vectorAgent")


#' The \code{pointAgent} class.
#'
#' Extends the \code{vectorAgent} class. Describes spatial agents whose spatial
#' components are characterized by points (i.e., in contrast to a polygon).
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{spatial}{A \code{SpatialPoints} object.}
#'    
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name pointAgent-class
#' @rdname pointAgent-class
#' @exportClass pointAgent
setClass("pointAgent", slots=list(spatial="SpatialPoints"), contains="vectorAgent")


#' The \code{spreadAgent} class.
#'
#' Extends the \code{rasterAgent} class. Describes spatial agents whose
#' spatial component ("area") is characterized by a raster and are capable
#' of spreading (growing/shrinking).
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{NumPix}{A numeric object describing the number of raster pixels occupied.}
#'    
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name spreadAgent-class
#' @rdname spreadAgent-class
#' @exportClass spreadAgent
setClass("spreadAgent", slots=list(NumPix="numeric"),
         prototype=list(NumPixels=NA_integer_), contains="rasterAgent")


#' The \code{mobileAgent} class.
#'
#' Extends the \code{pointAgent} class. Describes non-stationary spatial agents whose
#' spatial #' components are characterized by points (i.e., in contrast to a polygon).
#'
#' \describe{
#'    \item{ID}{A character}
#'
#'    \item{spatial}{A \code{SpatialPoints} object.}
#'    
#'    \item{heading}{A \code{numeric} describing the agent's heading (in degrees) from it's previous position.}
#'    
#'    \item{distance}{A \code{numeric} describing the agent's distance from previous position.}
#'    
#'    \item{other}{A list for storing other (user-specific) agent attributes.}
#'  }
#' @name pointAgent-class
#' @rdname pointAgent-class
#' @exportClass pointAgent
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"),
         prototype=list(heading=NA_real_, distance=NA_real_), contains="pointAgent")
