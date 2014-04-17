##############################################################
#' Simulate a spread process on a landscape.
#'
#' More detailed description needed here.
#'
#' @param landscape     A \code{RasterLayer} object.
#' 
#' @param loci          A list(?) of locations in \code{landscape}
#' 
#' @param spreadProb    The probability of spreading.
#' 
#' @param persistance   DOCUMENTATION NEEDED
#' 
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
#'                      whose elements are \code{0,1}, where 1 indicates "cannot spread to".
#' 
#' @param maxSize       DOCUMENTATION NEEDED
#' 
#' @param directions    The number adjacent cells in which to look; default is 8 (Queen case).
#' 
#' @return A \code{RasterLayer} indicating the spread of the process in the landscape.
#' 
#' @import raster
#' @export
#' @docType methods
#' @rdname SpreadEvents-method
#'
#' @examples
#'  \dontrun{tmp <- raster(nrows=10, ncols=10, vals=0)}
#'  \dontrun{plot(tmp)}
#'  \dontrun{tmp <- SpreadEvents(tmp, spreadProb=0.225)}
#'  \dontrun{plot(tmp)}
#'  
#'  @author Steve Cumming <Steve.Cumming@sbf.ulaval.ca>
#' 
SpreadEvents = function(landscape, loci=NULL, spreadProb=0.1,
             persistance=NULL, mask=NULL, maxSize=NULL, directions=8) {
              ### should sanity check map extents
                is.prob <- function(x){
                    if (!is.numeric(x)) 
                        return(FALSE)
                    else 
                        return(!(x>1 || x<0))
                }
                
                if (is.null(loci))
                    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols    
            
                Spreads <- setValues(raster(landscape), 0)
                n <- 1
                Spreads[loci] <- n
               
                while (length(loci)>0){
                  #print(paste(n, length(loci)))
                  Potentials <- adjacent(landscape, loci, directions)
                  #drop those inelgible
                  if (!is.null(mask)){
                    tmp <- extract(mask, Potentials[,2])
                    Potentials <- Potentials[ifelse(is.na(tmp), FALSE, tmp == 0),]
                  }
                  tmp <- extract(Spreads, Potentials[,2])
                  #print(cbind(Potentials,tmp))
                  Potentials <- Potentials[ifelse(is.na(tmp), FALSE, tmp == 0),]
                 
                  #select which potentials actually happened
                  #nrow() only works if Potentials is an array
                  if (is.numeric(spreadProb))
                    ItHappened <- runif(nrow(Potentials)) < spreadProb
                  else
                    stop("Unsupported type:spreadProb") #methods for raster* or function args
                  Events <- Potentials[ItHappened, 2]
                  #print(Events)
                  #update eligibility map
                  Spreads[Events] <- n
                  n <- n+1
                  #drop or keep loci
                  
                  if (is.null(persistance))
                      loci <- NULL
                  else {
                      if (is.prob(persistance)) 
                        loci <- loci[runif(length(loci))<persistance]
                      else
                        stop("Unsupported type: persistance") #here is were we would handle methods
                                                              #for raster* or functions
                  }
                 
                  loci <- c(loci, Events)
                  
                }
                return(Spreads)
}