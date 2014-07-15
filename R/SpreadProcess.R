##############################################################
#' Simulate a spread process on a landscape.
#'
#' More detailed description needed here.
#'
#' @param landscape     A \code{RasterLayer} object.
#' 
#' @param loci          A vector of locations in \code{landscape}
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
#' @param iterations    Number of iterations to spread. Leaving this \code{NULL} allows the spread
#'                      to continue until stops spreading itself (i.e., exhausts itself).
#' 
#' @return A \code{RasterLayer} indicating the spread of the process in the landscape.
#' 
#' @import raster
#' @export
#' @docType methods
#'
#' @examples
#'  \dontrun{tmp <- raster(nrows=10, ncols=10, vals=0)}
#'  \dontrun{plot(tmp)}
#'  \dontrun{tmp <- spread(tmp, spreadProb=0.225)}
#'  \dontrun{plot(tmp)}
#'  
#' @name spread  
#' @author Steve Cumming \email{Steve.Cumming@@sbf.ulaval.ca}
#' @rdname spread-method
#' 
setGeneric("spread", function(landscape, loci, spreadProb, persistance, 
                              mask, maxSize, directions, iterations, ...) {
    standardGeneric("spread")
})

# defaults:
# (landscape, loci=NULL, spreadProb=0.1, persistance=NULL, mask=NULL, maxSize=NULL, directions=8, iterations=NULL)

### ALLOW:
### landscape:      RasterLayer, RasterStack
### loci:           integer, SpatialPoints
### spreadProb:     [0,1], function, RasterLayer
### persistance:    [0,1], function, RasterLayer
### mask:           RasterLayer
### maxSize:        integer?
### directions:     integer
### iterations:     intger

#' @import raster
#' @rdname spread-method
setMethod("spread",
          signature(landscape="RasterLayer", loci="integer", spreadProb="numeric",
                    persistance="numeric", mask="RasterLayer", maxSize="numeric",
                    directions="integer", iterations="integer"),
          definition = function(landscape, loci, spreadProb, persistance,
                                mask, maxSize, directions, iterations) {
              ### should sanity check map extents
                is.prob <- function(x) {
                    if (!is.numeric(x)) 
                        return(FALSE)
                    else 
                        return(!(x>1 || x<0))
                }
                
                if (is.null(loci))  {
                    # start it in the centre cell
                    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
                }
                
                spreads <- setValues(raster(landscape), 0)
                n <- 1
                spreads[loci] <- n
                
                if (is.null(iterations)) {
                    iterations = Inf # this is a stupid way to do this!
                } else {
                    # do nothing
                }
                
                while ( (length(loci)>0) && (iterations>=n) ) {
                    #print(paste(n, length(loci)))
                    potentials <- adj(landscape, loci, directions)
                    
                    # drop those ineligible
                    if (!is.null(mask)){
                        tmp <- extract(mask, potentials[,2])
                    } else {
                        tmp <- extract(spreads, potentials[,2])
                    }
                    #print(cbind(potentials,tmp))
                    potentials <- potentials[ifelse(is.na(tmp), FALSE, tmp==0),]
                                        
                    # select which potentials actually happened
                    # nrow() only works if potentials is an array
                    if (is(potentials,"numeric")) {
                      ItHappened <- runif(1) <= spreadProb
                      events <- potentials[2][ItHappened]
                    } else if (is.numeric(spreadProb)) {
                      ItHappened <- runif(nrow(potentials)) <= spreadProb
                      events <- potentials[ItHappened, 2]
                    } else {
                      stop("Unsupported type:spreadProb") # methods for raster* or function args
                    }
                    #print(events)
                    
                    # update eligibility map
                    spreads[events] <- n
                    n <- n+1
                    
                    # drop or keep loci
                    if (is.null(persistance)) {
                        loci <- NULL
                    } else {
                        if (is.prob(persistance)) {
                            loci <- loci[runif(length(loci))<=persistance]
                        } else {
                            # here is were we would handle methods for raster* or functions
                            stop("Unsupported type: persistance")
                        }
                    }
                    
                    loci <- c(loci, events)
                    
                }
                return(spreads)
          }
)

##############################################################
#' Simulate a spread process on a landscape.
#'
#' More detailed description needed here.
#'
#' @param landscape     A \code{RasterLayer} object.
#' 
#' @param loci          A vector of locations in \code{landscape}
#' 
#' @param spreadProb    The probability of spreading.
#' 
#' @param persistance   DOCUMENTATION NEEDED
#' 
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
#'                      whose elements are \code{0,1}, where 1 indicates "cannot spread to".
#' 
#' @param maxSize       The maximum number of pixels for a fire. This is currently only a single number, 
#' not one for each spread event
#' 
#' @param directions    The number adjacent cells in which to look; default is 8 (Queen case).
#' 
#' @param iterations    Number of iterations to spread. Leaving this \code{NULL} allows the spread
#'                      to continue until stops spreading itself (i.e., exhausts itself).
#' 
#' @return A \code{RasterLayer} indicating the spread of the process in the landscape.
#' 
#' @import raster
#' @export
#' @docType methods
#'
#' @examples
#'  \dontrun{tmp <- raster(nrows=10, ncols=10, vals=0)}
#'  \dontrun{plot(tmp)}
#'  \dontrun{tmp <- spread(tmp, spreadProb=0.225)}
#'  \dontrun{plot(tmp)}
#'  
#' @name spread.adj
#' @author Steve Cumming \email{Steve.Cumming@@sbf.ulaval.ca}
#' @rdname spread.adj-method
#' 
setGeneric("spread.adj", function(landscape, loci, spreadProb, persistance, 
                              mask, maxSize, directions, iterations, ...) {
  standardGeneric("spread.adj")
})

#' @param plot.it    If TRUE, then plot the raster at every iteraction, so one can watch the 
#' spread event grow.
#' @import raster
#' @rdname spread.adj-method
setMethod("spread.adj",
          signature(landscape="RasterLayer",# loci="integer", 
                    spreadProb="numeric"
                    #persistance="numeric", 
                    #mask="RasterLayer"#, maxSize="numeric",
                    #directions="integer", iterations="integer"
                    ),
          definition = function(landscape, loci, spreadProb, persistance,
                       mask, maxSize, directions, iterations, plot.it=FALSE,...) {
  ### should sanity check map extents
  is.prob <- function(x) {
    if (!is.numeric(x)) 
      return(FALSE)
    else 
      return(!(x>1 || x<0))
  }
  
  if (is.null(loci))  {
    # start it in the centre cell
    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
  }
  
  #spreads <- setValues(raster(landscape), 0)
  spreads <- data.table(ind=1:ncell(landscape),burned=0,key="ind")
  if(!is.null(mask))
    masked<-getValues(mask)==0
  n <- 1
  spreads[loci,burned:=n]
  size <- length(loci)
  #spreads[4,burned:=NA]
  
  if (is.null(iterations)) {
    iterations = Inf # this is a stupid way to do this!
  } else {
    # do nothing
  }
  
  while ( (length(loci)>0) && (iterations>=n) ) {
    #print(paste(n, length(loci)))
    potentials <- adj(landscape, loci, directions,as.data.table=TRUE)
    setkey(potentials,to)
    
    # drop those ineligible
    if (!is.null(mask)){
      potentials <- spreads[masked][potentials][burned==0][,burned:=NULL]
    } else {
      potentials <- spreads[potentials][burned==0][,burned:=NULL]
    }
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (!is.numeric(spreadProb)) {
      #  ItHappened <- runif(nrow(potentials)) <= spreadProb
      #} else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }
    
    events <- potentials[runif(nrow(potentials))<=spreadProb,ind]
    
    # Implement maxSize
    if((size+length(events)) > maxSize) {
      keep<-length(events) - ((size+length(events)) - maxSize)
      events<-events[sample(length(events),keep)]
    }
    
    size <- size + length(unique(events))
    
    # update eligibility map
    
    n <- n+1
    spreads[events,burned:=n]
    
    if(size >= maxSize) {
      events<-NULL
    }
    
    # drop or keep loci
    if (is.null(persistance)) {
      loci <- NULL
    } else {
      if (is.prob(persistance)) {
        loci <- loci[runif(length(loci))<=persistance]
      } else {
        # here is were we would handle methods for raster* or functions
        stop("Unsupported type: persistance")
      }
    }
    
    loci <- c(loci, events)
    
    if (plot.it){
      top <- raster(landscape)
      top<-setValues(top,spreads[,burned])
      simplot(top,,...)
    }
    
  }
  #loci = sample(1:ncell(a),20)
  spre=raster(landscape)
  spre<-setValues(spre, spreads[,burned])
  return(spre)
}
)

