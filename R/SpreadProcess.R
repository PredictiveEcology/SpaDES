##############################################################
#' Simulate a spread process on a landscape.
#'
#' More detailed description needed here.
#'
#' @param landscape     A \code{RasterLayer} object.
#' 
#' @param loci          A vector of locations in \code{landscape}
#' 
#' @param spreadProb    Numeric or rasterLayer. The overall probability of spreading, or probability raster
#' driven.
#' 
#' @param persistance   A probability that a burning cell will continue to burn, per time step.
#' 
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
#'                      whose elements are \code{0,1}, where 1 indicates "cannot spread to".
#' 
#' @param maxSize       The maximum number of pixels for a fire. This is currently 
#'                      only a single number, not one for each spread event
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
#' @name spread
#' @author Steve Cumming \email{Steve.Cumming@@sbf.ulaval.ca}
#' @author Eliot McIntire 
#' @rdname spread
#' 
setGeneric("spread", function(landscape, loci, spreadProb, persistance, 
                              mask, maxSize, directions, iterations, ...) {
  standardGeneric("spread")
})

#' @param plot.it    If TRUE, then plot the raster at every iteraction, so one can watch the 
#' spread event grow.
#' 
#' @param mergeDuplicates Logical. While spreading is occuring, and the same pixel gets identified
#'                      as being adjacent to two different active cells, run "unique" or not. Default is FALSE.
#' 
#' @import raster
#' @rdname spread
#' @name spread
#' 
#' @examples
#' library(raster)
#' library(RColorBrewer)
#' 
#' # Make random forest cover map
#' a = raster(extent(0,1e2,0,1e2),res=1)
#' hab = GaussMap(a,speedup=1)
#' names(hab)="hab"
#' cells = loci = b = as.integer(sample(1:ncell(a),1e1))
#' mask = raster(a)
#' mask = setValues(mask, 0)
#' mask[1:5000] <- 1
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' directions=8
#' 
#' # Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent
#' cols = list(c("#00000000",brewer.pal(8,"RdYlGn")[8:1]),brewer.pal(9,"Greys"),brewer.pal(8,"Spectral"))
#' 
#' dev(2)
#' simPlot(hab,col=cols[[2]],speedup=1)
#' names(hab)<-"hab"
#' fire2 <- spread(hab,loci=as.integer(sample(1:ncell(hab),10)),
#'                 0.235,0,NULL,1e8,8,1e6,mergeDuplicates = T,
#'                 plot.it=T,col=cols[[1]],delete.previous=F,add=T,on.which.to.plot="hab",
#'                 speedup=1)
setMethod("spread",
          signature(landscape="RasterLayer"#, loci="integer", 
                    #spreadProb="numeric"
                    #persistance="numeric", 
                    #mask="RasterLayer", maxSize="numeric",
                    #directions="integer", iterations="integer"
                    ),
          definition = function(landscape, loci, spreadProb, persistance,
                                mask, maxSize=ncell(landscape), directions = 8, 
                                iterations = ncell(landscape), 
                                plot.it=FALSE, mergeDuplicates = FALSE, ...) {
            ### should sanity check map extents
            
            if (is.null(loci))  {
              # start it in the centre cell
              loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
            }
            
            spreads <- rep_len(0,ncell(landscape))#data.table(ind=1:ncell(landscape), burned=0, key="ind")
            if(!is.null(mask)) {
              masked<-Which(mask==0,cells=T)#getValues(mask)==0
              #  spreads[masked]<- NaN#[potentials %in% masked]]
              
            }
            n <- 1
            spreads[loci]<-n
            size <- length(loci)
            
            if (is.null(iterations)) {
              iterations = Inf # this is a stupid way to do this!
            } 
            
            while ( (length(loci)>0) && (iterations>=n) ) {
              potentials <- adj(landscape, loci, directions, pairs=F)
              
              # drop those ineligible
              if (!is.null(mask))
                potentials <- potentials[potentials %in% masked]
                
              # Should this be unique?
              # only accept cells that have no fire yet
              if (mergeDuplicates)
                potentials <- unique(potentials[spreads[potentials]==0])
              else 
                potentials <- potentials[spreads[potentials]==0]
#               } else {
#                 if (mergeDuplicates)
#                   potentials <- unique(potentials[spreads[potentials]==0])
#                 else 
#                   potentials <- potentials[spreads[potentials]==0]
#               }
              
              # select which potentials actually happened
              # nrow() only works if potentials is an array
              if (is.numeric(spreadProb)) {
                #  ItHappened <- runif(nrow(potentials)) <= spreadProb
                  spreadProbs <- spreadProb
                } else {
                  spreadProbs <- spreadProb[potentials]
              }
              
              events <- potentials[runif(length(potentials))<=spreadProbs]
              
              # Implement maxSize
              if((size+length(events)) > maxSize) {
                keep<-length(events) - ((size+length(events)) - maxSize)
                events<-events[sample(length(events),keep)]
              }
              
              size <- size + length(unique(events))
              
              # update eligibility map
              
              n <- n+1
              spreads[events] <- n
              
              if(size >= maxSize) {
                events <- NULL
              }
              
              # drop or keep loci
              if (is.null(persistance) | is.na(persistance)) {
                loci <- NULL
              } else {
                if (inRange(persistance)) {
                  loci <- loci[runif(length(loci))<=persistance]
                } else {
                  # here is were we would handle methods for raster* or functions
                  stop("Unsupported type: persistance")
                }
              }
              
              loci <- c(loci, events)
              
              if (plot.it){
#                 if (!is.null(events)) {
#                 pts = SpatialPoints(xyFromCell(landscape,events))
#                 simPlot(x=pts,on.which.to.plot="fire",add=T,pch=15,
#                         delete.previous = F, gp=gpar(cex=0.5))
#                 }
                top <- raster(landscape)
                top <- setValues(top,spreads)
                simPlot(top, ...)
              }
              #    simPlot(raster(matrix(spreads,ncol=10,nrow=10,byrow=T)),col=c("grey","black"))
              
            }
            
            # Convert the data.table back to raster
            spre <- raster(landscape)
            spre <- setValues(spre, spreads)
            return(spre)
          }
          
)
