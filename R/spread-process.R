##############################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulated fires or other things. Essentially, it starts from a collection of cells
#' (\code{loci}) and spreads to neighbours, according to the \code{directions} and \code{spreadProb} arguments.
#' This can become quite general, if \code{spreadProb} is 1 as it will expand from every loci until all pixels
#' in the landscape have been covered. With \code{mapID} set to \code{TRUE}, the resulting map will be
#' classified by the index of the pixel where that event propagated from. This can be used to examine things like
#' fire size distributions.
#'
#' @param landscape     A \code{RasterLayer} object.
#'
#' @param loci          A vector of locations in \code{landscape}
#'
#' @param spreadProb    Numeric or rasterLayer. The overall probability of spreading, or probability raster
#' driven.
#'
#' @param persistence   A probability that a burning cell will continue to burn, per time step.
#'
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
#'                      whose elements are \code{0,1}, where 1 indicates "cannot spread to". Currently
#'                      not implemented.
#'
#' @param maxSize       The maximum number of pixels for a fire. This is currently
#'                      only a single number, not one for each spread event
#'
#' @param directions    The number adjacent cells in which to look; default is 8 (Queen case).
#'
#' @param iterations    Number of iterations to spread. Leaving this \code{NULL} allows the spread
#'                      to continue until stops spreading itself (i.e., exhausts itself).
#'
#' @param ...           Additional parameters.
#'
#' @return A \code{RasterLayer} indicating the spread of the process in the landscape.
#'
#' @import raster
#' @export
#' @docType methods
#'
#' @author Steve Cumming \email{Steve.Cumming@@sbf.ulaval.ca}
#' @author Eliot McIntire
#'
#' @name spread
#' @aliases spread
#' @rdname spread-method
#'
setGeneric("spread", function(landscape, loci=ncell(landscape)/2, spreadProb=0.23,
                              persistence=0, mask=NULL, maxSize=ncell(landscape),
                              directions=8, iterations=NULL, ...) {
  standardGeneric("spread")
})

#' @param plot.it    If TRUE, then plot the raster at every iteraction, so one can watch the
#' spread event grow.
#'
#' @param mapID  Logical. If TRUE, then the returned fire map is a map of fire ids. If FALSE,
#' the returned map is the iteration number that the pixel burned
#'
#' @importFrom methods is
#' @import raster RColorBrewer
#' @rdname spread-method
#'
#' @examples
#' library(raster)
#' library(RColorBrewer)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e2,0,1e2),res=1)
#' hab <- GaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab)="hab"
#' cells <- loci <- b <- as.integer(sample(1:ncell(a),1e1))
#' mask <- raster(a)
#' mask <- setValues(mask, 0)
#' mask[1:5000] <- 1
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' directions <- 8
#'
#' # Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent
#' setColors(hab) <- paste(c("#FFFFFF",brewer.pal(8,"Greys")),c("00",rep("FF",8)),sep="")
#'
#' #dev(4)
#' Plot(hab,new=TRUE,speedup=3) # note speedup is equivalent to making pyramids,
#'                              # so, some details are lost
#'
#' # initiate 10 fires at to loci
#' fires <- spread(hab, loci=as.integer(sample(1:ncell(hab), 10)),
#'                 0.235, 0, NULL, 1e8, 8, 1e6, mapID=TRUE)
#' #set colors of raster, including a transparent layer for zeros
#' setColors(fires, 10)<-c("#00000000",brewer.pal(8,"Reds")[5:8])
#' Plot(fires)
#' Plot(fires,addTo="hab")
#'
#' #alternatively, set colors using cols= in the Plot function
#' Plot(hab,new=TRUE)
#' Plot(fires) # default color range makes zero transparent.
#' # Instead, to give a color to the zero values, use \code{zero.color=}
#' Plot(fires, addTo="hab",
#'      cols=colorRampPalette(c("orange","darkred"))(10))
#' hab2 <- hab
#' Plot(hab2)
#' Plot(fires, addTo="hab2.hab", zero.color="white",
#'      cols=colorRampPalette(c("orange","darkred"))(10))
#' # or overplot the original (NOTE: legend stays at original values)
#' Plot(fires,
#'      cols=topo.colors(10))
#'
setMethod("spread",
          signature(landscape="RasterLayer"),
          definition = function(landscape, loci, spreadProb, persistence,
                                mask, maxSize=ncell(landscape), directions=8,
                                iterations=ncell(landscape), mapID=FALSE,
                                plot.it=FALSE, ...) {
            ### should sanity check map extents

            if (is.null(loci))  {
              # start it in the centre cell
              loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
            }
            
            if(is(spreadProb,"RasterLayer")) {
              if (minValue(spreadProb)>1) stop("spreadProb is not a probability")
              if (maxValue(spreadProb)<0) stop("spreadProb is not a probability")
            } else {
              if (!inRange(spreadProb)) stop("spreadProb is not a probability")
            }
            
            spreads <- vector("integer", ncell(landscape))#data.table(ind=1:ncell(landscape), burned=0, key="ind")
            
            
            n <- 1
            if (mapID) {
              spreads[loci] <- 1:length(loci)
              if(length(maxSize) > 1){
                size <- rep_len(1, length(loci))
              } else {
                size <- length(loci)
              }      
            } else {
              spreads[loci] <- n
              size <- length(loci)
            }
            
            #size <- length(loci)
            
            if (is.null(iterations)) {
              iterations <- Inf # this is a stupid way to do this!
            }
            
            # Convert mask and NAs to 0 on the spreadProb Raster
            if (is(spreadProb, "Raster")) {
              spreadProb[is.na(spreadProb)]<-0
              if(!is.null(mask)) {
                spreadProb[mask==1]<-0
              }
            } else if (is.numeric(spreadProb)) { # Translate numeric spreadProb into a Raster
              #  if there is a mask Raster
              if(!is.null(mask)) {
                spreadProb <- raster(extent(landscape), res=res(landscape), vals=spreadProb)
                spreadProb[mask==1]<-0
              }
            }
            
            
            while ( (length(loci)>0) & (iterations>=n) ) {
              if (mapID) {
                potentials <- adj(landscape, loci, directions, pairs=TRUE)
                #potentials <- adj(landscape, loci, directions, pairs=TRUE)
              } else {
                # must pad the first column of potentials
                potentials <- cbind(NA, adj(landscape, loci, directions,
                                            pairs=FALSE))
              }
              #browser()
              
              
              #if there is only one potential, R converts this to a vector, instead of a matrix.
              # Force it back to a matrix
              #              if(length(potentials)==2) {
              #                potentials <- matrix(potentials,ncol=2)
              #              }
              
              # drop those ineligible
              #              if (!is.null(mask))
              #                potentials <- matrix(potentials[potentials[,2] %in% masked,], ncol=2)
              
              # only accept cells that have no fire yet
              # Need to call matrix because of the cast where there is only one cell
              #potentials <- matrix(potentials[spreads[potentials[,2]]==0,], ncol=2)
              potentials <- potentials[spreads[potentials[,2]]==0,,drop=FALSE]
              
              # If one pixels is selected as potential by more than one source
              #  Remove the duplication, and reorder the potentials so that it is not
              #  always the "first one", i.e., closest to top left of map, that is kept.
              if(NROW(potentials)>0) {
                if(anyDuplicated(potentials[,2])){
                  ## For testing purposes (to benchmark)
                  potentials <- potentials[sample.int(NROW(potentials)),,drop=FALSE]
                  potentials <- potentials[!duplicated(potentials[,2]),,drop=FALSE]
                }        
              }      
              
              # select which potentials actually happened
              # nrow() only works if potentials is an array
              if (is.numeric(spreadProb)) {
                #  ItHappened <- runif(nrow(potentials)) <= spreadProb
                spreadProbs <- spreadProb
              } else {
                spreadProbs <- spreadProb[potentials[,2]]
                #spreadProbs <- spreadProbs[is.na(spreadProbs)]<-0
              }
              
              #If there is only 1 event, R turns the matrix into a vector
              if(is.matrix(potentials)) {
                ItHappened =runif(NROW(potentials))<=spreadProbs
                events <- potentials[ItHappened,2]
              } else {
                ItHappened =runif(1)<=spreadProbs
                events <- potentials[2]
              }
              
              # Implement maxSize
              if(length(maxSize) == 1){
                len <- length(events)
                if((size+len) > maxSize) {
                  keep<-len - ((size+len) - maxSize)
                  samples <- sample(len,keep)
                  events<-events[samples]
                  ItHappened <- vector("logical", length(ItHappened))
                  ItHappened[samples] <- TRUE          
                }
                size <- size + length(unique(events))   
              } else {
                len <- tabulate(spreads[potentials[ItHappened,1]], length(maxSize))
                if(any((size + len) > maxSize & size < maxSize)){
                  whichID <- which(size + len > maxSize)
                  keep<-len[whichID] - ((size[whichID]+len[whichID]) - maxSize[whichID])
                  
                  whichHappened <- which(ItHappened)
                    
                  for(i in 1:length(whichID)){
                    thisID <- spreads[potentials[whichHappened,1]] == whichID[i]
                    ItHappened[whichHappened[thisID]] <- FALSE
                    ItHappened[sample(whichHappened[thisID], keep[i])] <- TRUE            
                  }            
                    
                  events <- potentials[ItHappened,2]        
                }
                size <- pmin(size + len, maxSize) ## Quick and dirty, fast but loose (too flexible)
              }
                     
             # update eligibility map
             
             n <- n+1
             
             if (mapID) {
               if(is.matrix(potentials)) {
                 spreads[events] <- spreads[potentials[ItHappened,1]]
               } else {
                 spreads[events] <- spreads[potentials[1]]
               }
             } else {
               spreads[events] <- n
             }
        
            if(length(maxSize) > 1){
              if(exists("whichID")){
                events <- events[!spreads[events] %in% whichID]
              }
        
            } else {
              if(size >= maxSize) {
                events <- NULL
              }  
            }
             
             # drop or keep loci
             if (is.null(persistence) | is.na(persistence) | persistence == 0) {
               loci <- NULL
             } else {
               if (inRange(persistence)) {
                 loci <- loci[runif(length(loci))<=persistence]
               } else {
                 # here is were we would handle methods for raster* or functions
                 stop("Unsupported type: persistence")
               }
             }
             
             loci <- c(loci, events)      

              if (plot.it){
                plotCur <- raster(landscape)
                plotCur <- setValues(plotCur,spreads)
                Plot(plotCur, ...)
              }
            }

            # Convert the data back to raster
            spre <- raster(landscape)
            spre <- setValues(spre, spreads)
            return(spre)
          }
)
