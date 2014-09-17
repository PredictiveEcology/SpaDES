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
#' @rdname spread-method
#'
setGeneric("spread", function(landscape, loci, spreadProb, persistance,
                              mask, maxSize, directions, iterations, ...) {
  standardGeneric("spread")
})

#' @param plot.it    If TRUE, then plot the raster at every iteraction, so one can watch the
#' spread event grow.
#'
#' @param mapID  Logical. If TRUE, then the returned fire map is a map of fire ids. If FALSE,
#' the returned map is the iteration number that the pixel burned
#'
#' @import raster RColorBrewer
#' @rdname spread-method
#' @name spread
#'
#' @examples
#' library(raster)
#' library(RColorBrewer)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e2,0,1e2),res=1)
#' hab <- GaussMap(a,speedup=3)
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
#' setColors(hab) <- paste(c("#000000",brewer.pal(8,"Greys")),c("00",rep("FF",8)),sep="")
#'
#' #dev(4)
#' Plot(hab,speedup=3) # note speedup is equivalent to making pyramids, so, some details are lost
#'
#' # initiate 10 fires at to loci
#' fires <- spread(hab, loci=as.integer(sample(1:ncell(hab), 10)),
#'                 0.235, 0, NULL, 1e8, 8, 1e6, mapID=TRUE)
#' #set colors, adding a transparency factor... i.e., the last 2 characters of an 8 character
#' #  hex code are transparency, from 00 (fully transparent) to FF (fully opaque)
#' #  Here, we are using only the darkest end of the Red palette (i.e., of 8 reds, use the 5:8 ones)
#' setColors(fires)<-paste(c("#000000",brewer.pal(8,"Reds")[5:8]),c("00",rep("FF",4)),sep="")
#' Plot(fires,addTo="hab",speedup=3)
setMethod("spread",
          signature(landscape="RasterLayer"#, loci="integer",
                    #spreadProb="numeric"
                    #persistance="numeric",
                    #mask="RasterLayer", maxSize="numeric",
                    #directions="integer", iterations="integer"
                    ),
          definition = function(landscape, loci, spreadProb, persistance,
                                mask, maxSize=ncell(landscape), directions=8,
                                iterations=ncell(landscape), mapID=FALSE,
                                plot.it=FALSE, ...) {
            ### should sanity check map extents

            if (is.null(loci))  {
              # start it in the centre cell
              loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
            }

            spreads <- rep_len(0, ncell(landscape))#data.table(ind=1:ncell(landscape), burned=0, key="ind")
            if(!is.null(mask)) {
              masked <- Which(mask==0, cells=TRUE)#getValues(mask)==0
              #  spreads[masked]<- NaN#[potentials %in% masked]]

            }
            n <- 1
            if (mapID) {
              spreads[loci] <- 1:length(loci)
            } else {
              spreads[loci] <- n
            }
            size <- length(loci)

            if (is.null(iterations)) {
              iterations <- Inf # this is a stupid way to do this!
            }

            while ( (length(loci)>0) && (iterations>=n) ) {
              if (mapID) {
                potentials <- matrix(adj(landscape, loci, directions, pairs=TRUE),ncol=2)
              } else {
                # must pad the first column of potentials
                potentials <- matrix(cbind(NA, adj(landscape, loci, directions, pairs=FALSE)),ncol=2)
              }

              #if there is only one potential, R converts this to a vector, instead of a matrix.
              # Force it back to a matrix
              if(length(potentials)==2) {
                potentials <- matrix(potentials,ncol=2)
              }


              # drop those ineligible
              if (!is.null(mask))
                potentials <- matrix(potentials[potentials[,2] %in% masked,], ncol=2)

              # Should this be unique?
              # only accept cells that have no fire yet
#               if (mergeDuplicates)
#                 potentials <- potentials[!duplicated(potentials[spreads[potentials[,2]]==0,2]),]
#
#                 #potentials <- unique(potentials[spreads[potentials[,2]]==0,2])
#               else
              potentials <- matrix(potentials[spreads[potentials[,2]]==0,], ncol=2)
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
                  spreadProbs <- spreadProb[potentials[,2]]
                  spreadProbs[is.na(spreadProbs)]<-0
              }

              #If there is only 1 event, R turns the matrix into a vector
              if(is(potentials,"matrix")) {
                ItHappened =runif(nrow(potentials))<=spreadProbs
                events <- potentials[ItHappened,2]
              } else {
                ItHappened =runif(1)<=spreadProbs
                events <- potentials[2]
              }

              # Implement maxSize
              len <- length(events)
              if((size+len) > maxSize) {
                keep<-len - ((size+len) - maxSize)
                events<-events[sample(len,keep)]
              }

              size <- size + length(unique(events))

              # update eligibility map

              n <- n+1

              if (mapID) {
                if(is(potentials,"matrix")) {
                  spreads[events] <- spreads[potentials[ItHappened,1]]
                } else {
                  spreads[events] <- spreads[potentials[1]]
                }
              } else {
                spreads[events] <- n
              }


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
                top <- raster(landscape)
                top <- setValues(top,spreads)
                Plot(top)
              }
            }

            # Convert the data.table back to raster
            spre <- raster(landscape)
            spre <- setValues(spre, spreads)
            return(spre)
          }
)
