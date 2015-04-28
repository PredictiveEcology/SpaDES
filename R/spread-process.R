##############################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulated fires or other things. Essentially, it starts from a collection of cells
#' (\code{loci}) and spreads to neighbours, according to the \code{directions} and \code{spreadProbPixel} arguments.
#' This can become quite general, if \code{spreadProbPixel} is 1 as it will expand from every loci until all pixels
#' in the landscape have been covered. With \code{mapID} set to \code{TRUE}, the resulting map will be
#' classified by the index of the pixel where that event propagated from. This can be used to examine things like
#' fire size distributions.
#'
#' @param landscape     A \code{RasterLayer} object.
#'
#' @param loci          A vector of locations in \code{landscape}
#'
#' @param spreadProbPixel    Numeric or rasterLayer. The overall probability of spreading, or probability raster
#' driven.
#'
#' @param persistence   A probability that a burning cell will continue to burn, per time step.
#'
#' @param mask          non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
#'                      whose elements are \code{0,1}, where 1 indicates "cannot spread to". Currently
#'                      not implemented.
#'
#' @param maxSize       Vector of the maximum number of pixels for a single or all events to be spread.
#'                      Recycled to match \code{loci} length.
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
#' @rdname spread
#'
setGeneric("spread", function(landscape, loci=ncell(landscape)/2L, spreadProbPixel=0.23,
                              persistence=0L, mask=NULL, maxSize=rep_len(ncell(landscape), length(loci)),
                              directions=8L, iterations=NULL, ...) {
  standardGeneric("spread")
})

#' @param plot.it    If TRUE, then plot the raster at every iteraction, so one can watch the
#' spread event grow.
#'
#' @param mapID  Logical. If TRUE, returns a raster of events ids. If FALSE,
#' returns a raster of iteration numbers, i.e. the spread history of one or more events.
#'
#' @importFrom methods is
#' @import raster
#' @import RColorBrewer
#' @rdname spread
#'
#' @examples
#' library(raster)
#' library(RColorBrewer)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e2,0,1e2),res=1)
#' hab <- gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
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
#' setColors(fires, 10)<-c("#00000000", brewer.pal(8,"Reds")[5:8])
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
#' Plot(fires, addTo="hab2$hab", zero.color="white",
#'      cols=colorRampPalette(c("orange","darkred"))(10))
#' # or overplot the original (NOTE: legend stays at original values)
#' Plot(fires,
#'      cols=topo.colors(10))
#'
setMethod("spread",
          signature(landscape="RasterLayer"),
          definition = function(landscape, loci, spreadProbPixel, persistence,
                                mask, maxSize=rep_len(ncell(landscape), length(loci)),
                                directions=8L, iterations = NULL, mapID=FALSE,
                                plot.it=FALSE, spreadProbCluster, dist, ...) {
            ### should sanity check map extents
            if (is.null(loci))  {
              # start it in the centre cell
              loci <- (nrow(landscape)/2L + 0.5) * ncol(landscape)
            }

            if(is(spreadProbPixel,"RasterLayer")) {
              if (minValue(spreadProbPixel)>1L) stop("spreadProbPixel is not a probability")
              if (maxValue(spreadProbPixel)<0L) stop("spreadProbPixel is not a probability")
            } else {
              if (!inRange(spreadProbPixel)) stop("spreadProbPixel is not a probability")
            }

            ## Recycling maxSize as needed
            maxSize <- rep_len(maxSize, length(loci))

            spreads <- vector("integer", ncell(landscape))

            n <- 1L
            if (mapID) {
              spreads[loci] <- 1L:length(loci)
              if(length(maxSize) > 1L){
                size <- rep_len(1L, length(loci))
              } else {
                size <- length(loci)
              }
            } else {
              spreads[loci] <- n
              size <- length(loci)
            }

            # Convert mask and NAs to 0 on the spreadProbPixel Raster
            if (is(spreadProbPixel, "Raster")) {
              spreadProbPixel[is.na(spreadProbPixel)]<-0L
              if(!is.null(mask)) {
                spreadProbPixel[mask==1L]<-0L
              }
            } else if (is.numeric(spreadProbPixel)) { # Translate numeric spreadProbPixel into a Raster
              #  if there is a mask Raster
              if(!is.null(mask)) {
                spreadProbPixel <- raster(extent(landscape), res=res(landscape), vals=spreadProbPixel)
                spreadProbPixel[mask==1L]<-0L
              }
            }

            while (length(loci)) { # while there are active cells

              # identify neighbours
              if (mapID) {
                potentials <- adj(landscape, loci, directions, pairs=TRUE)
              } else {
                # must pad the first column of potentials
                potentials <- cbind(NA, adj(landscape, loci, directions,
                                            pairs=FALSE))
              }

              # keep only neighbours that have not been spread to yet
              potentials <- potentials[spreads[potentials[,2L]]==0L,,drop=FALSE]


              if (is.numeric(spreadProbPixel)) {
                spreadProbs <- spreadProbPixel
              } else {
                spreadProbs <- spreadProbPixel[potentials[,2L]]
              }

              potentials <- potentials[runif(NROW(potentials)) <= spreadProbs,,drop=FALSE]
              potentials <- potentials[sample.int(NROW(potentials)),,drop=FALSE]
              potentials <- potentials[!duplicated(potentials[,2L]),,drop=FALSE]
              events <- potentials[,2L]

              # Implement maxSize
              if(length(maxSize) == 1L){
                len <- length(events)
                if((size+len) > maxSize) {
                  keep <- len - ((size+len) - maxSize)
                  samples <- sample(len,keep)
                  events <- events[samples]
                  potentials <- potentials[samples,,drop=FALSE]
                }
                size <- size + length(events)
              } else {
                len <- tabulate(spreads[potentials[,1L]], length(maxSize))
                if(any((size + len) > maxSize & size < maxSize)){
                  whichID <- which(size + len > maxSize)
                  toRm <- (size + len)[whichID] - maxSize[whichID]

                  for(i in 1:length(whichID)){
                    thisID <- which(spreads[potentials[,1L]] == whichID[i])
                    potentials <- potentials[-sample(thisID, toRm[i]),,drop = FALSE]
                  }
                  events <- potentials[,2L]
                }
                size <- pmin(size + len, maxSize) ## Quick? and dirty, fast but loose (too flexible)
              }

              size <- size + length(unique(events))

              # update eligibility map

              n <- n+1L

              if (mapID) {
                spreads[events] <- spreads[potentials[,1L]]
              } else {
                spreads[events] <- n
              }

              if(length(maxSize) > 1L){
                if(exists("whichID")){
                  events <- events[!spreads[events] %in% whichID]
                  rm(whichID)
                }

              } else {
                if(size >= maxSize) {
                  events <- NULL
                }
              }

              # drop or keep loci
              if (is.null(persistence) | is.na(persistence) | persistence == 0L) {
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




setGeneric("landisWardSpread", function(seedSrc, seedRcv=ncell(landscape)/2L,
                              directions=8L, ...) {
  standardGeneric("landisWardSpread")
})



setMethod("landisWardSpread",
          signature(seedSrc="RasterLayer"),
          definition = function(seedSrc, seedRcv, directions=8L,
                                plot.it=FALSE, ...) {
            ### should sanity check map extents
            if (is.null(seedRcv))  {
              # start it in the centre cell
              seedRcv <- (nrow(seedSrc)/2L + 0.5) * ncol(seedSrc)
            }
#            nInitial <- length(seedRcv)
            lociReturn <- data.table(fromInit=seedRcv,key="fromInit")
            seedsArrived <- data.table(fromInit=numeric(),key="fromInit")

#             if(is(spreadProbPixel,"RasterLayer")) {
#               if (minValue(spreadProbPixel)>1L) stop("spreadProbPixel is not a probability")
#               if (maxValue(spreadProbPixel)<0L) stop("spreadProbPixel is not a probability")
#             } else {
#               if (!inRange(spreadProbPixel)) stop("spreadProbPixel is not a probability")
#             }
            if(plot.it) {
              hab1 <- raster(getGlobal("hab"))
              assignGlobal("hab1",hab)
              Plot(seedSrc, new=TRUE)
            }

            n <- 1L

            # Convert mask and NAs to 0 on the spreadProbPixel Raster
#             if (is(spreadProbPixel, "Raster")) {
#               spreadProbPixel[is.na(spreadProbPixel)]<-0L
#               if(!is.null(mask)) {
#                 spreadProbPixel[mask==1L]<-0L
#               }
#             } else if (is.numeric(spreadProbPixel)) { # Translate numeric spreadProbPixel into a Raster
#               #  if there is a mask Raster
#               if(!is.null(mask)) {
#                 spreadProbPixel <- raster(extent(seedSrc), res=res(seedSrc), vals=spreadProbPixel)
#                 spreadProbPixel[mask==1L]<-0L
#               }
#             }

            potentialsInit <- data.table("fromInit"=seedRcv,key="fromInit")
            potentialsInit[,from:=fromInit]
            setkey(potentialsInit,"from", "fromInit")
            potentials <- copy(potentialsInit)


            while (length(seedRcv) & (n<=maxDist)) { # while there are active cells

              # identify neighbours
              adjCells <- adj(seedSrc, seedRcv, directions, pairs=TRUE) %>%
                    data.table(key="from")
              if(n>1) {
                potentials[,`:=`(from=NULL,dis=NULL)][,from:=to][,to:=NULL]
                setkey(potentials,"from", "fromInit")
                potentials <- potentials[adjCells, allow.cartesian=TRUE]

              } else {
                potentials <- potentials[adjCells, allow.cartesian=TRUE]
              }

              if(plot.it) {
                hab1[potentials[,from]] <- n
                assignGlobal("hab1",hab1)
                Plot(hab1, addTo="seedSrc")
              }


              # keep only neighbours that have not been landisWardSpread to yet, within each
              # cluster. This means that a cell can't spread backwards, but two different
              # clusters can be on the same cell
              potentials <- potentials[from!=to,.SD,by="fromInit"]
              # Don't know how to do next within data.table syntax - remove duplicate "to"
              #  within a cluster
              potentials  <- potentials %>%
                group_by(fromInit) %>%
                filter(!duplicated(to))

              nr <- NROW(potentials)
              xys <- xyFromCell(hab, as.matrix(potentials[,list(fromInit,to)]))
              potentials[,dis:=pointDistance(xys[1:nr,], xys[(nr+1):(2*nr),], lonlat=FALSE)]

              # discard those that more than "n" units from a "from" cell. This keeps spreading
              #   in a circle. It is somewhat wasteful, because the distances are calculated above
              #   and then deleted on the next line, but this may be the most efficient way
              potentials <- potentials[((n-1) < dis) & (dis <= n),]
              potentialsWithSeed <- as.logical(seedSrc[potentials[,to]])
              if(any(potentialsWithSeed)) {

                cellFn <- expression(if(cellSize<=effDist) {
                  ifelse(dis<effDist,
                                   exp((dis-cellSize)*log(1-k)/effDist)-
                                         exp(dis*log(1-k)/effDist),
                                   (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
                                          (1-k)*exp((dis-effDist)*log(b)/maxDist))
                } else {
                  ifelse(dis==cellSize,
                         exp((dis-cellSize)*log(1-k)/effDist)-(1-k)*
                                         exp((dis-effDist)*log(b)/maxDist),
                         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
                                     (1-k)*exp((dis-effDist)*log(b)/maxDist))
                })

                potentialsWithSeedDT  <- potentials[potentialsWithSeed,]
                nr <- NROW(potentialsWithSeedDT)
                setkey(potentialsWithSeedDT, "fromInit")
                pot <- copy(potentialsWithSeedDT)

                potentialsWithSeedDT[,receivesSeeds:=runif(nr)<eval(cellFn)]
                receivedSeeds <- potentialsWithSeedDT[,any(receivesSeeds), by="fromInit"]

                #drop any that received seeds from potentials, as they are now in lociReturn
                if(NROW(receivedSeeds[V1==TRUE])>0) {
                  seedsArrived <- rbindlist(list(seedsArrived,lociReturn[receivedSeeds[V1==TRUE]][,V1:=NULL]))
                  setkey(seedsArrived, "fromInit")
                  setkey(potentials, "fromInit")
                  potentials <- potentials[!seedsArrived]
                }
              }

              n <- n+1L
              # refresh so that "to" cells become new "from cells
              seedRcv <- potentials[,to]

            }
            return(as.matrix(seedsArrived))

          }
)
