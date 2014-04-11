#' @param Raster* Landscape
#' @param Mask  non null, a raster congruent with Landscape whose elements are 0,1 where 1 indicates can not spread to.
#  DOCUMENT ME
# tst<-raster(nrows=10,ncols=10,vals=0)
#' 
SpreadEvents<-
    function(Landscape, Loci=NULL, Mask=NULL,
             directions=8, SpreadProb=0.1,MaxSize=NULL){
   # should sanity check map extents
    if (is.null(Loci))
        Loci <- (Landscape@nrows/2 + 0.5) * Landscape@ncols    

    Spreads<- setValues(raster(Landscape),0)
    n<-1
    Spreads[Loci]<-n
   
    while (length(Loci)>0){
      #print(paste(n,length(Loci)))
      Potentials<-adjacent(Landscape,Loci,directions)
      #drop those inelgible
      if (!is.null(Mask)){
        tmp<-extract(Mask,Potentials[,2])
        Potentials<-Potentials[ifelse(is.na(tmp),FALSE,tmp == 0,)]
      }
      tmp<-extract(Spreads,Potentials[,2])
      #print(cbind(Potentials,tmp))
      Potentials<-Potentials[ifelse(is.na(tmp),FALSE,tmp == 0),]
     
      #select which potentials actually happened
      #nrow() only works if Potentials is an array
      ItHappened<-runif(nrow(Potentials))<SpreadProb
      Events<-Potentials[ItHappened,2]
      #print(Events)
      #update eligibility map
      Spreads[Events]<-n
      n<-n+1
      #drop or keep Loci
      Loci<-Loci[runif(length(Loci)<0.05)]
      Loci<-c(Loci,Events)
      
    }
    return(Spreads)
}