#' @param  Landscape
#' @param Loci
#' @param Mask  non null, a raster congruent with Landscape whose elements are 0,1 where 1 indicates can not spread to.
#  DOCUMENT ME
# tst<-raster(nrows=10,ncols=10,vals=0)
#' 
SpreadEvents<-
    function(Landscape, Loci=NULL,SpreadProb=0.1,
             Persistance=NULL,Mask=NULL,MaxSize=NULL,directions=8){
   # should sanity check map extents
        
    is.prob<-function(x){
        if (!is.numeric(x)) 
            return(FALSE)
        else 
            return(!(x>1|| x<0))
    }
    
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
      if (is.numeric(SpreadProb))
        ItHappened<-runif(nrow(Potentials))<SpreadProb
      else
        stop("Unsupported type:SpreadProb") #methods for raster* or function args
      Events<-Potentials[ItHappened,2]
      #print(Events)
      #update eligibility map
      Spreads[Events]<-n
      n<-n+1
      #drop or keep Loci
      
      if (is.null(Persistance))
          Loci<-NULL
      else {
          if (is.prob(Persistance)) 
            Loci<-Loci[runif(length(Loci))<Persistance]
          else
            stop("Unsupported type: Persistance") #here is were we would handle methods
                                                  #for raster* or functions
      }
     
      Loci<-c(Loci,Events)
      
    }
    return(Spreads)
}