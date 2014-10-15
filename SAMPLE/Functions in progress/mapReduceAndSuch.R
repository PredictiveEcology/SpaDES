
library(dplyr)

# get_next_integer = function(){
#   j = 0
#   function(u,v,w){ j <<- j+1 }
# }

# will be used to convert shortened


library(microbenchmark)
(mb=microbenchmark(times=1L,dplyrVersion <- {
  vegVal <- tbl_df(data.frame(traj = getValues(trajMap),
                              age=getValues(ageMap)+1,
                              rand=sample(1:3, ncell(trajMap), replace=T),
                              index=1:ncell(trajMap)))
#   get_integer = get_next_integer()
#   (a = vegVal %>%
#     group_by(traj,age,rand) %>%
#     mutate(label=get_integer(traj, age, rand)))

  (a = vegVal %>%
     group_by(traj,age,rand) %>%
     mutate(label=min(index)))

  d = a %>%
    summarise(label=unique(label)) %>%
    ungroup() %>%
    mutate(veg=trajObj[cbind(age, traj)]) %>%
    select(label, veg, age, traj, rand)

  for(i in 1:10) {
    d[,"age"] <- d[,"age"]+1
    d[,"veg"]=trajObj[cbind(d[,"age"], d[,"traj"])]
  }

  #vegVal2 = left_join(d,a, by="label" )
  vegVal2 = inner_join(a,d, by="label" )
  vegMap0 <- RasterLayerNamed(setValues(vegMap,vegVal2$veg),name="vegMap")
},

vectorVersion<-{
  ageMap.v <- round(getValues(ageMap))+0
  trajMap.v <- getValues(trajMap)
  vegMap.v <- trajObj[cbind(ageMap.v,trajMap.v)]

  for(i in 1:10) {
    ageMap.v <- ageMap.v + 0
    vegMap.v <- trajObj[cbind(ageMap.v, trajMap.v)]
  }

#    vegMap <- raster(ageMap)
  vegMap1 <- RasterLayerNamed(setValues(vegMap,vegMap.v),name="vegMap")
}
)
)

all.equal(getValues(vegMap0), getValues(vegMap1))

indexRaster<-raster(trajMap)
indexRaster<-setValues(indexRaster,1:ncell(trajMap))
ind = sampleRegular(indexRaster, 3e4,asRaster=TRUE, useGDAL=TRUE)

vegVal2 = inner_join(a[getValues(ind),],d, by="label" )
smallVeg=raster(ind)
vegMap3 <- RasterLayerNamed(setValues(smallVeg,vegVal2$veg),name="vegMap")
dev(4);plot(vegMap3)

