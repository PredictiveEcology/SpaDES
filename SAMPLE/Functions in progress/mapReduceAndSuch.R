
library(dplyr)

get_next_integer = function(){
  i = 0
  function(u,v){ i <<- i+1 }
}


library(microbenchmark)
(mb=microbenchmark(times=1L,{
  vegVal <- tbl_df(data.frame(traj = getValues(trajMap),
                              age=getValues(ageMap)+1))
  get_integer = get_next_integer()
  (a = vegVal %>%
    group_by(traj,age) %>%
    mutate(label=get_integer(traj, age)))

  d = a %>%
    summarise(label=unique(label)) %>%
    ungroup() %>%
    mutate(veg=trajObj[cbind(age, traj)]) %>%
    select(label, veg)

  #vegVal2 = left_join(d,a, by="label" )
  vegVal2 = inner_join(a,d, by="label" )
  vegMap0 <- RasterLayerNamed(setValues(vegMap,vegVal2$veg),name="vegMap")
},

{
  ageMap.v <- round(getValues(ageMap))+1
  trajMap.v <- getValues(trajMap)
  vegMap.v <- trajObj[cbind(ageMap.v,trajMap.v)]
#    vegMap <- raster(ageMap)
  vegMap1 <- RasterLayerNamed(setValues(vegMap,vegMap.v),name="vegMap")
}
)
)


setColors(vegMap, n=12 ) <- vegMapColors

assign("vegMap", vegMap, envir=.GlobalEnv)
