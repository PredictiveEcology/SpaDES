to_define_step_length<-function(N_agents,mean_log_normal,sd_log_normal,max_step){ ##define N_agents values from a log normal distribution with a maximium threshold
  steps_simulated<-exp(rnorm(n=N_agents,mean=mean_log_normal,sd=sd_log_normal)) ##simulate values from a log normal distribution
  for(a in 1:N_agents){
    while(steps_simulated[a]>max_step){ ##as long as one value is above the threshold given
      steps_simulated[a]<-exp(rnorm(1,mean=mean_log_normal,sd=sd_log_normal)) ##resimulate it
    }
  }

  return(steps_simulated) ##vector of unique value, one for each individual
}


to_identify_patches_on_the_circle<-function(positions,buffers,raster_world,scale_raster){ ##identify the pixels ("patches" in NetLogo) that are at a buffer distance of the individual location
  seq_num_ind<-seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  n.angles<-(ceiling(buffers/scale_raster)+1)*16 ##n = optimum number of points to create the circle for a given individual
  ##number of possible pixel on the perimeter of the square with the point as the center = round scaled radius (i.e., ceiling to avoid having zero if radius is less than cell size), *2 for the diameter, +1 if the point is not on the edge of a pixel, *4 for the 4 sides of the perimeter, *2 points per pixel to be sure =+1*16
  ##gross estimation (checked that it seems to be enough so that pixels extracted are almost always duplicated, which means there is small chances that we missed some on the circle)
  
  ## Eliot's code to replace the createCircle of the package PlotRegionHighlighter
  ids<-rep(seq_num_ind,times=n.angles) ##create individual IDs for the number of points that will be done for their circle
  rads<-rep(buffers,times=n.angles) ##create vector of radius for the number of points that will be done for each individual circle
  xs<-rep(positions[,1],times=n.angles) ##extract the individual current position
  ys<-rep(positions[,2],times=n.angles)
  nvs<-rep(c(0,n.angles[-length(n.angles)]),times=n.angles) ##to be used below to do calculation for angle increments
  angle.inc<-rep(2*pi,length(n.angles))/n.angles ##calculate the angle increment that each individual needs to do to complete a circle (2 pi)
  angs<-rep(angle.inc,times=n.angles) ##repeat this angle increment the number of times it needs to be done to complete the circles
  # Find the angles for each of the n.angles line segments around each agent
  dnvs<-c(0,diff(ids)) ##determine the index that separates two individuals
  nvs[dnvs==0]=0 ##make all values of the nvs = 0 
  nvs2<-cumsum(nvs)
  cum<-1:length(ids)
  index<-cum-nvs2-1 ##this is the series of indices for each angle increment
  angles<-angs*index
  # Calculate the x and y coordinates of the points on the circles
  x<-cos(angles)*rads+xs
  y<-sin(angles)*rads+ys
  
  coordinates_all_ind<-as.matrix(cbind(x,y)) ##put the coordinates of the points on the circles from all individuals in the same matrix
  pixels_under_coordinates<-cellFromXY(raster_world,coordinates_all_ind) ##extract the pixel IDs under the points
  pixels_ind_ids<-cbind.data.frame(ids,pixels_under_coordinates) ##associate the pixel IDs with the individuals
  
  unique_pixels_values<-extract(raster_world,unique(pixels_under_coordinates)) ##extract the raster values under the unique pixel IDs
  pixels_values_ids<-cbind.data.frame(unique(pixels_under_coordinates),unique_pixels_values) ##create a df with the raster values for the unique pixels
  coordinates_unique_pixels_noNA<-xyFromCell(raster_world,pixels_values_ids[!is.na(pixels_values_ids[,2]),1]) ##extract the coordinates for the pixel IDs for which the value != NA (i.e., inside the study area)
  pixels_values_ids$coordinates_unique_pixels_noNA_x<-coordinates_unique_pixels_noNA[,1] ##put the x and y coordinates in the df for the unique pixels
  pixels_values_ids$coordinates_unique_pixels_noNA_y<-coordinates_unique_pixels_noNA[,2]
  
  pixels_ind_ids_merged<-unique(merge(pixels_ind_ids,pixels_values_ids, ##merge the individual ids with the coordinates of the unique pixels
                                      by.x="pixels_under_coordinates",by.y="unique(pixels_under_coordinates)",all=TRUE))
  coord_unique_pixels<-split(pixels_ind_ids_merged[,c(4,5)],pixels_ind_ids_merged[,2]) ##put the coordinates x and y back into a list according to the individual IDs
  
  return(coord_unique_pixels) ##list of df with x and y coordinates of each unique pixel of the circle of each individual
}
