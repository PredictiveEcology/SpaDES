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


to_extract_unique_pixels_on_line<-function(positions,next_possible_locations,distances,raster_world,scale_raster){ ##extract the unique pixel on pathways (i.e., line between the current position and several others)
  seq_num_ind<-seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  num_lines<-as.numeric(unlist(lapply(next_possible_locations,nrow))) ##number of lines for each individual
  points_needed<-as.numeric(ceiling((distances/scale_raster)*3)+1) ##number of points to sample on each line per individual (at leat 3 points per pixel size (scale_raster) + 1 for the origin)
  repeat_time<-num_lines*points_needed ##number of points per individual
  rep_points_by_line<-rep(points_needed,num_lines) ##indices for numbers of points per lines for each individual, repeated the "number of lines" time per individual
  
  ids<-rep(seq_num_ind,repeat_time) ##individual IDs time number of lines times number of points per line
  xs<-rep(positions[,1],repeat_time) ##individual position
  ys<-rep(positions[,2],repeat_time) 
  xe<-rep(as.data.frame(rbindlist(next_possible_locations))[,1],rep_points_by_line) ##ending line point position
  ye<-rep(as.data.frame(rbindlist(next_possible_locations))[,2],rep_points_by_line) 
  heading<-atan2(x=xe-xs,y=ye-ys) ##angle between the individual position and the ending points of their lines 
  dist_line<-sqrt((as.data.frame(rbindlist(next_possible_locations))[,1]-rep(positions[,1],num_lines))^2+
                    (as.data.frame(rbindlist(next_possible_locations))[,2]-rep(positions[,2],num_lines))^2) ##distance between the individual position and the ending points of their lines (because it can be different than the given distances)
  
  increments_by_one<-rep(dist_line/(rep_points_by_line-1),rep_points_by_line) ##calculate the distance between the points for the individual lines (-1 for the intervalles)
  increments_time<-unlist(rep(lapply(points_needed, function(b) 0:(b-1)),num_lines)) ##repeat the "number of point needed" sequence for each line (0:b-1, for the origin until the last point)
  increments<-increments_by_one*increments_time ##multiply them to have the distance of each points from the individual position
  new_x<-cos(heading)*increments+xs ##calculate the coordinates of the points created on the lines
  new_y<-sin(heading)*increments+ys
  
  coordinates_all_pts<-as.matrix(cbind(new_x,new_y)) ##put the coordinates of the points on the lines from all individuals in the same matrix
  pixels_under_coordinates<-cellFromXY(raster_world,coordinates_all_pts) ##extract the pixel IDs under these points
  pixels_per_ind<-split(pixels_under_coordinates,ids) ##split the results by individuals
  lines_ids<-split(rep(unlist(lapply(num_lines, function(c) seq_len(c))),rep_points_by_line),ids) ##give a unique ids for the different lines of each individual
  pixels_per_line_per_ind<-lapply(seq_num_ind, function(d) split(pixels_per_ind[[d]],lines_ids[[d]])) ##split the individual results by lines
  
  pixels<-lapply(seq_num_ind, function(e) lapply(pixels_per_line_per_ind[[e]],unique)) ##for each line of each individual keep only the unique pixel IDs
  
  return(pixels) ##one list of list (number of individuals) of vectors (one per line, pixels IDs composing the line)
}


to_extract_mean_raster_value_and_prob_cross_pathway_roads<-function(pathways,raster_world_mean,raster_world_cross,prob_cross_road){ ##calculate the probability to take the pathway based on the mean value of one raster and road presence on the pathways
  seq_num_ind<-seq_len(length(pathways)) ##create an index sequence for the number of individuals
  num_pathways<-unlist(lapply(pathways,length)) ##retrieve the number of pathways per individual
  length_pathways<-as.vector(unlist(lapply(seq_num_ind, function(f) lapply(pathways[[f]],length)))) ##and the number of pixels per pathways
  ids<-rep(seq_num_ind,num_pathways) ##repeat the individual IDs the number of pathways they have
  ind_ids_per_pixels<-rep(ids,length_pathways) ##repeat the individual IDs for each of their pixels
  ids_path<-unlist(lapply(num_pathways,seq_len)) ##create pathway IDs
  pathway_ids_per_pixels<-rep(ids_path,length_pathways) ##create pathway IDs for each pixel
  
  all_pixels<-as.vector(unlist(pathways)) ##extract all the pixels IDs from all individuals/pathways
  all_pixels_ids<-cbind.data.frame(ind_ids_per_pixels,pathway_ids_per_pixels,all_pixels) ##put the pixels with individual and pathways IDs
  
  all_unique_pixels<-unique(all_pixels) ##keep only the unique pixels
  values_raster_mean<-extract(raster_world_mean,all_unique_pixels) ##extract the values from the raster only for the unique pixels to do the mean value
  values_raster_cross<-extract(raster_world_cross,all_unique_pixels) ##extract the values from the raster only for the unique pixels to do the cross value 
  unique_pixels_and_values<-cbind.data.frame(all_unique_pixels,values_raster_mean,values_raster_cross) ##put the pixels IDs with theirs values
  
  pixels_and_values<-merge(unique_pixels_and_values,all_pixels_ids,
                           by.x="all_unique_pixels",by.y="all_pixels",all=TRUE) ##and merge it with the full list of all the (duplicated) pixels
  pixels_and_values$ind_path_id<-as.factor(paste(pixels_and_values$ind_ids_per_pixels,pixels_and_values$pathway_ids_per_pixels,sep=".")) ##create a unique ID for the individual and pathways

  mean_value<-tapply(pixels_and_values$values_raster_mean,pixels_and_values$ind_path_id,mean) ##take the mean of the raster value per individual pathway
  sum_value<-tapply(pixels_and_values$values_raster_cross,pixels_and_values$ind_path_id,sum) ##take the sum of the raster value per individual pathway
  results_value_df<-cbind.data.frame(mean_value,sum_value) ##turn the mean and sum value into a df
  results_value_df$prob_cross<-prob_cross_road^results_value_df$sum_value
  results_value_df$ind_ids<-rownames(results_value_df) ##and keep the rownames as the unique IDs
  
  full_ids<-cbind.data.frame(ids,ids_path,paste(ids,ids_path,sep=".")) ##create the same IDs as the one in mean_value_df
  colnames(full_ids)[3]<-"ind_ids"
  results_value_df_ids<-merge(results_value_df,full_ids) ##and merge the sum pathway values to the IDs
  results_value_df_ordered<-results_value_df_ids[with(results_value_df_ids,order(ids_path)),] ##order by pathway number
  
  mean_split_ind<-split(results_value_df_ordered$mean_value,results_value_df_ordered$ids) ##and split by individual
  cross_split_ind<-split(results_value_df_ordered$prob_cross,results_value_df_ordered$ids) ##and split by individual
  
  return(list(mean_value_raster=mean_split_ind, ##list of vectors (on for each individual) of the mean raster value for each pathway
              cross_value_raster=cross_split_ind)) ##list of vectors (on for each individual) of the prob_cross^(sum raster value) for each pathway
}


to_scale_prob<-function(value_per_path){ ##scale the probability of each individual pathway based on some value per pathway so that the probabilities sum to 1 for each individual
  seq_num_ind<-seq_len(length(value_per_path)) ##create an index sequence for the number of individuals
  prob_scaled<-list()
  prob_scaled[seq_num_ind]<-lapply(seq_num_ind, ##for each individual
                                   function(g) ifelse(rep(sum(value_per_path[[g]]!=0),length(value_per_path[[g]])), ##check if the mean value_per_path of all the pathways is different from 0
                                                      value_per_path[[g]]/sum(value_per_path[[g]]), ##if yes, the pathway probability scaled is = pathway value / sum of all pathway values
                                                      rep(1,length(value_per_path[[g]]))/length(value_per_path[[g]]))) ##otherwise give an equal probability for each pathway
  
  return(prob_scaled) ##list of vector with the scaled probabilities (sum to 1) for each available pathway of each individual
}