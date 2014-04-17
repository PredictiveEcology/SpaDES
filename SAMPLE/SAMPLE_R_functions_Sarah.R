to_define_step_length<-function(N_agents,mean_log_normal,sd_log_normal,max_step){ ##define N_agents values from a log normal distribution with a maximum threshold
  steps_simulated<-exp(rnorm(n=N_agents,mean=mean_log_normal,sd=sd_log_normal)) ##simulate values from a log normal distribution
  for(a in 1:N_agents){ 
    while(steps_simulated[a]>max_step){ ##as long as one value is above the threshold given
      steps_simulated[a]<-exp(rnorm(1,mean=mean_log_normal,sd=sd_log_normal)) ##resimulate it
    }
  }
  
  return(steps_simulated) ##vector of unique values (distance in meters) for each individual
}


to_identify_patches_on_the_circle<-function(positions,buffers,raster_world,scale_raster,speedup=1){ ##identify the pixels ("patches" in NetLogo) that are at a buffer distance of the individual location
  seq_num_ind<-seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  n.angles<-pmin(200,ceiling({{buffers/scale_raster}*16}/speedup)) ##number of points to create the circle for a given individual
  # *16, two points in each of the 8 neighboring cells at least, but no more than 200 pathways per individual
  
  ## Eliot's code to replace the createCircle of the package PlotRegionHighlighter
  ids<-rep.int(seq_num_ind,times=n.angles) ##create individual IDs for the number of points that will be done for their circle
  rads<-rep.int(buffers,times=n.angles) ##create vector of radius for the number of points that will be done for each individual circle
  xs<-rep.int(positions[,1],times=n.angles) ##extract the individual current position
  ys<-rep.int(positions[,2],times=n.angles)
  nvs<-rep.int(c(0,n.angles[-length(n.angles)]),times=n.angles) ##to be used below to do calculation for angle increments
  angle.inc<-rep.int(2*pi,length(n.angles))/n.angles ##calculate the angle increment that each individual needs to do to complete a circle (2 pi)
  angs<-rep.int(angle.inc,times=n.angles) ##repeat this angle increment the number of times it needs to be done to complete the circles
  # Find the angles for each of the n.angles line segments around each agent
  increments<-data.table(ids,angs)
  increments[,angles:=cumsum(angs),by=ids] ##find the angle for each point of the circles
  # Calculate the x and y coordinates of the points on the circles
  x<-cos(increments[,angles])*rads+xs
  y<-sin(increments[,angles])*rads+ys
  
  coordinates_all_ind<-cbind(x,y) ##put the coordinates of the points on the circles from all individuals in the same matrix
  pixels_under_coordinates<-cellFromXY(raster_world,coordinates_all_ind) ##extract the pixel IDs under the points
  pixels_ind_ids<-unique(data.table(ids,pixels_under_coordinates)) ##associate the pixel IDs with the individuals and keep unique entries (remove duplicated pixels for each caribou)
  
  unique_pixels<-unique(pixels_under_coordinates) ##keep the unique pixels IDs
  unique_pixels_values<-extract(raster_world,unique_pixels) ##extract the raster values under the unique pixel IDs
  pixels_values_idsNA<-cbind(unique_pixels,unique_pixels_values) ##bind the raster values with the unique pixel IDs
  pixels_unique_ids_noNA<-pixels_values_idsNA[!is.na(pixels_values_idsNA[,2]),1] ##keep the pixels for which the value != NA (i.e., inside the study area)
  coordinates_unique_pixels_noNA<-xyFromCell(raster_world,pixels_unique_ids_noNA) ##extract the coordinates for the pixel IDs 
  pixels_values_ids2<-data.table(pixels_unique_ids_noNA,coordinates_unique_pixels_noNA) ##put the x and y coordinates for the unique pixels
  
  setkey(pixels_ind_ids,"pixels_under_coordinates") ##set the common key to merge the 2 data.tables
  setkey(pixels_values_ids2,"pixels_unique_ids_noNA")
  pixels_ind_ids_merged<-pixels_ind_ids[J(pixels_values_ids2)] ##merge the coordinates for the pixels with values to the individual ids
  coord_circle_ind<-subset(pixels_ind_ids_merged,select=c(ids,x,y)) ##keep only the individual IDs with the coordinates of the unique pixels on their circle
  setkey(coord_circle_ind,"ids") ##sort the data.table by ids
  
  lack<-seq_num_ind[!seq_num_ind %in% unique(coord_circle_ind[,ids])] ##check if one individual doesn't have any direction
  if(length(lack!=0)){
    coord_circle_ind<-rbindlist(list(coord_circle_ind,cbind.data.frame(ids=lack,x=positions[lack,1],y=positions[lack,2]))) ##put its origninal location
    setkey(coord_circle_ind,"ids") ##re-sort the data.table by ids
  }
  
  return(coord_circle_ind) ##data.table with x and y coordinates of each unique pixel of the circle of each individual
}

to_extract_unique_pixels_on_line<-function(positions,next_possible_locations,distances,raster_world,scale_raster,speedup=1){ ##extract the pixel on pathways (i.e., line between the current position and several others)
  seq_num_ind<-seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  points_needed<-ceiling({distances/scale_raster}/speedup) ##number of points needed to sample on each line per individual (=number of pixels at the scale_raster)
  init_pos<-data.table(positions,points_needed,seq_num_ind,key="seq_num_ind") ##key=individual IDs
  
  num_lines<-next_possible_locations[,length(x), by=ids][,V1] ##number of lines for each individual (number of ending points x coordinates)
  next_possible_locations[,ids_path:=unlist(lapply(num_lines, seq_len))] ##ending line point position with path IDs
  
  pos_next<-next_possible_locations[J(init_pos)] ##join the initial positions with their ending points
  
  pos_next[,heading:=atan2(x=x-V1,y=y-V2)] ##angle between the individual position and the ending points of their lines 
  pos_next[,dist:=sqrt({x-V1}*{x-V1}+{y-V2}*{y-V2})] ##distance between the individual position and the ending points of their lines (because it can be different than the given distances)
  pos_next[,increments_by_one:=dist/points_needed] ##distance between each point that will be created on the lines
  
  ## Duplicate the lines as many times as the number of points needed for individual each line
  pos_next[,multiple:=as.numeric(points_needed)]
  setkey(pos_next,"multiple")
  pos_next_duplicated<-pos_next[J(rep(unique(multiple),unique(multiple))),allow.cartesian=TRUE]
  setkeyv(pos_next_duplicated,c("ids","ids_path")) ##order the data by individual and path IDs
  
  incr_time<-rep.int(lapply(points_needed, function(b) seq_len(b)),as.numeric(num_lines))
  pos_next_duplicated[,increments_time:=unlist(incr_time)] ##position of the new points created on the line to know their distances from the initial position
  pos_next_duplicated[,increments:=increments_by_one*increments_time] ##multiply with the distances between the points created to have the distance of each points from the individual position
  
  pos_next_duplicated[,new_x:=cos(heading)*increments+V1] ##calculate the coordinates of the points created on the lines
  pos_next_duplicated[,new_y:=sin(heading)*increments+V2]
  
  pos_next_duplicated[,pixels_path:=cellFromXY(raster_world,matrix(c(new_x,new_y),ncol=2))] ##extract the pixel IDs under the coordinates of the points on the individual lines  
  pixels_per_line_per_ind<-subset(pos_next_duplicated,select=c(ids,ids_path,pixels_path)) ##keep only the pixel IDs with the individual and path IDs
  
  return(pixels_per_line_per_ind) ##data.table of pixels on the individual pathways with individual and path IDs
}


to_extract_mean_raster_value_and_prob_cross_pathway_roads<-function(pathways,stack_raster_mean_cross,prob_cross_road){ ##calculate the probability to take the pathway based on the mean value of one raster and road presence on the pathways
  all_unique_pixels<-unique(pathways[,pixels_path]) ##retrieve the pixels composing the pathways and keep only the unique IDs
  values_raster_mean_cross<-extract(stack_raster_mean_cross,all_unique_pixels,layer=1,nl=2) ##extract the values from the raster only for the unique pixels to do the mean value and the cross value
  unique_pixels_and_values<-data.table(all_unique_pixels,values_raster_mean_cross,key="all_unique_pixels") ##put the pixels IDs with theirs values
  
  setkey(pathways,"pixels_path") ##set the key to the pathways table as the pixels IDs to merge with the raster values
  pixels_and_values<-pathways[J(unique_pixels_and_values)] ##join the raster values with  all the pathway pixels
    
  pixels_and_values[,mean_layer1:=mean(layer.1),by="ids,ids_path"] ##calculate the mean raster value (layer1) per individual per pathway
  pixels_and_values[,sum_layer2:=sum(layer.2),by="ids,ids_path"] ##calculate the sum raster value (layer2) per individual per pathway
  val_per_path<-unique(subset(pixels_and_values,select=c(ids,ids_path,mean_layer1,sum_layer2))) ##select only the needed values
  setkeyv(val_per_path,c("ids","ids_path")) ##order the data by individual and path IDs
  prob_cross_r<-data.table(ids=seq_len(length(prob_cross_road)),prob_cross_road,key="ids") ##create a data.table with the crossing probabilities for each individual
  val_per_path_r<-val_per_path[J(prob_cross_r)] ##and join it to the table with the raster probabilities
  val_per_path_r[,prob_cross:=prob_cross_road^sum_layer2] ##calculate the crossing probabilities
  
  mean_split_ind<-split(val_per_path_r$mean_layer1,val_per_path_r$ids) ##split the mean raster values per individuals
  cross_split_ind<-split(val_per_path_r$prob_cross,val_per_path_r$ids) ##split the road crossing proabilities per individuals
  
  return(list(mean_value_raster=mean_split_ind, ##list of vectors (on for each individual) of the mean raster value for each pathway
              cross_value_raster=cross_split_ind)) ##list of vectors (on for each individual) of the prob_cross^(sum raster value) for each pathway
}

to_scale_prob<-function(value_per_path){ ##scale the probability of each individual pathway based on some value per pathway so that the probabilities sum to 1 for each individual
  seq_num_ind<-seq_len(length(value_per_path)) ##create an index sequence for the number of individuals
  prob_scaled<-list()
  prob_scaled[seq_num_ind]<-lapply(seq_num_ind, ##for each individual
                                   function(g) ifelse(rep.int(sum(value_per_path[[g]]!=0),length(value_per_path[[g]])), ##check if the mean value_per_path of all the pathways is different from 0
                                                      value_per_path[[g]]/sum(value_per_path[[g]]), ##if yes, the pathway probability scaled is = pathway value / sum of all pathway values
                                                      rep.int(1,length(value_per_path[[g]]))/length(value_per_path[[g]]))) ##otherwise give an equal probability for each pathway
  
  return(prob_scaled) ##list of vector with the scaled probabilities (sum to 1) for each available pathway of each individual
}


to_rotate_between_two_directions<-function(origin,first_direction,several_second_direction,mean_wrapped_normal,sd_wrapped_normal){ ##calculate the direction probabilities from a wrapped normal distribution based on the angle between the direction from the origin to the first direction and the direction to several other second directions 
  num_second_direction<-unlist(lapply(several_second_direction,nrow)) ##retrieve the number of several_second_direction per individual
  x_ori<-rep.int(origin[,1],num_second_direction) ##repeat the origin and first points the number of corresponding second points
  y_ori<-rep.int(origin[,2],num_second_direction)
  x_first<-rep.int(first_direction[,1],num_second_direction)
  y_first<-rep.int(first_direction[,2],num_second_direction)
  ## Calculate the angles between the 3 points (origin, first and second direction) with the following formulae
  ## theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  ## where a and b are vectors with origin c(0,0) and are calculated as c(end(x)-origin(x),end(y)-origin(y))
  ## Values are always between 0 and 180 degrees, does not matter the direction to the right or to the left (+ or - angles)
  x_first_trans<-x_first-x_ori ##susbtract the origin point
  y_first_trans<-y_first-y_ori
  second_direction<-as.matrix(rbindlist(several_second_direction)) ##rbind the several_second_direction from all the individuals
  x_second_trans<-second_direction[,1]-x_ori
  y_second_trans<-second_direction[,2]-y_ori
  sum_a_times_b<-x_first_trans*x_second_trans+y_first_trans*y_second_trans ##decompose the formulae to apply it on vectors
  sum_a_times_a<-x_first_trans*x_first_trans+y_first_trans*y_first_trans
  sum_b_times_b<-x_second_trans*x_second_trans+y_second_trans*y_second_trans
  angles<-deg(acos(sum_a_times_b/(sqrt(sum_a_times_a)*sqrt(sum_b_times_b)))) ##calculate the angles between each 3 points (between vector origin-first_dir and vector origin-second_dir)
  angles_prob<-dnorm(angles,mean=mean_wrapped_normal,sd=sd_wrapped_normal) ##calculate the probability from a wrapped normal distribution (angles are alwyas between 0 and 180)
  angles_prob[is.na(angles_prob)]<-1 ##if an angle could not be calculated (because origin=first_dir), put 1 
  
  seq_num_ind<-seq_len(nrow(origin)) ##create an index sequence for the number of individuals
  angles_prob_split<-split(angles_prob,rep.int(seq_num_ind,num_second_direction)) ##split the results by individuals
  return(angles_prob_split) ##list of vectors (one for each individual) with values for each pathway proabilities
}


to_equal_prob_path<-function(end_pts_pathways){ ##create equal probabilities for the pathways for each individual
  seq_num_ind<-seq_len(length(end_pts_pathways)) ##create an index sequence for the number of individuals
  equal_prob<-list()
  equal_prob[seq_num_ind]<-lapply(seq_num_ind, ##for each individual
                                  function(i) rep.int(0,nrow(end_pts_pathways[[i]]))) ##create a vector of 0 of length of the number of pathways available (=number of end_pts_pathways)
  
  return(equal_prob) ##list of vectors of 1 of length the number of end_pts_pathways for each individual
}


to_rotate_with_current<-function(origin,heading,several_second_direction,mean_wrapped_normal,sd_wrapped_normal){ ##calculate the angle of rotation between the current heading and any of the several_second_direction
  angles_second_directions<-to_angle_2_points(origin=origin,second_points=several_second_direction) ##calculate the angles (directions) from the origin to each one of the several_second_direction
  num_second_points<-unlist(lapply(several_second_direction,nrow)) ##retrieve the number of several_second_direction per individual
  headings<-rep.int(heading,num_second_points) ##repeat the heading value the number of times their number of corresponding several_second_directions
  angle_subtraction<-angles_second_directions-headings ##find the rotation angle between the 2 directions
  angle_subtraction[angle_subtraction>180]<-angle_subtraction[angle_subtraction>180]-360 ##angles reported are between -360 and 360, need to be between -180 and 180
  angle_subtraction[angle_subtraction<(-180)]<-angle_subtraction[angle_subtraction<(-180)]+360
  
  angles_prob<-dnorm(angle_subtraction,mean=mean_wrapped_normal,sd=sd_wrapped_normal) ##calculate the probability from a wrapped normal distribution
  seq_num_ind<-seq_len(nrow(origin)) ##create an index sequence for the number of individuals
  angles_prob_split<-split(angles_prob,rep.int(seq_num_ind,num_second_points)) ##split the angle by individual
  
  return(angles_prob_split) ##list of vectors (one for each individual) with values for each pathway proabilities
}


to_foray_loop<-function(going_away,num_steps_done,num_steps_max,origin,loop_starting_point,possible_destinations,mean_wrapped_normal,sd_wrapped_normal){ ##perform a foray loop movement by returning the values of the angles between the loop_starting_point if going back (or its opposite direction if going away) and each one of the possible destinations
  angle_rotations_prob<-ifelse(going_away==TRUE & num_steps_done<num_steps_max, ##if the individual was going away from its starting point at its previous loop movement and if it has not reach yet its maximum number of steps allowed for this loop
                               to_foray_loop_away(origin=origin,loop_starting_point=loop_starting_point,possible_destinations=possible_destinations,mean_wrapped_normal=mean_wrapped_normal,sd_wrapped_normal=sd_wrapped_normal), ##it keeps going away
                               to_foray_loop_back(origin=origin,loop_starting_point=loop_starting_point,possible_destinations=possible_destinations,mean_wrapped_normal=mean_wrapped_normal,sd_wrapped_normal=sd_wrapped_normal)) ##otherwise, if it was going away but had reached its maximum number of steps or it was going back from its starting point at its previous loop movement, it goes back to its starting location
  
  going_away[going_away==TRUE & num_steps_done>=num_steps_max]<-FALSE ##update the move done for the next one (to know if the individual did a looping away or back)
  
  results<-list("angle_rotations_prob"=angle_rotations_prob,
                "going_away"=going_away)
  
  return(results) ##list with first item = list of vectors of angles for each individual and second item = vector of TRUE and FALSE for going away or not
}


to_foray_loop_away<-function(origin,loop_starting_point,possible_destinations,mean_wrapped_normal,sd_wrapped_normal){ ##if the individual has to go away from its loop_starting_point
  ## Calculate the angles (directions) of the origin (current position) to their loop_starting_point
  angle_to_loop_pts<-deg(atan2(x=loop_starting_point[,1]-origin[,1],y=loop_starting_point[,2]-origin[,2])) ##calculate the angle of each direction (origin to loop_starting_point)
  angle_to_loop_pts[angle_to_loop_pts<0]<-angle_to_loop_pts[angle_to_loop_pts<0]+360 ##atan2() gives angles between -180° and 180° so they need to be re-adjusted between 0 and 360 
  angle_opposite_loop_starting_point<-angle_to_loop_pts+180 ##add 180 to these angles to find the opposite direction (i.e., going AWAY from this loop_starting_point)
  angle_opposite_loop_starting_point[angle_opposite_loop_starting_point>360]<-angle_opposite_loop_starting_point[angle_opposite_loop_starting_point>360]-360 ##if the new values are greater than 360°, rescale them
  
  ## Calculate the probabilities for the rotation angles between the direction away from the loop_starting_point and each one of the possible_destinations
  angle_rotate_away<-to_rotate_with_current(origin=origin,
                                            heading=angle_opposite_loop_starting_point,
                                            several_second_direction=possible_destinations,
                                            mean_wrapped_normal=mean_wrapped_normal,
                                            sd_wrapped_normal=sd_wrapped_normal)
  
  return(angle_rotate_away) ##list of vectors (one per individual) of probabilities for the rotation angles from the wrapped normal distribution
}


to_foray_loop_back<-function(origin,loop_starting_point,possible_destinations,mean_wrapped_normal,sd_wrapped_normal){ ##if the individual has to go back to its loop_starting_point
  ## Calculate the angles (directions) of the origin (current position) to their loop_starting_point
  angle_to_loop_pts<-deg(atan2(x=loop_starting_point[,1]-origin[,1],y=loop_starting_point[,2]-origin[,2])) ##calculate the angle of each direction (origin to loop_starting_point)
  angle_to_loop_pts[angle_to_loop_pts<0]<-angle_to_loop_pts[angle_to_loop_pts<0]+360 ##atan2() gives angles between -180° and 180° so they need to be re-adjusted between 0 and 360 
  
  ## Calculate the probabilities for the rotation angles between the direction back to its loop_starting_point and each one of the possible_destinations
  angle_rotate_back<-to_rotate_with_current(origin=origin,
                                            heading=angle_to_loop_pts,
                                            several_second_direction=possible_destinations,
                                            mean_wrapped_normal=mean_wrapped_normal,
                                            sd_wrapped_normal=sd_wrapped_normal)
  
  return(angle_rotate_back) ##list of vectors (one per individual) of probabilities for the rotation angles from the wrapped normal distribution
}


to_choose_best_pathway<-function(pathways_probabilities){ ##select one pathway based on their probabilities
  seq_num_ind<-seq_len(length(pathways_probabilities)) ##create an index sequence for the number of individuals
  pathways_probabilities_sum1<-lapply(seq_num_ind,function(k) pathways_probabilities[[k]]/sum(pathways_probabilities[[k]])) ##rescale the probabilities so that they sum to 1
  cum_sum_prob<-lapply(pathways_probabilities_sum1,cumsum) ##calculate the cumulative sum of all the pathway probabilities
  prob<-runif(seq_num_ind,0,1) ##draw a probability value at random between 0 and 1, one for each individual
  selected_pathway_probability<-lapply(seq_num_ind, ##for each individual ...
                                       function(j) match(min(cum_sum_prob[[j]][cum_sum_prob[[j]]>prob[j]]),cum_sum_prob[[j]])) ##"match" selects the pathway position for which the drawn probability falls in the pathway probability (based on the cumulative probability sum)
  
  return(selected_pathway_probability) ##list of unique value for each individual with the value of the selected pathway
}


to_move<-function(origin,heading,length){ ##give new coordinate based on the origin and the heading
  origin[,1]<-origin[,1]+cos(rad(heading))*length ##convert the angle (heading) in radians
  origin[,2]<-origin[,2]+sin(rad(heading))*length
  return(origin) ##each row is the new position
}