CreateCostSurface = function(input.map, output.map, mill.loc = c(-35000,610000), plot.maps = T,
  directions = 4, working.dir ="C:/Eliot/ProjectsBig/Caribou/CoteNord_v2_21nov12",
  seles.bin = "C:/Eliot/Google Drive/SELES/seles3_4.exe", trfn = function(x) 1/mean(x), 
  correct.for.raster.gaps = T, mask.map = "StudyArea", subregion.mask.map = NULL, 
  subregion.mask.map.groups = NULL, mill.loc.groups = NULL, merge.groups.output = F, 
  add.arbitrary.dist = NULL, use.map.selection = F, remove.ascii.maps = T, use.snowfall = F, 
  save.transition.map = T, transition.map.name=NULL, use.saved.transition = F, ...)
  
  {
    # Objective
    #  Take a road network map in GRASS compressed format (SELES uses) and calculate a cost surface to
    #  a mill, then resave that cost surface to GRASS compressed format for SELES

    # Three parts -
    # 1) Load the GRASS COMPRESSED road file
    # 2) Calculate cost surface to a mill
    # 3) Save that in GRASS COMPRESSED format

    setwd(working.dir)

    wind = function(x, y, z, ...)  {
      while (dev.set(x) != x) windows(y, z, ...)
    }
    require(SELES)
    require(raster)
    require(gdistance)

    # Part 1
    asc.files.already = strsplit(dir(pattern = ".asc"), split = ".asc")
    if (all(asc.files.already != mask.map)) convertGRASSCompressedToARCASCII(mask.map)
    SA.raw = raster(paste(mask.map,".asc",sep=""))
    ex1 = extent(SA.raw)
    

    if (!is.null(subregion.mask.map)) {    
      if (all(asc.files.already != subregion.mask.map)) 
        convertGRASSCompressedToARCASCII(subregion.mask.map)
      SubRegion.raw = raster(paste(subregion.mask.map,".asc",sep=""))
    }

    if (all(asc.files.already != input.map)) convertGRASSCompressedToARCASCII(input.map)
    map.raw = raster(paste(input.map,".asc",sep=""),overwrite = T)

    usines.raw = raster(paste("usines", ".asc", sep = ""))
    
    mill.loc.sp = SpatialPoints(mill.loc)

    projection(map.raw) = crs="+proj=utm +units=m"
    map.raw[!is.na(usines.raw)] = 1 # THis makes sure there is a road element at each usine
    map = (1-(!is.na(map.raw)+0))*9999+1 #* res(map.raw)[1]  # give each road element its value in m

if (use.map.selection) {
  plot(map)
  points(mill.loc.sp,pch = 19)
  map.sm = select(map)
  map = map.sm
  ex.sm = extent(map.sm)
  
  map.raw = crop(map.raw, ex.sm)
  SA.raw = crop(SA.raw, ex.sm)
  SubRegion.raw = crop(SubRegion.raw, ex.sm)
}

    if (plot.maps)  {   
      wind(3,12, 12)
      plot(map)
      points(mill.loc.sp,pch = 19)
    }


### temporary to keep simulation time down
#    map.sm = select(map)
#    map = map.sm
#    ex.sm = extent(map.sm)
#    
#    map.raw = crop(map.raw, ex.sm)
#    SA.raw = crop(SA.raw, ex.sm)
#    SubRegion.raw = crop(SubRegion.raw, ex.sm)
####### end of shrinking of maps... normally, this would be commented out    
    
    if (is.null(subregion.mask.map)) {
      num.subs = 1
    } else {
      num.subs = length(subregion.mask.map.groups)
    }

    cost.masked = list()

    # This is the long function
    if (!use.saved.transition) {
      tr = transition(map, transitionFunction=trfn, directions=directions) #15 minutes with large map
      if (save.transition.map)
        save(tr, file = transition.map.name)
    } else {
      load(file = transition.map.name)
    }
    
    
    funs = "lapply"
    if (use.snowfall) {require(snowfall); 
      sfInit(parallel = T, cpus=length(subregion.mask.map.groups))
      sfLibrary(gdistance);sfLibrary(raster);sfLibrary(rgdal)
      sfExport("mill.loc.sp", "tr", "ex1","correct.for.raster.gaps","map.raw","merge.groups.output",
        "SubRegion.raw","subregion.mask.map.groups","add.arbitrary.dist","num.subs"); 
      funs = "sfClusterApplyLB"
    }

    
    cost.masked = get(funs)(1:num.subs, function(i) {
#      for (i in 1:num.subs) {
        if(num.subs==1) mill.loc.sub = 1:nrow(mill.loc) else mill.loc.sub = mill.loc.groups[[i]]
        # Part 2
        # Can take many minutes if 2000x2000 pixels
      #    tr1 = geoCorrection(tr)  # CPU time = 64 hours!!!
      #    cost.surf3 = accCost(tr, mill.loc[3,])   # CPU time = 11 minutes 
        cost.surf = accCost(tr, mill.loc.sp[mill.loc.sub])   # CPU time = 11 minutes 
        cs = crop(cost.surf, ex1)

        #    correct holes in road map
        if (correct.for.raster.gaps) {
          cs = cs%%5e03
          cost = cs*res(map.raw)[1]
        } else {
          cost = cs*res(map.raw)[1]
        }
        rm(cs)
      
        if (merge.groups.output) {
          map.mask = !is.na(match(SubRegion.raw, subregion.mask.map.groups[[i]]))
          map.mask[map.mask==0] = NA
          
          map.ex1 = crop(map.mask, ex1)
  #        map.ex1.sm = crop(map.ex1, ex.sm)
  #        cost.masked[[i]] = raster::mask(cost, map.ex1.sm)
          cost.masked[[i]] = raster::mask(cost, map.ex1)
        } else {
          cost.masked[[i]] = raster::mask(cost, SA.raw)
#          cost.masked[[i]] = cost
        } 
        if (!is.null(add.arbitrary.dist)) 
           cost.masked[[i]] = cost.masked[[i]]+add.arbitrary.dist[[i]]
         rm(cost)
        return(cost.masked[[i]])
      }
      )
      if (use.snowfall) sfStop()
      
      if (merge.groups.output) {
        cost.masked.all = do.call(cover,cost.masked)
        assign(output.map,cost.masked.all)

        if (plot.maps) {
          wind(4, 12, 12)
          plot(get(output.map))
        }
      } else {
        cost.masked.all = cost.masked
        output.map.names = paste(output.map, "_uaf_",lapply(subregion.mask.map.groups,paste,collapse="_"),sep = "")
        for (k in 1:length(subregion.mask.map.groups)) assign(output.map.names[k],cost.masked.all[[k]])
        if (plot.maps) {
          wind(4, 12, 12)
          par(mfrow = c(1,length(subregion.mask.map.groups)))
          lapply(output.map.names, function(x) plot(get(x)))
        }
      }

    #Part 3
    if (merge.groups.output) {
      writeRaster(get(output.map), filename = output.map, format = "ascii", overwrite=T)
      convertARCASCIIToGRASSCompressed(output.map)
    } else {
      lapply(output.map.names, function(x) writeRaster(get(x),filename = x, format = "ascii", overwrite=T))
#      writeRaster(get(output.map.names), filename = output.map, format = "ascii", overwrite=T)
      convertARCASCIIToGRASSCompressed(output.map.names, seles.bin = seles.bin)
    }
    if (remove.ascii.maps) 
      file.remove(paste(output.map,".asc",sep=""),paste(input.map,".asc",sep=""),
        paste(mask.map,".asc",sep=""))
}

            