# A short file showing how to use GRASS from R. However
#  Manual for all GRASS commands is here:
#   http://grass.osgeo.org/grass64/manuals/
#  Note the difference between raster commands (r.*) and vector commands (v.*)

library(spgrass6)

# Load the MPB R objects (point and polygon and raster maps)
setwd("c:/Rwork/MPB")
objects2load= c("bc", "ab", "ab.poly", "west", "west.county", "west.r", "bc.poly")
lapply(objects2load,
  function(x) load(file = paste("mpb.",x,".rdata",sep=""),env = globalenv()))

# Example: Run a buffer command, v.buffer, on the points in one year, 2009, of spot MPB data
    #1. create a map which has all the projection issues, as the basis for the GRASS workspace
    west.sg = as(west.r,"SpatialGrid")

    #2. Start GRASS
    initGRASS(gisBase="C:\\Program Files (x86)\\GRASS 6.4.2",override=T, SG=west.sg)

    #3. Move an R object to GRASS, using "overwrite"
    writeVECT6(bc[["2009"]], vname="bc2009spot", v.in.ogr_flags = c("o","overwrite"))

    #4. Run the command in GRASS, specifically, v.buffer
    execGRASS("v.buffer", flags = c("s","verbose","overwrite"),input="bc2009spot",output="bc2009_buff",distance=100)

    #5. Return that map to R as an R object
    bc2009_buff = readVECT6("bc2009_buff")

    # It turns out that an R command, gBuffer in the Rgeos library is about 15 times faster than the GRASS command
    library(rgeos)
    library(rbenchmark)
    ben = benchmark(replications = 1,
    bc2009_buff2 = gBuffer(bc[["2009"]], width = 1000),
    execGRASS("v.buffer", flags = c("s","verbose","overwrite"),input="bc2009spot",output="bc2009_buff",distance=1000))
    print(ben)



############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
# OTher potentially useful... though maps are not here, so these don't work
projection(roads.sg) = crs="+proj=utm +units=m +ellps=WGS84"
projection(roads.sgdf) = crs="+proj=utm +units=m +ellps=WGS84"
projection(roads.notroad.sgdf) = crs="+proj=utm +units=m +ellps=WGS84"

# Raster specific
writeRAST6(roads.notroad.sgdf,vname="road_notroad", overwrite = T, flags="o")

execGRASS("r.cost", flags = c("k","verbose","overwrite"),input="road",output="road.cost", start_points="MillLoc")

road.cos = raster(readRAST6("road.cost"))
