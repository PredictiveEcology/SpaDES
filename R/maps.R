### MAPS LIBRARY

ProbInit = function(map, p=NULL, absolute=FALSE) {
    if (length(p) == 1) { 
        ProbInit = raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
        ProbInit = setValues(ProbInit, rep(p,length(ProbInit)))
    } else if (is(p,"RasterLayer")) {
        ProbInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
    } else if (is(map,"SpatialPolygonsDataFrame")) {
        ProbInit = p/sum(p) 
    } else if (is(p,"NULL"))  {
        ProbInit = map/(cellStats(p,sum)*(1-absolute)+1*(absolute))
    } else {
        stop("error initializing probability map: bad inputs") # temp err msg (general)
    }
    return(ProbInit)
}


# To initialize with a specific number per patch, which may come from
#  data or have been derived from patch size. Options include a combination of either
#  a patchid map and a table with 2 columns, pops and num.in.pop,
#  or 2 maps, patchid and patchnumber. Returns a map with a single unique pixel
#  within each patch representing an agent to start. This means that the number
#  of pixels per patch must be greater than the number of agents per patch
spec.num.per.patch = function(patches, num.per.patch.table = NULL, num.per.patch.map=NULL) {
    patchids = as.numeric(na.omit(getValues(patches)))
    wh = Which(patches, cells = T)
    if (!is.null(num.per.patch.table)) {
        dt1 = data.table(wh, pops=patchids)
        setkey(dt1, pops)
        if (is(num.per.patch.table, "data.table")) {
            num.per.patch.table = data.table(num.per.patch.table)
        }
        setkey(num.per.patch.table, pops)
        dt2 = dt1[num.per.patch.table]
    } else if (!is.null(num.per.patch.map)) {
        num.per.patch.table = as.numeric(na.omit(getValues(num.per.patch.map)))
        dt2 = data.table(wh,pops = patchids, num.in.pop = num.per.patch.table)
    } else { stop("need num.per.patch.map or num.per.patch.table") }
    
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    dt3 = dt2[, list(cells=resample(wh, unique(num.in.pop))), by=pops]
    dt3$ids = rownames(dt3)
    
    al = raster(patches)
    al[dt3$cells] = 1
    
    return(al)
}

spread = function(maps, start.points, r=NULL, id.colour = TRUE, 
                  ncells=NULL, fn=expression(!is.na(maps)), backwards = FALSE, ...) {
    
    spread = raster(maps)
    spread[] = NA
    start.cells = sort(cellFromXY(spread,start.points))
    spread[start.cells] = 1
    spread.start.id = raster(maps)
    spread.start.id[] = NA
    spread.start.id[start.cells] = start.cells
    
    for (i in 1:r) {
        spreadadj = adjacent(spread, Which(spread==i, cells=TRUE), sorted=TRUE, ...)
        
        spread[spreadadj[,"to"][eval(fn)[spreadadj[,"to"]] & 
                                    is.na(spread[spreadadj[,"to"]])]] = i+1
        spread.start.id[spreadadj[,"to"]] = spread.start.id[spreadadj[,"from"]]
    }
    if (id.colour) { return(spread.start.id) }
    else {return(spread)}
}




patch.size = function(patches) {
    patch.size = freq(patches)
    return(patch.size)
}
