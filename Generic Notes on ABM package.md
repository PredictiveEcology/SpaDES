ABM - A generic platform for building 
========================================================

1. Data - 
    1. Retrieve from various sources
        1. GADM.org via sp package - administrative boundaries
        1. Ecological data 
            1. Boreal forest extent
               - http://cfs.nrcan.gc.ca/common/boreal.zip
            1. A classified land cover 
                1. LCC05 - Land Cover Classification 2005 (2000 imagery)
                    http://bit.ly/1ujeRE1
            1. Fire data
               http://cwfis.cfs.nrcan.gc.ca/datamart/datarequest/nfdbpnt
            1. Age class data
            1. knn Forest Cover
            1. 
            1. Any other
        1. Climate data
            - Past
            - Future projections
                - Downscaled
        1. Weather data
            - daily? monthly? annual?
        1. Geographic data
            1. DEM
            1. Topographic data
            1. Soils
    1. Manipulation
    1. Reprojection for spatial information
    1. Reclassification of categorical data 
        1. lcc05 - forest data

1. Code repository - such as GitHub
    1. Allows sharing
    1. Can be public
    1. Can be hosted privately on own server

1. Calibration of parameters used within simulation models
    1. Pattern Oriented Modeling
        - Take fire data
        - Fit models to estimate
            - Initiation Probability - The number of fires that start per area
            - Fire spread - The spread probability as a function of lakes, tree species, fuels, weather
    1. Curve calibration
    1. 
    
1. Build simulation model

1. Code style preferences
    - R style guide
        - Camel case is preferred
        - first letter always lower case, 



```r
setwd("C:/shared/data/shared/")
library(rgdal)
```

```
## Loading required package: sp
## rgdal: version: 0.8-16, (SVN revision 498)
## Geospatial Data Abstraction Library extensions to R successfully loaded
## Loaded GDAL runtime: GDAL 1.10.1, released 2013/08/26
## Path to GDAL shared files: C:/Eliot/R/win-library/3.0/rgdal/gdal
## GDAL does not use iconv for recoding strings.
## Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
## Path to PROJ.4 shared files: C:/Eliot/R/win-library/3.0/rgdal/proj
```

```r
library(raster)
```


```r
setwd("C:/shared/data/shared/boreal")
boreal <- readOGR(dsn=".", layer = "NABoreal")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: ".", layer: "NABoreal"
## with 7798 features and 3 fields
## Feature type: wkbPolygon with 2 dimensions
```


```r
lcc05 <- raster("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
plot(lcc05)
```

![plot of chunk Load LCC05](figure/Load LCC051.png) ![plot of chunk Load LCC05](figure/Load LCC052.png) 

```r
# Extract a sensible smaller extent
#ex <- drawExtent()
#lcc05.ex <- crop(lcc05,ex)
```


```r
age <- raster("C:/shared/data/shared/age/age.asc")
plot(age)
```

![plot of chunk Load age clases](figure/Load age clases.png) 

```r
crs(age)
```

```
## CRS arguments:
##  +proj=aea +lat_1=49 +lat_2=77 +lat_0=63.4 +lon_0=-91.867 +x_0=0
## +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
```

```r
# Extract a sensible smaller extent
#ex <- drawExtent()
#lcc05.ex <- crop(lcc05,ex)
```


```r
lccReclass <- structure(list(BAM.Land.Cover.Class.Name = structure(c(2L, 3L, 
5L, 6L, 4L, 9L, 17L, 18L, 16L, 11L, 14L, 12L, 15L, 10L, 13L, 
7L, 1L, 8L), .Label = c("Burns ", "Closed Coniferous ", "Closed Deciduous ", 
"Closed Deciduous Mixed ", "Closed Mature Mixed ", "Closed Young Mixed ", 
"Mixed Forest/Crop ", "Not Used ", "Open Coniferous ", "Open Herb/Grass ", 
"Open Mature Deciduous ", "Open Mixed ", "Open Northern ", "Open Young Deciduous ", 
"Open Young Mixed ", "Poorly Drained ", "Sparse Coniferous ", 
"Sparse Coniferous Shield "), class = "factor"), BAM.Code = c(1, 
2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 17, 21, 26, 33, NA), 
    LCC05.Labels = structure(c(1L, 8L, 11L, 14L, 15L, 16L, 17L, 
    18L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 12L, 13L), .Label = c("1", 
    "10,19", "11", "12,16", "14", "15", "17,18", "2", "21,22,23,24,25,30,31,32", 
    "26,27,28,29", "3", "33,34,35", "36,37,38,39", "4", "5", 
    "6,7", "8,13,20", "9"), class = "factor")), .Names = c("BAM.Land.Cover.Class.Name", 
"BAM.Code", "LCC05.Labels"), class = "data.frame", row.names = c(NA, 
-18L))
lcc05Labels <- as.numeric(strsplit(paste(lcc.reclass$LCC05.Labels,collapse = ","),",")[[1]])
```

```
## Error: object 'lcc.reclass' not found
```

```r
numOldInNew <- sapply(strsplit(unname(sapply(as.character(lccReclass$LCC05.Labels),function(x) x)),","),length)
lcc05Reclass <- cbind(lcc05Labels,rep(lccReclass$BAM.Code,numOldInNew))
```

```
## Error: object 'lcc05Labels' not found
```

```r
lcc05BAM <- writeRaster(reclassify(lcc05,lcc05Reclass),filename="lcc05BAMReclass.grid")
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'writeRaster': Error in .local(x, rcl, ...) : object 'lcc05Reclass' not found
## Calls: reclassify -> reclassify -> .local
```

