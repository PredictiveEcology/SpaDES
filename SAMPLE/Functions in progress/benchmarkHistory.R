# for(i in 1:2) {
#   print(system.time(mySim <- spades(mySim, debug=F)))
#   simStopTime(mySim) <- i + 1
# }
rm(nPixelsBurned)
print(system.time(mySim <- spades(mySim, debug=T)))
Plot(vegMapInit, vegMap)
dev(5)
if("fireSpreadLcc" %in% simModulesLoaded(mySim))  hist(nPixelsBurned/6.25, xlab="Hectares",
main=paste0("Hectares burned")); #simStopTime(mySim)<-20
#dev.off()
times=list(start=0.0, stop=20)
parameters <- list(.globals=list(burnStats="nPixelsBurned"),
.progress=list(NA),#.graphical=FALSE, .progressInterval=1),
#.loadFileList=fileList,
forestSuccession=list(returnInterval=1, startTime=0,
.plotInitialTime=1, .plotInterval=1),
forestAge=list(returnInterval=1, startTime=0.5,
.plotInitialTime=1, .plotInterval=1),
fireSpreadLcc=list(nFires= 100, #spreadprob=0.225,
its=1e6, drought=1.2,
persistprob=0, returnInterval=1, startTime=1,
.plotInitialTime=1, .plotInterval=1)
#                   caribouMovement=list(N=1e2, moveInterval=1,
#                                        .plotInitialTime=1.01, .plotInterval=1)
)
modules <- list("forestSuccession", "forestAge", "fireSpreadLcc")
path <- file.path("C:","Eliot","GitHub","SpaDES","SAMPLE")
trajMap <- RasterLayerNamed(trajMapInit, name="trajMap")
ageMap <- RasterLayerNamed(ageMapInit, name="ageMap")
vegMap <- RasterLayerNamed(vegMapInit, name="vegMap")
mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
dev(4)#pdf("test.pdf")
# for(i in 1:2) {
#   print(system.time(mySim <- spades(mySim, debug=F)))
#   simStopTime(mySim) <- i + 1
# }
rm(nPixelsBurned)
print(system.time(mySim <- spades(mySim, debug=T)))
Plot(vegMapInit, vegMap)
dev(5)
if("fireSpreadLcc" %in% simModulesLoaded(mySim))  hist(nPixelsBurned/6.25, xlab="Hectares",
main=paste0("Hectares burned")); #simStopTime(mySim)<-20
#dev.off()
shiny::runApp('SAMPLE/shiny_succession')
shiny::runApp('SAMPLE/shiny_succession')
library(SpaDES)
rasterOptions(maxmemory=2e9)
downloadRequired = FALSE
lcc05
dev(4)
plot(lcc05)
library(SpaDES)
rasterOptions(maxmemory=2e9)
downloadRequired = FALSE
interactiveExtent = T
setwd("~")
dir.create("spadesTmp")
setwd(file.path("spadesTmp"))
if(downloadRequired) {
#46MB file
fN <- "lcc05.zip"
download.file("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip", fN, mode="wb")
unzip(fN, files="LCC2005_V1_4a.tif")
# 9MB file
download.file("ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
"age.tif",mode="wb")
}
fileList <- data.frame(files=c("LCC2005_V1_4a.tif",
"age.tif"),
functions="raster", packages="raster",
objectNames=c("lcc05","age"),
stringsAsFactors=FALSE)
loadFiles(fileList=fileList)
ext <- extent(-1380607, -345446, 7211410, 7971750) # large central BC 12Million
#ext <- extent(1612240, 1895057, 6756615, 6907451) # small 600k pixels Quebec City Lac St. Jean
#QC.crs = crs("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-71 +x_0=0 +y_0=0
#+ellps=GRS80 +units=m +no_defs")
#ext <- extent(-1073154,-987285,7438423,7512480) # small central Sask 100 Thousand
if(interactiveExtent) {
dev(4)
plot(lcc05)
ext <- drawExtent()
}
library(SpaDES)
rasterOptions(maxmemory=2e9)
downloadRequired = FALSE
interactiveExtent = T
setwd("~")
dir.create("spadesTmp")
setwd(file.path("spadesTmp"))
if(downloadRequired) {
#46MB file
fN <- "lcc05.zip"
download.file("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip", fN, mode="wb")
unzip(fN, files="LCC2005_V1_4a.tif")
# 9MB file
download.file("ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
"age.tif",mode="wb")
}
fileList <- data.frame(files=c("LCC2005_V1_4a.tif",
"age.tif"),
functions="raster", packages="raster",
objectNames=c("lcc05","age"),
stringsAsFactors=FALSE)
loadFiles(fileList=fileList)
ext <- extent(-1380607, -345446, 7211410, 7971750) # large central BC 12Million
#ext <- extent(1612240, 1895057, 6756615, 6907451) # small 600k pixels Quebec City Lac St. Jean
#QC.crs = crs("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-71 +x_0=0 +y_0=0
#+ellps=GRS80 +units=m +no_defs")
#ext <- extent(-1073154,-987285,7438423,7512480) # small central Sask 100 Thousand
if(interactiveExtent) {
dev(4)
plot(lcc05)
ext <- drawExtent()
}
vegMapLcc <- RasterLayerNamed(crop(lcc05,ext), name="vegMapLcc")
setColors(vegMapLcc, n=255) = getColors(lcc05)[[1]][-1]
if(ncell(vegMapLcc)>1e6) beginCluster(12)
# age will not run with projectRaster directly. Instead, project the vegMap to age, then crop, then project back to vegMap
vegMapLcc.crsAge = projectRaster(vegMapLcc, crs=crs(age))
age.crsAge <- crop(age, vegMapLcc.crsAge)
ageMap <- projectRaster(age.crsAge, to=vegMapLcc, method="ngb")
#ageMap <- projectRaster(ageMap, crs = QC.crs, method="ngb")
#vegMapLcc <- projectRaster(vegMapLcc, crs = QC.crs, method="ngb")
endCluster()
writeRaster(ageMap, filename="ageMap.tif", overwrite=TRUE)
### From the table 1 in Word file from Steve Cumming & Pierre Vernier, June 6, 2014
###  09 A5 MDR ANslysis V4_SL.docx
#
# lcc05TrajReclass <- read.table(file="clipboard", header=TRUE, sep="\t")
# dput(lcc05TrajReclass[,c("LCC05.classes","Trajectory","Description")])
# dput(lcc05TrajReclass[,c("LCC05.classes","VEG.reclass","Description")])
#
lcc05TrajReclass <- structure(
list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
.Label=c("0,30,31,32,33,36,38,39", "1",
"16,35", "17,18,20,21,22,23,24,25",
"19", "2,11,12", "26,27,28,29",
"3,4,5,13,14,15", "34", "37",
"6,7,8,9,10"), class = "factor"),
Trajectory=structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 11L, 3L, 4L),
.Label=c("1,2,3,4,5,6", "1,3,4,5,6", "10",
"11", "2,4", "3,4,5", "3,4,6", "6",
"6", "8", "9"), class = "factor"),
Description=structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
.Label=c("Burned", "Closed coniferous", "Cropland",
"Deciduous", "Herbaceous", "Mixedwood",
"Open coniferous", "Other", "Shrub",
"Water", "Wetland"), class = "factor")),
.Names=c("LCC05.classes", "Trajectory", "Description"),
class="data.frame", row.names = c(NA, -11L))
lcc05VegReclass <- structure(
list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
.Label=c("0,30,31,32,33,36,38,39", "1",
"16,35", "17,18,20,21,22,23,24,25",
"19", "2,11,12", "26,27,28,29",
"3,4,5,13,14,15", "34", "37",
"6,7,8,9,10"), class = "factor"),
VEG.reclass=1:11, Description=structure(
c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
.Label = c("Burned", "Closed coniferous",  "Cropland", "Deciduous",
"Herbaceous", "Mixedwood", "Open coniferous", "Other",
"Shrub", "Water", "Wetland"), class = "factor")),
.Names = c("LCC05.classes", "VEG.reclass", "Description"),
class = "data.frame", row.names = c(NA, -11L))
lcc05VegLabels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes, collapse=","),",")[[1]])
numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes), function(x) x)), ","), length)
lcc05VegTable <- cbind(lcc05VegLabels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))
vegMap <- RasterLayerNamed(reclassify(vegMapLcc, lcc05VegTable),name="vegMap")
vegMapColors <<- getColors(lcc05)[[1]][c(1,lcc05VegTable[,1][match(1:11,
lcc05VegTable[,2])]+1)]
setColors(vegMap, n=11 ) <- vegMapColors[-1]
# the raster package does not keep colors when writing to a tif file
writeRaster(vegMap, filename="vegMap.tif", overwrite=TRUE)
lcc05TrajLabels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes, collapse=","), ",")[[1]])
numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes), function(x) x)), ","), length)
lcc05TrajReclass$TrajectoryNum <- lapply(as.character(
lcc05TrajReclass$Trajectory), function(x) as.numeric(strsplit(x,",")[[1]]))
lcc05TrajTable <- cbind(
lcc05TrajLabels,
unlist(
lapply(1:length(lcc05TrajReclass$TrajectoryNum),
function(x)
sample(lcc05TrajReclass$TrajectoryNum[[x]],
numLccInTraj[x],replace=T))))
#  lcc05TrajTable <- cbind(lcc05TrajLabels,rep(lcc05TrajReclass$Trajectory,numLccInTraj))
trajMap <- reclassify(vegMapLcc, lcc05TrajTable)
setColors(trajMap,n=12) <- brewer.pal(9, "YlGn")
name(trajMap) <- "trajMap"
# trajObj.raw <- read.table(file="clipboard", sep="\t", header=TRUE, stringsAsFactors=FALSE)
# dput(trajObj.raw)
trajObj.raw <- structure(
list(Veg.Type=c("Closed coniferous", "Open coniferous", "Mixedwood",
"Deciduous*", "Deciduous*", "Shrub", "Herbaceous"),
X0.2=c("Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Burned"),
X3.20=c("Closed coniferous", "Open coniferous", "Deciduous",
"Deciduous", "Deciduous", "Shrub", "Herbaceous"),
X21.60=c("Closed coniferous", "Open coniferous", "Mixedwood",
"Mixedwood", "Deciduous", "Shrub", "Herbaceous"),
X61.80=c("Closed coniferous", "Open coniferous", "Mixedwood",
"Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
X81.120=c("Closed coniferous", "Open coniferous", "Mixedwood",
"Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
X121.160=c("Closed coniferous", "Open coniferous", "Mixedwood",
"Open coniferous", "Deciduous", "Closed coniferous",
"Herbaceous"),
X.160=c("Closed coniferous", "Open coniferous", "Closed coniferous",
"Closed coniferous", "Closed coniferous", "Closed coniferous",
"Herbaceous")),
.Names=c("Veg.Type", "X0.2", "X3.20", "X21.60", "X61.80", "X81.120", "X121.160", "X.160"),
class="data.frame", row.names=c(NA, -7L))
numYearsPer <- na.omit(unlist(lapply(strsplit(substr(colnames(trajObj.raw),2,9),"\\."), function(x) diff(as.numeric(x))))+1)
maxAge <- 200
ages <- 0:maxAge
trajObj1 <- apply(trajObj.raw[-4,-c(1)],1,function(x) rep(x, times=c(numYearsPer, maxAge+1-sum(numYearsPer))))
trajObj2 <- cbind(trajObj1,matrix(rep(c("Burned", "Wetland", "Water", "Cropland","Other"), each=maxAge+1), ncol=5))
trajObj <<- matrix(match(trajObj2,
as.character(lcc05TrajReclass$Description))
, ncol=ncol(trajObj2))
ageMapInit <- RasterLayerNamed(ageMap, name="ageMapInit")
vegMapInit <- RasterLayerNamed(vegMap, name="vegMapInit")
trajMapInit <- RasterLayerNamed(trajMap, name="trajMapInit")
setColors(vegMapInit, n=11 ) <- vegMapColors[-1]
writeRaster(trajMapInit, filename="trajMap.tif", overwrite=TRUE)
times=list(start=0.0, stop=20)
parameters <- list(.globals=list(burnStats="nPixelsBurned"),
.progress=list(NA),#.graphical=FALSE, .progressInterval=1),
#.loadFileList=fileList,
forestSuccession=list(returnInterval=1, startTime=0,
.plotInitialTime=1, .plotInterval=1),
forestAge=list(returnInterval=1, startTime=0.5,
.plotInitialTime=1, .plotInterval=1),
fireSpreadLcc=list(nFires= 100, #spreadprob=0.225,
its=1e6, drought=1.2,
persistprob=0, returnInterval=1, startTime=1,
.plotInitialTime=1, .plotInterval=1)
#                   caribouMovement=list(N=1e2, moveInterval=1,
#                                        .plotInitialTime=1.01, .plotInterval=1)
)
modules <- list("forestSuccession", "forestAge", "fireSpreadLcc")
path <- file.path("C:","Eliot","GitHub","SpaDES","SAMPLE")
trajMap <- RasterLayerNamed(trajMapInit, name="trajMap")
ageMap <- RasterLayerNamed(ageMapInit, name="ageMap")
vegMap <- RasterLayerNamed(vegMapInit, name="vegMap")
mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
dev(4)#pdf("test.pdf")
# for(i in 1:2) {
#   print(system.time(mySim <- spades(mySim, debug=F)))
#   simStopTime(mySim) <- i + 1
# }
rm(nPixelsBurned)
print(system.time(mySim <- spades(mySim, debug=F)))
Plot(vegMapInit, vegMap)
dev(5)
if("fireSpreadLcc" %in% simModulesLoaded(mySim))  hist(nPixelsBurned/6.25, xlab="Hectares",
main=paste0("Hectares burned")); #simStopTime(mySim)<-20
#dev.off()
library(SpaDES)
shiny::runApp('C:/Eliot/GitHub/SpaDES/SAMPLE/shiny_succession')
library(siny
library(shiny)\
library(shiny)
install.packages("shiny")
install.packages("shiny")
install.packages("shiny", method = "source")
?install.packages
f <- function(x) NULL
s3 <- function(x) UseMethod("s3")
s3.integer <- f
A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)
B <- setRefClass("B", methods = list(rc = f))
a <- A()
b <- B$new()
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
library(microbenchmark)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
,times=1e4)
s3(1L)
s4(a)
A()
a <- 1
f <- function() {
g <- function() {
print(a)
assign("a", 2, envir = parent.frame())
print(a)
a <- 3
print(a)
}
g()
}
f()
library(Rcpp)
using namespace Rcpp;
// [[Rcpp::export]]
double cond_sum_cpp(NumericVector x, NumericVector y,
LogicalVector z) {
double sum = 0;
int n = x.length();
for(int i = 0; i < n; i++) {
if (!z[i]) continue;
sum += x[i] + y[i];
}
return sum;
}
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double cond_sum_cpp(NumericVector x, NumericVector y,
LogicalVector z) {
double sum = 0;
int n = x.length();
for(int i = 0; i < n; i++) {
if (!z[i]) continue;
sum += x[i] + y[i];
}
return sum;
}
Reduce("+", 1:5)
Reduce("+", 1:1e3)
microbenchmark(Reduce("+", 1:1e3), sum(1:1e3))
?Reduce
sourceCpp(file="clipboard")
setwd("SAMPLE/")
sourceCpp(file="meanCPP.cpp")
ls()
dir()
setwd("Functions in progress/")
sourceCpp(file="meanCPP.cpp")
library(microbenchmark)
x <- runif(1e5)
microbenchmark(
mean(x),
meanC(x)
)
library(microbenchmark)
x <- runif(1e6)
microbenchmark(
mean(x),
meanC(x)
)
sourceCpp(file="meanCPP.cpp")
f1
f1(x)
microbenchmark(f1(x), mean(x))
microbenchmark(f1(x), mean(x), meanC(x))
microbenchmark(a<-f1(x), b<-mean(x), d<-meanC(x))
a
b
d
microbenchmark(a<-f1(x), b<-mean(x), d<-meanC(x))
microbenchmark(a<-f2(x), b<-var(x)
)
microbenchmark(a<-f2(x), b<-var(x))
a
b
microbenchmark(a<-f2(x), b<-cumsum(x))
all.equal(a,b)
mean
showMethods("mean")
f3(x)
x
microbenchmark(a<-f1(x), b<-mean(x), e<-.Internal(mean(x)), d<-meanC(x))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), d<-meanC(x))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-rowMeans(x))
rowMeans
x1 = matrix(x, ncol=1)
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-rowMeans(x1))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1), times=300L)
mb<-microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1), times=300L)
sum
length
mean
mb<-microbenchmark(a<-f1(x), b<-base::mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1), times=300L)
mb
f4()
f4(10)
f4(10, 1:10)
f4(mean, 1:10)
f4(sum, 1:10)
f4(sum, 2:10)
f4(sum, c(F,F,F,T))
f4(sum, c(F,F,F,T, F, F, F))
f4(sum, c(F,F,F,T, F, F, F, T))
f5(x, x)
cbind(x,x)[1:10;]
cbind(x,x)[1:10,]
cbind(x,x*2)[1:10,]
f5(x, x*2)
f5(x, x*2)[1:10,]
f5(x, x*2)[1:10]
cbind(x,x*2)[1:10,]
pmin(x,x*2)
pmin(x,x*2)[1:10]
f5(x, x*2)[1:10]
mb<-microbenchmark(a<-f1(x), b<-base::mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
mb<-microbenchmark(a<-f5(x,x*2),b<-pmin((x,x*2))
mb<-microbenchmark(a<-f5(x,x*2),b<-pmin(x,x*2))
all.equal(a,b)
mb
pmin
mb<-microbenchmark(a<-f5(x,x*2),b<-base::pmin(x,x*2))
mb
mb<-microbenchmark(a<-f5(x,x*2),b<-base::pmin(x,x*2), d<-.Internal(pmin(x,x*2)))
mb
.Internal(pmin)
.Internal(pmin)
.Internal(pmin(...))
all.equal(a,b,d)
length(x)
f <- function(x) NULL
s3 <- function(x) UseMethod("s3")
s3.integer <- f
A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)
B <- setRefClass("B", methods = list(rc = f))
a <- A()
b <- B$new()
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
library(microbenchmark)
microbenchmark(
fun = f(),
S3 = s3(1L),
S4 = s4(a),
RC = b$rc()
)
mb<-microbenchmark(a<-f1(x), b<-base::mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
sourceCpp(file="meanCPP.cpp")
library(Rcpp)
setwd("SAMPLE/")
setwd("Functions in progress/")
sourceCpp(file="meanCPP.cpp")
mb<-microbenchmark(a<-f5(x,x*2),b<-base::pmin(x,x*2), d<-.Internal(pmin(x,x*2)))
x = rnorm(1e5)
mb<-microbenchmark(a<-f5(x,x*2),b<-base::pmin(x,x*2), d<-.Internal(pmin(x,x*2)))
mb
mb<-microbenchmark(a<-f1(x), b<-base::mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
x1 = matrix(x, ncol=1)
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1))
sum
length
colMeans
microbenchmark(a<-f1(x), b<-mean(x), e<-mean.default(x), g<-sum(x)/length(x), d<-meanC(x), h<-colMeans(x1), i<-.Internal(mean(x)))
.Internal(min(x))
.Internal(max(x))
.Internal(x)
mb<-microbenchmark(a<-f5(x,x*2),b<-base::pmin(x,x*2), d<-.Internal(pmin(x,x*2)))
mb
savehistory("C:/Eliot/GitHub/SpaDES/SAMPLE/Functions in progress/benchmarkHistory.R")
