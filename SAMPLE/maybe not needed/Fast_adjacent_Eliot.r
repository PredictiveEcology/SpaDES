
library(microbenchmark)
a <- raster(extent(0,1000,0,1000),res=1)
sam = sample(1:length(a),1e4)
#sam = sample(1:10)
nc <- ncol(a)
numCell <- numCell(a)
microbenchmark(times=100L,
               adj.new <- adj(nc,numCell,sam,directions=8),
               adj.new.raster <- adj(rast=a,ind=sam,directions=8),
               adj.old <- adjacent(a,sam,directions=8,sorted=T)
)
