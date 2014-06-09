Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r make empty raster and data.table}
a <- raster(extent(0,1000,0,1000),res=1)
#b <- a
    
adj <- function(b,ind) {
    dim.ras<-dim(b)
    nc=dim.ras[2]
    topl=ind-nc-1
    top=ind-nc
    topr=ind-nc+1
    lef=ind-1
    rig=ind+1
    botl=ind+nc-1
    bot=ind+nc
    botr=ind+nc+1
    out=data.table(from=rep(ind,times=8),to=c(topl,top,topr,lef,rig,botl,bot,botr),key=c("from"))
    out[,`:=`(from.mod.nc=from%%nc,to.mod.nc=to%%nc)]
    out2 <- out[
        i = !(to<1 |
            to>prod(dim.ras[1:2]) |
            (from.mod.nc==0 & to.mod.nc==1) |
            (from.mod.nc==1 & to.mod.nc==0))
#        to:=NA
        ]
    
#    out2<-out[!is.na(to)]
    return(out2)
}

library(microbenchmark)
sam = sample(1:length(a),1e3)
microbenchmark(
adj.new <- adj(a,sam),
adj.old <- adjacent(a,sam,directions=8,sorted=T)
)


summary(cars)
```

You can also embed plots, for example:

```{r fig.width=7, fig.height=6}
plot(cars)
```

