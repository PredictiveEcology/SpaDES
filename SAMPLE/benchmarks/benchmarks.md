---
title: "Benchmarking with R/SpaDES R 2.15.3"
author: "Eliot McIntire"
date: "Friday, Octore 6, 2014"
output: html_document
---

The objective of this file is to show some speed comparisons between R and C++ and in some cases, other languages or software.

## Low level functionality

We will begin with low level functions that are generally highly optimized in R. As a result, the comparison C++ functions, which are not optimized, may not fully represent what C++ could do. However, this represents a real world issue: if the "out of the box" R function is competitive with a "quick" C++ version, then the R version is easier to write as there is no further development. If there is a desire or need for more speed, then a more optimzed C++ version can be written and used either in native C++ applications or R.





For the mean, I show two different C++ versiopns. The R function, "mean" is somewhat slower (1/2x), but the .Primitive option in R, sum/length is faster than either C++ function.

```
## Unit: microseconds
##                     expr     min      lq  median      uq     max neval
##           a <- meanC1(x)   92.77   93.69   94.92   98.61   183.1   100
##           d <- meanC2(x)  662.62  663.54  664.77  669.53   733.9   100
##             b <- mean(x)  193.53  196.60  199.37  211.50   272.5   100
##     e <- mean.default(x)  183.70  184.78  186.16  188.92   293.1   100
##    g <- sum(x)/length(x)   91.24   92.47   93.08   94.46   129.9   100
##  i <- .Internal(mean(x))  181.24  181.55  182.17  183.40   246.1   100
##        h <- rowMeans(x1) 1060.13 1152.13 1232.46 1653.63 29686.9   100
```

```
## [1] TRUE
```



Below, we take the minimum of each of a pair of columns.  

```r
x2 <- rnorm(1e+05)
rm(a, b, d)
(mb <- microbenchmark(a <- pminC(x, x2), b <- base::pmin(x, x2), d <- .Internal(pmin(x, 
    x2))))
```

```
## Unit: nanoseconds
##                         expr     min      lq  median      uq      max
##            a <- pminC(x, x2) 1246593 1285146 1722898 2063883 30480701
##       b <- base::pmin(x, x2) 2091991 2197051 2656614 2947679 33193530
##  d <- .Internal(pmin(x, x2))       0     308    1229    1843     5222
##  neval
##    100
##    100
##    100
```

```r
print(pmins <- round(summary(mb)[[4]][1]/min(summary(mb)[[4]][3]), 0))
```

```
## [1] 1402
```

```r
all.equal(a, b, d)
```

```
## [1] TRUE
```


The internal R function is ***1402x*** faster than the C++ version, or the base R version.

This is taken from a blog post by Wingfeet at http://www.r-bloggers.com/quicksort-speed-just-in-time-compiling-and-vectorizing/ which drew on benchmark tests here: http://julialang.org/ 
Essentially, this was a benchmark to test the speed of Julia. It shows for the Quicksort, that R is 524x slower than C.  Below is a "simple" version, then the best, fastest version that Wingfeet was able to do. But, there was no explicit comparison of how the base R sort would match with C. 




Real number sortin:

```r
x = runif(1e+05)
(mb <- microbenchmark(a0 <- qsort(x), a <- wfqsx(x), b <- wfqs1(x), d <- sort(x), 
    e <- sort(x, method = "quick"), f <- .Internal(sort(x, decreasing = FALSE)), 
    g <- data.table(x = x, key = "x"), times = 1L))
```

```
## Unit: milliseconds
##                                         expr      min       lq   median
##                               a0 <- qsort(x) 5452.528 5452.528 5452.528
##                                a <- wfqsx(x) 1567.316 1567.316 1567.316
##                                b <- wfqs1(x)  923.701  923.701  923.701
##                                 d <- sort(x)   12.366   12.366   12.366
##               e <- sort(x, method = "quick")    8.451    8.451    8.451
##  f <- .Internal(sort(x, decreasing = FALSE))   11.703   11.703   11.703
##            g <- data.table(x = x, key = "x")    8.651    8.651    8.651
##        uq      max neval
##  5452.528 5452.528     1
##  1567.316 1567.316     1
##   923.701  923.701     1
##    12.366   12.366     1
##     8.451    8.451     1
##    11.703   11.703     1
##     8.651    8.651     1
```

```r
print(sumReals <- round(summary(mb)[[4]][1]/min(summary(mb)[[4]][4:7]), 0))
```

```
## [1] 645
```

```r
all.equal(a0, a, b, d, e, f, g$x)
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

```
## [1] TRUE
```


And Integers are faster in the low-level R functions:

```r
x = sample(1e+05)
(mb <- microbenchmark(a0 <- qsort(x), a <- wfqsx(x), b <- wfqs1(x), d <- sort(x), 
    e <- sort(x, method = "quick"), f <- .Internal(sort(x, decreasing = FALSE)), 
    g <- data.table(x = x, key = "x"), times = 1L))
```

```
## Unit: milliseconds
##                                         expr      min       lq   median
##                               a0 <- qsort(x) 5544.419 5544.419 5544.419
##                                a <- wfqsx(x) 1556.466 1556.466 1556.466
##                                b <- wfqs1(x)  895.043  895.043  895.043
##                                 d <- sort(x)   10.881   10.881   10.881
##               e <- sort(x, method = "quick")    7.457    7.457    7.457
##  f <- .Internal(sort(x, decreasing = FALSE))   10.818   10.818   10.818
##            g <- data.table(x = x, key = "x")    2.984    2.984    2.984
##        uq      max neval
##  5544.419 5544.419     1
##  1556.466 1556.466     1
##   895.043  895.043     1
##    10.881   10.881     1
##     7.457    7.457     1
##    10.818   10.818     1
##     2.984    2.984     1
```

```r
print(sumInts <- round(summary(mb)[[4]][1]/min(summary(mb)[[4]][4:7]), 0))
```

```
## [1] 1858
```

```r
all.equal(a0, a, b, d, e, f, g$x)
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

```
## [1] TRUE
```


The first three function are 3 different implementations of the quicksort algorithm shown on the Julia pages, with the first, qsort, being the one that the Julia testers used. Using the data.table sorting we were able to achieve ***483x*** speedup if Reals, and **1858x** speedup if integers. These put them as fast or faster than C or Fortran or Julia. In Wingfeet's blog post, he also showed that using JIT can speed up non-optimized, "procedural" R code, though not as fast as the low level functions that exist in various R packages.


Again, the fastest native R function is faster than a simple (unoptimized) C or C++ function. As with any language, there is faster code and slower code. With R, it may take a few tries, but there is usually a very fast option.

#### Conclusions
Clearly, low level speed can be achieved within R, often better than quick implementations in C or C++. This is because efforts have been made in primitives and internal functions with core R functions to provide optimal versions, without any extra user coding.  Many work flows to not require explicit loops. R's vectorization model allows for fast code, with little coding. ***Write vectorized code in R***

## Loops

```r

N = 20000
(mb = microbenchmark(times = 5L, naiveVector = {
    set.seed(104)
    a <- numeric()
    for (i in 1:N) {
        a[i] = rnorm(1)
    }
}, presetVector = {
    set.seed(104)
    b <- numeric(N)
    for (i in 1:N) {
        b[i] = rnorm(1)
    }
}, vectorized = {
    set.seed(104)
    d <- rnorm(N)
}))
```

```
## Unit: milliseconds
##          expr     min      lq  median     uq     max neval
##   naiveVector 549.201 564.754 566.979 637.41 646.171     5
##  presetVector  71.408  71.716  75.003  75.56  76.009     5
##    vectorized   1.539   1.587   1.668   1.67   1.796     5
```

```r
all.equal(a, b, d)
```

```
## [1] TRUE
```

```r
print(sumLoops <- round(summary(mb)[[4]][1]/min(summary(mb)[[4]][3]), 0))
```

```
## [1] 340
```


The vectorized function is ***340x*** faster.

## High level functionality

R also has numerous high level functions and packages that allow users to do a diversity of analyses and data manipulations, from GIS to MCMC to optimally stored file-based object storing for fast access (ff package), and much more.  Here are a few examples.

### GIS

