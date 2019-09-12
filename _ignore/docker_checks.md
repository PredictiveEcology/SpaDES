# Running `R CMD check` on this package using Docker

## R-oldrel

```bash
docker pull rocker/geospatial:3.5.3
docker run -it --rm -v ~/GitHub/PredictiveEcology/SpaDES:/SpaDES rocker/geospatial:3.5.3 bash -c "apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev xvfb && xvfb-run R"
```

```r
setwd("SpaDES")
install.packages(c("devtools", "tkrplot"))
library(devtools)
install_dev_deps()
check(args = c("--as-cran"))
```

## R-release

```bash
docker pull rocker/geospatial:latest
docker run -it --rm -v ~/GitHub/PredictiveEcology/SpaDES:/SpaDES rocker/geospatial bash -c "apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev xvfb && xvfb-run R"
```

```r
setwd("SpaDES")
install.packages(c("devtools", "tkrplot"))
library(devtools)
install_dev_deps()
check(args = c("--as-cran"))
```

## R-devel

```bash
docker pull rocker/drd
docker run -it --rm -v ~/GitHub/PredictiveEcology/SpaDES:/SpaDES rocker/drd
```

```r
setwd("SpaDES")
install.packages("devtools")
library(devtools)
install_dev_deps()
check(args = c("--as-cran"))
```

