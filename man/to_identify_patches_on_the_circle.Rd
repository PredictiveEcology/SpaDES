\name{to_identify_patches_on_the_circle}
\alias{to_identify_patches_on_the_circle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify unique pixels around individuals
}
\description{
Given individual current positions and distances for their next movements, identifies the unique pixels, from the raster composing the world, all around each individual at the given distance. These pixels are located on the circle with the individual position as the center and the next movement distance as the radius.
}
\usage{
to_identify_patches_on_the_circle(positions, buffers, raster_world, scale_raster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{positions}{
%%     ~~Describe \code{positions} here~~
}
  \item{buffers}{
%%     ~~Describe \code{buffers} here~~
}
  \item{raster_world}{
%%     ~~Describe \code{raster_world} here~~
}
  \item{scale_raster}{
%%     ~~Describe \code{scale_raster} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (positions, buffers, raster_world, scale_raster) 
{
    seq_num_ind <- seq_len(nrow(positions))
    n.angles <- (ceiling(buffers/scale_raster) + 1) * 16
    ids <- rep(seq_num_ind, times = n.angles)
    rads <- rep(buffers, times = n.angles)
    xs <- rep(positions[, 1], times = n.angles)
    ys <- rep(positions[, 2], times = n.angles)
    nvs <- rep(c(0, n.angles[-length(n.angles)]), times = n.angles)
    angle.inc <- rep(2 * pi, length(n.angles))/n.angles
    angs <- rep(angle.inc, times = n.angles)
    dnvs <- c(0, diff(ids))
    nvs[dnvs == 0] = 0
    nvs2 <- cumsum(nvs)
    cum <- 1:length(ids)
    index <- cum - nvs2 - 1
    angles <- angs * index
    x <- cos(angles) * rads + xs
    y <- sin(angles) * rads + ys
    coordinates_all_ind <- as.matrix(cbind(x, y))
    pixels_under_coordinates <- cellFromXY(raster_world, coordinates_all_ind)
    pixels_ind_ids <- cbind.data.frame(ids, pixels_under_coordinates)
    unique_pixels_values <- extract(raster_world, unique(pixels_under_coordinates))
    pixels_values_ids <- cbind.data.frame(unique(pixels_under_coordinates), 
        unique_pixels_values)
    coordinates_unique_pixels_noNA <- xyFromCell(raster_world, 
        pixels_values_ids[!is.na(pixels_values_ids[, 2]), 1])
    pixels_values_ids$coordinates_unique_pixels_noNA_x <- coordinates_unique_pixels_noNA[, 
        1]
    pixels_values_ids$coordinates_unique_pixels_noNA_y <- coordinates_unique_pixels_noNA[, 
        2]
    pixels_ind_ids_merged <- unique(merge(pixels_ind_ids, pixels_values_ids, 
        by.x = "pixels_under_coordinates", by.y = "unique(pixels_under_coordinates)", 
        all = TRUE))
    coord_unique_pixels <- split(pixels_ind_ids_merged[, c(4, 
        5)], pixels_ind_ids_merged[, 2])
    return(coord_unique_pixels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
