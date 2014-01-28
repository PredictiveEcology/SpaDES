\name{to_extract_unique_pixels_on_line}
\alias{to_extract_unique_pixels_on_line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract unique pixels on pathways around individuals
}
\description{
Give the pixels IDs (from a raster of the world) for the individual pathways. Pathways are lines going from the individual position to an ending points which are the unique pixels on the circle with the center as the individual position and a given radius.
}
\usage{
to_extract_unique_pixels_on_line(positions, next_possible_locations, distances, raster_world, scale_raster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{positions}{
%%     ~~Describe \code{positions} here~~
}
  \item{next_possible_locations}{
%%     ~~Describe \code{next_possible_locations} here~~
}
  \item{distances}{
%%     ~~Describe \code{distances} here~~
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
function (positions, next_possible_locations, distances, raster_world, 
    scale_raster) 
{
    seq_num_ind <- seq_len(nrow(positions))
    num_lines <- as.numeric(unlist(lapply(next_possible_locations, 
        nrow)))
    points_needed <- as.numeric(ceiling((distances/scale_raster) * 
        3) + 1)
    repeat_time <- num_lines * points_needed
    rep_points_by_line <- rep(points_needed, num_lines)
    ids <- rep(seq_num_ind, repeat_time)
    xs <- rep(positions[, 1], repeat_time)
    ys <- rep(positions[, 2], repeat_time)
    xe <- rep(as.data.frame(rbindlist(next_possible_locations))[, 
        1], rep_points_by_line)
    ye <- rep(as.data.frame(rbindlist(next_possible_locations))[, 
        2], rep_points_by_line)
    heading <- atan2(x = xe - xs, y = ye - ys)
    dist_line <- sqrt((as.data.frame(rbindlist(next_possible_locations))[, 
        1] - rep(positions[, 1], num_lines))^2 + (as.data.frame(rbindlist(next_possible_locations))[, 
        2] - rep(positions[, 2], num_lines))^2)
    increments_by_one <- rep(dist_line/(rep_points_by_line - 
        1), rep_points_by_line)
    increments_time <- unlist(rep(lapply(points_needed, function(b) 0:(b - 
        1)), num_lines))
    increments <- increments_by_one * increments_time
    new_x <- cos(heading) * increments + xs
    new_y <- sin(heading) * increments + ys
    coordinates_all_pts <- as.matrix(cbind(new_x, new_y))
    pixels_under_coordinates <- cellFromXY(raster_world, coordinates_all_pts)
    pixels_per_ind <- split(pixels_under_coordinates, ids)
    lines_ids <- split(rep(unlist(lapply(num_lines, function(c) seq_len(c))), 
        rep_points_by_line), ids)
    pixels_per_line_per_ind <- lapply(seq_num_ind, function(d) split(pixels_per_ind[[d]], 
        lines_ids[[d]]))
    pixels <- lapply(seq_num_ind, function(e) lapply(pixels_per_line_per_ind[[e]], 
        unique))
    return(pixels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
