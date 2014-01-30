\name{to_extract_mean_raster_value}
\alias{to_extract_mean_raster_value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract mean pixel raster value on pathways
}
\description{
Extract the mean value from a raster on each individual pathways. Pathways are unique pixels on the lines going from the individual current position to any of the unique pixels located on the circle the individual position as the center of the circle and a given distance for the radius.
}
\usage{
to_extract_mean_raster_value(pathways, raster_world)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathways}{
%%     ~~Describe \code{pathways} here~~
}
  \item{raster_world}{
%%     ~~Describe \code{raster_world} here~~
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
function (pathways, raster_world) 
{
    seq_num_ind <- seq_len(length(pathways))
    num_pathways <- unlist(lapply(pathways, length))
    length_pathways <- as.vector(unlist(lapply(seq_num_ind, function(f) lapply(pathways[[f]], 
        length))))
    ids <- rep(seq_num_ind, num_pathways)
    ind_ids_per_pixels <- rep(ids, length_pathways)
    ids_path <- unlist(lapply(num_pathways, seq_len))
    pathway_ids_per_pixels <- rep(ids_path, length_pathways)
    all_pixels <- as.vector(unlist(pathways))
    all_pixels_ids <- cbind.data.frame(ind_ids_per_pixels, pathway_ids_per_pixels, 
        all_pixels)
    all_unique_pixels <- unique(all_pixels)
    values_raster <- extract(raster_world, all_unique_pixels)
    unique_pixels_and_values <- cbind.data.frame(all_unique_pixels, 
        values_raster)
    pixels_and_values <- merge(unique_pixels_and_values, all_pixels_ids, 
        by.x = "all_unique_pixels", by.y = "all_pixels", all = TRUE)
    pixels_and_values$ind_path_id <- as.factor(paste(pixels_and_values$ind_ids_per_pixels, 
        pixels_and_values$pathway_ids_per_pixels, sep = "."))
    mean_value <- tapply(pixels_and_values$values_raster, pixels_and_values$ind_path_id, 
        mean)
    mean_value_df <- as.data.frame(mean_value)
    mean_value_df$ind_ids <- rownames(mean_value_df)
    full_ids <- cbind.data.frame(ids, ids_path, paste(ids, ids_path, 
        sep = "."))
    colnames(full_ids)[3] <- "ind_ids"
    mean_value_ids <- merge(mean_value_df, full_ids)
    mean_value_ordered <- mean_value_ids[with(mean_value_ids, 
        order(ids_path)), ]
    values_split_ind <- split(mean_value_ordered$mean_value, 
        mean_value_ordered$ids)
    return(values_split_ind)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
