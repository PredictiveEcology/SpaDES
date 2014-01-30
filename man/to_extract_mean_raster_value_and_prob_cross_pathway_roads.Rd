\name{to_extract_mean_raster_value_and_prob_cross_pathway_roads}
\alias{to_extract_mean_raster_value_and_prob_cross_pathway_roads}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract raster information under pathways
}
\description{
Extract mean value from one raster for the unique pixels composing the individual pathways. Also calculate the probability of crossing the pathway based on the road presence raster.
}
\usage{
to_extract_mean_raster_value_and_prob_cross_pathway_roads(pathways, raster_world_mean, raster_world_cross, prob_cross_road)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathways}{
%%     ~~Describe \code{pathways} here~~
}
  \item{raster_world_mean}{
%%     ~~Describe \code{raster_world_mean} here~~
}
  \item{raster_world_cross}{
%%     ~~Describe \code{raster_world_cross} here~~
}
  \item{prob_cross_road}{
%%     ~~Describe \code{prob_cross_road} here~~
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
function (pathways, raster_world_mean, raster_world_cross, prob_cross_road) 
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
    values_raster_mean <- extract(raster_world_mean, all_unique_pixels)
    values_raster_cross <- extract(raster_world_cross, all_unique_pixels)
    unique_pixels_and_values <- cbind.data.frame(all_unique_pixels, 
        values_raster_mean, values_raster_cross)
    pixels_and_values <- merge(unique_pixels_and_values, all_pixels_ids, 
        by.x = "all_unique_pixels", by.y = "all_pixels", all = TRUE)
    pixels_and_values$ind_path_id <- as.factor(paste(pixels_and_values$ind_ids_per_pixels, 
        pixels_and_values$pathway_ids_per_pixels, sep = "."))
    mean_value <- tapply(pixels_and_values$values_raster_mean, 
        pixels_and_values$ind_path_id, mean)
    sum_value <- tapply(pixels_and_values$values_raster_cross, 
        pixels_and_values$ind_path_id, sum)
    results_value_df <- cbind.data.frame(mean_value, sum_value)
    results_value_df$prob_cross <- prob_cross_road^results_value_df$sum_value
    results_value_df$ind_ids <- rownames(results_value_df)
    full_ids <- cbind.data.frame(ids, ids_path, paste(ids, ids_path, 
        sep = "."))
    colnames(full_ids)[3] <- "ind_ids"
    results_value_df_ids <- merge(results_value_df, full_ids)
    results_value_df_ordered <- results_value_df_ids[with(results_value_df_ids, 
        order(ids_path)), ]
    mean_split_ind <- split(results_value_df_ordered$mean_value, 
        results_value_df_ordered$ids)
    cross_split_ind <- split(results_value_df_ordered$prob_cross, 
        results_value_df_ordered$ids)
    return(list(mean_value_raster = mean_split_ind, cross_value_raster = cross_split_ind))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
