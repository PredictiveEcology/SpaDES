\name{to_scale_prob}
\alias{to_scale_prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale probabilities so that they sum to 1
}
\description{
Given a list of vectors (one vector per individual) with different values, rescale these values so that they sum to 1 in each vector
}
\usage{
to_scale_prob(value_per_path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value_per_path}{
%%     ~~Describe \code{value_per_path} here~~
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
function (value_per_path) 
{
    seq_num_ind <- seq_len(length(value_per_path))
    prob_scaled <- list()
    prob_scaled[seq_num_ind] <- lapply(seq_num_ind, function(g) ifelse(rep(sum(value_per_path[[g]] != 
        0), length(value_per_path[[g]])), value_per_path[[g]]/sum(value_per_path[[g]]), 
        rep(1, length(value_per_path[[g]]))/length(value_per_path[[g]])))
    return(prob_scaled)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
