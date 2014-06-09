\docType{methods}
\name{adj}
\alias{adj}
\title{Fast Adjacent function}
\usage{
adj(x = NULL, cells, directions = 8, pairs = TRUE, numCol = NULL,
  numCell = NULL)
}
\arguments{
  \item{x}{Raster* object for which adjacency will be
  calculated.}

  \item{cells}{vector of cell numbers for which adjacent
  cells should be found. Cell numbers start with 1 in the
  upper-left corner and increase from left to right and
  from top to bottom}

  \item{directions}{the number of directions in which cells
  should be connected: 4 (rook's case), 8 (queen's case),
  or 'bishop' to connect cells with one-cell diagonal
  moves. Or a neigborhood matrix (see Details)}

  \item{pairs}{logical. If TRUE, a matrix of pairs of
  adjacent cells is returned. If FALSE, a vector of cells
  adjacent to cells is returned}

  \item{numCol}{numeric indicating number of columns in the
  raster. Using this with numCell is a bit faster execution
  time.}

  \item{numCell}{numeric indicating number of cells in the
  raster. Using this with numCol is a bit faster execution
  time.}
}
\value{
a matrix of one or two columns, from and to.
}
\description{
Faster function for determining the cells of the 4 or 8
neighbours of the \code{cells}
}
\examples{
a <- raster(extent(0,1000,0,1000),res=1)
sam = sample(1:length(a),1e4)
numCol <- ncol(a)
numCell <- numCell(a)
adj.new <- adj(numCol,numCell,sam,directions=8)
print(head(adj.new))
}
\seealso{
\code{\link{adjacent}}
}

