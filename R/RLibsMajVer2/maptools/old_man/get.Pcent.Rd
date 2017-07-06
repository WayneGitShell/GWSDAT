\name{get.Pcent}
\alias{get.Pcent}
\title{Polygon centroids}
\description{
  return the centroids of a map of polygons
}
\usage{
get.Pcent(theMap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theMap}{a Map object returned by read.shape()}
}

\value{
 a matrix of centroids.
}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand \email{Roger.Bivand@nhh.no}}

\examples{
x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
get.Pcent(x)
}
\keyword{spatial}

