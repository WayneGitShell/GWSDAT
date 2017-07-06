\name{write.pointShape}
\alias{write.pointShape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a point-type shapefile}
\description{
  The function calls code from shapelib to write a point-type shapefile.
}
\usage{
write.pointShape(coordinates, df, file, factor2char = TRUE,
 strictFilename=FALSE, max_nchar=254)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coordinates}{a 2-column numeric matrix of coordinates}
  \item{df}{a data frame object}
  \item{file}{a file name of maximum 8 chararacters, numbers or the underscore symbol to be written, omitting the extensions *.shp, *.shx and *.dbf, which are added in the function}
  \item{factor2char}{logical, default TRUE, convert factor columns to character}
  \item{strictFilename}{if TRUE, impose file basename length limit of 8 characters}
  \item{max_nchar}{default 254, may be set to a higher limit and passed through to the DBF writer, please see Details in \code{\link[foreign]{write.dbf}}}
}

\details{
  The function calls code from shapelib to write a point-type shapefile (both the geometry file with a *.shp extension, the index file with a *.shx extension, and the database file with a *.dbf extension - see \code{\link[foreign]{write.dbf}} for details of the data frame export within this function.
}
\value{
  no return value.
}

\references{\url{http://shapelib.maptools.org/}}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand; shapelib by Frank Warmerdam}

\note{From maptools 0.4-7, this function is placed in the user-visible namespace on a trial basis, and reports of any malfunction should be sent to the package maintainer, Roger Bivand \email{Roger.Bivand@nhh.no}. It is likely that this function and its arguments will be changed.}

\seealso{\code{\link[foreign]{write.dbf}}}
\examples{
balt_orig <- readShapePoints(system.file("shapes/baltim.shp", package="maptools")[1])
plot(balt_orig)
balt_cheap <- balt_orig[balt_orig$PRICE < 40,]
file <- tempfile("")
write.pointShape(coordinates=coordinates(balt_cheap),
 df=as(balt_cheap, "data.frame"), file)
getinfo.shape(paste(file, ".shp", sep=""))
balt_new <- readShapePoints(paste(file, ".shp", sep=""))
plot(balt_new, col="red", pch=16, add=TRUE)
}
\keyword{spatial}

