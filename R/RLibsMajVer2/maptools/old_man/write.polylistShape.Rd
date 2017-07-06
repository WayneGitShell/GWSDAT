\name{write.polylistShape}
\alias{write.polylistShape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a polygon-type shapefile}
\description{
  The function calls code from shapelib to write a polygon-type shapefile from an S3 polylist object.
}
\usage{
write.polylistShape(polylist, df, file, factor2char = TRUE,
 strictFilename=FALSE, force = TRUE, max_nchar=254)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polylist}{list of polygons of class \code{polylist}}
  \item{df}{a data frame object}
  \item{file}{a file name of maximum 8 chararacters, numbers or the underscore symbol to be written, omitting the extensions *.shp, *.shx and *.dbf, which are added in the function}
  \item{factor2char}{logical, default TRUE, convert factor columns to character}
  \item{strictFilename}{if TRUE, impose file basename length limit of 8 characters}
  \item{force}{default TRUE, to try to force malformed polylist objects to some reasonable form that will not cause both maptools and R to crash. Because polylist objects are old-style rather than new-style classes, it is possible to crash R by trying to write malformed objects. Attempts are made to check for known problems, but using polylist objects rather than sp SpatialPolygons objects is known to be more risky. From release 0.6-14, polylist objects will be given an nDims attribute at this stage to permit the output of constructed 3D objects, and so report a warning, which should be ignored.}
  \item{max_nchar}{default 254, may be set to a higher limit and passed through to the DBF writer, please see Details in \code{\link[foreign]{write.dbf}}}
}
\details{
  The function calls code from shapelib to write a polygon-type shapefile (both the geometry file with a *.shp extension, the index file with a *.shx extension, and the database file with a *.dbf extension - see \code{\link[foreign]{write.dbf}} for details of the data frame export within this function.
}
\value{
  no return value.
}
\references{\url{http://shapelib.maptools.org/}}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand; shapelib by Frank Warmerdam}

\note{From maptools 0.4-7, this function is placed in the user-visible namespace on a trial basis, and reports of any malfunction should be sent to the package maintainer, Roger Bivand \email{Roger.Bivand@nhh.no}. It is likely that this function and its arguments will be changed.}

\seealso{\code{\link{write.pointShape}}, \code{\link[foreign]{write.dbf}}}
\examples{
col_orig <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
mappolys <- Map2poly(col_orig, as.character(col_orig$att.data$NEIGNO))
plot(mappolys)
col_df <- col_orig$att.data
col_cheap <- subset(mappolys, col_df$HOVAL < 34)
col_df_cheap <- subset(col_df, col_df$HOVAL < 34)
file <- tempfile("")
write.polylistShape(col_cheap, col_df_cheap, file)
getinfo.shape(paste(file, ".shp", sep=""))
col_new <- read.shape(paste(file, ".shp", sep=""))
mappolys <- Map2poly(col_new, as.character(col_new$att.data$NEIGNO))
plot(mappolys, border="red", add=TRUE)
}
\keyword{spatial}

