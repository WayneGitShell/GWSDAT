\name{write.linelistShape}
\alias{write.linelistShape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a arc-type shapefile}
\description{
  The function calls code from shapelib to write an arc-type shapefile from a list of matrices of line coordinates with no NAs.
}
\usage{
write.linelistShape(linelist, df, file, factor2char = TRUE,
 strictFilename=FALSE, max_nchar=254)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{linelist}{a list of matrices of line coordinates}
  \item{df}{a data frame object}
  \item{file}{a file name of maximum 8 chararacters, numbers or the underscore symbol to be written, omitting the extensions *.shp, *.shx and *.dbf, which are added in the function}
  \item{factor2char}{logical, default TRUE, convert factor columns to character}
  \item{strictFilename}{if TRUE, impose file basename length limit of 8 characters}
  \item{max_nchar}{default 254, may be set to a higher limit and passed through to the DBF writer, please see Details in \code{\link[foreign]{write.dbf}}}
}
\details{
  The function calls code from shapelib to write an arc-type shapefile (both the geometry file with a *.shp extension, the index file with a *.shx extension, and the database file with a *.dbf extension - see \code{\link[foreign]{write.dbf}} for details of the data frame export within this function.
}
\value{
  no return value.
}
\references{\url{http://shapelib.maptools.org/}}
\author{Nicholas J. Lewin-Koh, modified by Roger Bivand; shapelib by Frank Warmerdam}
\note{From maptools 0.4-10, this function is placed in the user-visible namespace on a trial basis, and reports of any malfunction should be sent to the package maintainer, Roger Bivand \email{Roger.Bivand@nhh.no}. It is likely that this function and its arguments will be changed.}

\seealso{\code{\link{write.pointShape}}, \code{\link[foreign]{write.dbf}}}
\examples{
x <- 10 * 1:nrow(volcano)
y <- 10 * 1:ncol(volcano)
line.list <- contourLines(x, y, volcano)
vol.levels <- data.frame(alt=sapply(line.list, function(x) x[[1]]))
vol.ll <- lapply(line.list, function(x) cbind(x$x, x$y))
for (i in seq(along=vol.ll)) {
  attr(vol.ll[[i]], "nParts") <- as.integer(1)
  attr(vol.ll[[i]], "pstart") <- list(as.integer(1),
    as.integer(nrow(vol.ll[[i]])))
}
tmpshp <- paste(tempdir(), "volcano", sep="/")
write.linelistShape(vol.ll, vol.levels, file=tmpshp)
try1 <- readShapeLines(tmpshp)
plot(try1)
}
\keyword{spatial}
