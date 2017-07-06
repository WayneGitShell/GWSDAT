\name{read.shape}
\alias{read.shape}
\alias{getinfo.shape}
\alias{print.shapehead}

\title{Read shapefile into Map object}
\description{
  Read shapefile into Map object; the file should be given including its ".shp" extension, and the function will reconstruct the names of the database (dbf) file and the index (shx) file from these.
}
\usage{
read.shape(filen, dbf.data = TRUE, verbose=TRUE, repair=FALSE)
getinfo.shape(filen)
\method{print}{shapehead}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filen}{name of file with *.shp extension}
  \item{dbf.data}{read DBF data together with shapes, default TRUE}
  \item{verbose}{default TRUE --- report type of shapefile and number of shapes}
  \item{repair}{default FALSE: some shapefiles provided by Geolytics Inc. have values of object sizes stored in the *.shx index file that are eight bytes too large, leading the function to try to read past the end of file. If repair=TRUE, an attempt is made to repair the internal values, permitting such files to be read.}
  \item{x}{a shapehead list as returned by getinfo.shape}
  \item{...}{other arguments passed to print}
}
\details{
  The function calls code from shapelib to read shapefiles, a file format used by ESRI GIS software among others
}
\value{
read.shape() returns either a list of shapes of class ShapeList, or if dbf.data = TRUE a Map object with:
  \item{Shapes}{a list of shapes of class ShapeList; both the individual shapes and the list have attributes}
  \item{att.data}{a data frame of data from the associated DBF file; note that the field names are adjusted to use in R using \code{make.names()}, and so will permit the underscore character from R release 1.9.0.}
}
\references{\url{http://shapelib.maptools.org/}}
\author{Nicholas J. Lewin-Koh, modified by Roger Bivand \email{Roger.Bivand@nhh.no}; shapelib by Frank Warmerdam}

\seealso{\code{\link{plot.Map}}, \code{\link[foreign]{read.dbf}}}
\examples{
x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
length(x$Shapes)
unlist(lapply(x$att.data, class))
str(getinfo.shape(system.file("shapes/fylk-val.shp", package="maptools")[1]))
}
\keyword{spatial}

