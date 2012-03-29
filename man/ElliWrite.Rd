\name{ElliWrite}
\alias{ElliWrite}
\title{Ellipsoid2003 input file writer
}
\description{
Creates a file that can be loaded into the program Ellipsoid2003 for statistical ellipse fitting.
}
\usage{
ElliWrite(path, section.a, section.b, section.c,
          ort.a, ort.b, ort.c)
}
\arguments{
  \item{path}{
A string containing the file location to write.
}
  \item{section.a}{
A three column data frame object specific to an oriented section containing Fry plot data (see details below).
}
  \item{section.b}{
A three column data frame object specific to an oriented section containing Fry plot data (see details below).
}
  \item{section.c}{
A three column data frame object specific to an oriented section containing Fry plot data (see details below).
}
  \item{ort.a}{
A numeric vector with two cells containing the strike and dip (in degrees) of the section corresponding to \code{section.a}.
}
  \item{ort.b}{
A numeric vector with two cells containing the strike and dip (in degrees) of the section corresponding to \code{section.b}.
}
  \item{ort.c}{
A numeric vector with two cells containing the strike and dip (in degrees) of the section corresponding to \code{section.c}.
}
}
\details{
The \code{section.x} data frames are returned directly from \code{\link{FryPLOT}}.
}
\value{
A ``.elli'' file of sectional data that can be read directly into the program Ellipsoid2003.
}
\references{
Webber, J.R. (2012) Kinematic reconstruction of magmatic flow and subsolidus deformation within the Coastal Batholith, central Chile. M.S. thesis, The University of Vermont.\\

Launeau, P. and Robin, P.Y.F. (2005) Determination of fabric and strain ellipsoids from measured sectional ellipses-implementation and applications. Journal of structural geology, 27(12): 2223-2233

}
\author{
Jeffrey R. Webber
}
\note{
Function will automatically overwrite existing files with conflicting names. See Webber, J.R. 2012 or contact the author for specific example usage.
}

\seealso{
\code{\link{ElliRead}}, \code{\link{FryPLOT}}
}