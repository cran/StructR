\name{ElliRead}
\alias{ElliRead}

\title{Ellipsoid2003 output visualizer
}
\description{
This highly specific function reads in a data file export from the program Ellipsoid2003 and plots a series of two and three dimensional graphs. These graphs include a stereographic projection of axial orientations, interactive 3D oriented ellipsoid within the ``rgl'' environment, and a Nadai diagram.
}
\usage{
ElliRead(elli.file)
}
\arguments{
  \item{elli.file}{
The path the Ellipsoid2003 output file. 
}
}
\details{
This function incorporates numerous aspects of other packages to produce the standardized graphs including an arc plotting function modified from the package: ``plotrix.''
}
\value{
\item{comp1}{An oriented fabric ellipsoid in the ``rgl'' environment.}
\item{comp2}{A standardized stereographic projection of ellipsoid axial orientations.}
\item{comp3}{A Nadai diagram with table containing the Lode's parameter and the Octahedral shear strain.}
}
\references{
Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.\\

Webber, J.R. (2012) Kinematic reconstruction of magmatic flow and subsolidus deformation within the Coastal Batholith, central Chile. M.S. thesis, The University of Vermont.\\

Launeau, P. and Robin, P.Y.F. (2005) Determination of fabric and strain ellipsoids from measured sectional ellipses-implementation and applications. Journal of structural geology, 27(12): 2223-2233
}
\author{Jeffrey R. Webber
}
\note{
See Webber, J.R. 2012 or contact the author for specific example usage.
}

\seealso{
\code{\link{Nadai}}, \code{\link{ElliWrite}}
}
