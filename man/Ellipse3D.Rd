\name{Ellipse3D}
\alias{Ellipse3D}

\title{
Three dimensional ellipse plotting
}
\description{
Adds an ellipse to an existing three dimensional plot.
}
\usage{
Ellipse3D(hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0,
          strike = 0, dip = 0, npoints = 50, sty = "red")
}

\arguments{
  \item{hlaxa}{
Half-length of the major axis (parallel to the X-axis when theta = 0).
}
  \item{hlaxb}{
Half-length of the minor axis.
}
  \item{theta}{
Angle of rotation, measured counter-clockwise from the positive X-axis.
}
  \item{xc}{
The X-coordinate of the ellipse center.
}
  \item{yc}{
The Y-coordinate of the ellipse center.
}
  \item{strike}{
The azimuth strike of the section that contains the ellipse. 
}
  \item{dip}{
the dip of the section that contains the ellipse. 
}
  \item{npoints}{
The number of line segments used to plot the ellipse.
}
  \item{sty}{
The line color of the ellipse.
}
}
\details{
The angle of the ellipse is in radians not degrees (this is a vestigial feature). The \code{strike} and \code{dip} are in degrees to better integrate with standard geological data. 
}
\value{
Adds an oriented ellipse to an existing plot using the package: ``rgl.''
}
\references{
http://www.math.mcmaster.ca/peter/s4m03/s4m03_0304/classnotes/ellipse.html Drawing and ellipse in Splus or R.
}
\author{
Jeffrey R. Webber (modified from code written by Peter Macdonald).
}
\note{
This does not plot an ellipsoid, but rather an ellipse within an oriented three dimensional section.
}

\seealso{
\code{\link{Ellipse2D}}
}
\examples{
rat <- sample(1:5, 5)
aha <- sqrt(rat)
bha <- sqrt(1 / rat)
xco <- runif(5, -1, 1)
yco <- runif(5, -1, 1)
ang <- runif(5, 0, pi)

str <- sample(0:360, 5)
dip <- sample(0:90, 5)

open3d()

i <- 0

while(i < 5){
	i <- i + 1
	Ellipse3D(aha[i], bha[i], ang[i], xco[i], yco[i], str[i], dip[i])
}

}
