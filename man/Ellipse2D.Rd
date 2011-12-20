\name{Ellipse2D}
\alias{Ellipse2D}

\title{Two dimensional ellipse plotting
}
\description{
Adds an ellipse to an existing two-dimensional plot.
}
\usage{
Ellipse2D(hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0,
          npoints = 50, sty = "red")
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
  \item{npoints}{
The number of line segments used to plot the ellipse.
}
  \item{sty}{
The line color of the ellipse.
}
}
\details{
Angle is in radians not degrees.
}
\value{
Adds ellipse to existing plot.
}
\references{
http://www.math.mcmaster.ca/peter/s4m03/s4m03_0304/classnotes/ellipse.html Drawing and ellipse in Splus or R.
}
\author{
Jeffrey R. Webber (modified from code written by Peter Macdonald)
}
\seealso{
\code{\link{Ellipse3D}}
}
\examples{
rat <- sample(1:5, 5)
aha <- sqrt(rat)
bha <- sqrt(1 / rat)
xco <- runif(5, -1, 1)
yco <- runif(5, -1, 1)
ang <- runif(5, 0, pi)

plot.new()
plot.window(c(-2.5, 2.5), c(-2.5, 2.5), asp = 1)

i <- 0

while(i < 5){
	i <- i + 1
	Ellipse2D(aha[i], bha[i], ang[i], xco[i], yco[i])
}
}
