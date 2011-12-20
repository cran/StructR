\name{pMOD}
\alias{pMOD}

\title{Plane modeler 
}
\description{
Models the orientations of planes in the ``rgl'' environment.
}
\usage{
pMOD(strike = 0, dip = 45, x.loc = 0, y.loc = 0, z.loc = 0,
     n.faces = 32, scale.factor = 1, color = "orange")
}
\arguments{
  \item{strike}{
The strike of the plane in degrees.
}
  \item{dip}{
The dip of the plane in degrees.
}
  \item{x.loc}{
X-coordinate of the plane center location.
}
  \item{y.loc}{
Y-coordinate of the plane center location.
}
  \item{z.loc}{
Z-coordinate of the plane center location.
}
  \item{n.faces}{
The number of edges to approximate the plane.
}
  \item{scale.factor}{
The factor to proportionately scale the plane.
}
  \item{color}{
The color to plot the plane.
}
}
\details{
Use \code{\link{RFrame}} to add a reference frame to plot.
}
\value{
Returns plot of oriented plane.
}
\author{
Jeffrey R. Webber
}
\seealso{
\code{\link{lMOD}}, \code{\link{RFrame}}
}
\examples{
strike<-sample(0:360, 15)
dip<-sample(0:90, 15)
x.loc<-sample(-15:15, 15)
y.loc<-sample(-15:15, 15)
z.loc<-sample(-15:15, 15)

pMOD(strike, dip, x.loc, y.loc, z.loc, color = rainbow(25))
}

