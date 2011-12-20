\name{lMOD}
\alias{lMOD}
\title{
Lineation modeler 
}
\description{
Plots lineations within the ``rgl'' environment.
}
\usage{
lMOD(trend = 0, plunge = 0, x.loc = 0, y.loc = 0, z.loc = 0,
     scale.factor = 1, lweight = 1, color = "blue")
}
\arguments{
  \item{trend}{
The trend of the lineation in degrees.
}
  \item{plunge}{
The plunge of the lineation in degrees.
}
  \item{x.loc}{
X-coordinate of the lineation center location.
}
  \item{y.loc}{
Y-coordinate of the lineation center location.
}
  \item{z.loc}{
Z-coordinate of the lineation center location.
}
  \item{scale.factor}{
The factor to which the line length is scales (defaults to 1).
}
  \item{lweight}{
The line weight to draw.
}
  \item{color}{
Color of the plotted line.
}
}
\details{
Use \code{\link{RFrame}} to add a reference frame to plot.
}
\value{
Returns plot of oriented lineation.
}
\author{
Jeffrey R. Webber
}
\seealso{
\code{\link{pMOD}}, \code{\link{RFrame}}
}
\examples{
trend <- sample(0:360, 1)
dip <- sample(0:90, 1)
x.loc <- sample(-15:15, 1)
y.loc <- sample(-15:15, 1)
z.loc <- sample(-15:15, 1)

lMOD(trend, dip, x.loc, y.loc, z.loc)
}
