\name{Att3P}
\alias{Att3P}

\title{Three point problem
}
\description{
Calculates the attitude of a dipping plane from the coordinates of three coplanar points.
}
\usage{
Att3P(coords)
}
\arguments{
  \item{coords}{
Either a 3 by 3 matrix or data frame containing the X, Y, and Z coordinates of three coplanar points, as columns, in respective order (rows denote subsequent points). 
}
}
\details{
Each coordinate must be of the same unit in linear distance and not angular distance (i.e. not in latitude and longitude). 
}
\value{
Returns a two column object consisting of the strike azimuth and dip respectively.
}

\author{
Jeffrey R. Webber
}

\note{
Coordinates may be obtained from Google Earth with UTM coordinates and elevations in meters.
}

\seealso{
\code{\link{kmlDPP}}
}
\examples{
coord.m <- matrix(rbind(c(0, 0, 1600), c(860, -500, 2100),
                  c(460, 470, 1900)), ncol = 3)

Att3P(coord.m)
}

