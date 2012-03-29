\name{kmlDPP}
\alias{kmlDPP}
\title{
Google Earth down plunge projection
}
\description{
Creates a ``.kml'' file based that sets the view to a down plunge projection.
}
\usage{
kmlDPP(NAME = "Sandy Hollow", LAT = 45.45384, LON = -112.5655,
       DIST = 2500, TREND = 350, PLUNGE = 17, FILE = NULL)
}

\arguments{
  \item{NAME}{
The name of the fold.
}
  \item{LAT}{
The latitude of the fold nose (WGS 84).
}
  \item{LON}{
The longitude of the fold nose (WGS 84)
}
  \item{DIST}{
The distance of view from the ground in feet(?).
}
  \item{TREND}{
The trend of the fold axis in degrees.
}
  \item{PLUNGE}{
The plunge of the fold axis in degrees.
}
  \item{FILE}{
A string specifying the path to the output file.
}
}
\details{
In general, a \code{DIST} of 2500 works well, but you can always zoom in and out in GE.
}
\value{
Saves a file in KML format that can be opened in Google Earth.
}
\author{
Jeffrey R. Webber
}
\note{
For an example run the function with default variables and check the working directory for the output.
}
