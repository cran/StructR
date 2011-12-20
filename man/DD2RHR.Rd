\name{DD2RHR}
\alias{DD2RHR}

\title{
Dip dip direction to azimuth and dip
}
\description{
Converts orientations in dip dip direction convention to strike azimuth and dip following right hand rule convention.
}
\usage{
DD2RHR(dip, direction)
}

\arguments{
  \item{dip}{
The dip angle in degrees from 0 to 90.
}
  \item{direction}{
The dip direction azimuth in degrees from 0 to 360.
}
}

\value{
Returns a two column object consisting of the strike azimuth and dip respectively.
}

\author{
Jeffrey R. Webber
}

\seealso{\code{\link{RHR2DD}}, \code{\link{ADD2RHR}}
}
\examples{
Dip<-sample(0:90, 8)
Direction<-sample(0:360, 8)

attDat<-NULL
i<-0
while(i < 8){
  i<-i + 1
  att<-DD2RHR(Dip[i], Direction[i])
  attDat<-rbind(attDat,att)
}

print(attDat)
}
