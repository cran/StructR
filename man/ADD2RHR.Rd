\name{ADD2RHR}
\alias{ADD2RHR}
\title{Strike, dip, and dip direction to right hand rule conversion
}
\description{
Converts strike, dip, and dip direction to strike and dip following right hand rule convention.
}
\usage{
ADD2RHR(strike, dip, direction)
}
\arguments{
  \item{strike}{
The strike azimuth in degrees from 0 to 360.
}
  \item{dip}{
The dip angle in degrees from 0 to 90.
}
  \item{direction}{
The dip direction quadrant of class "character" and a length of one (e.g. ``N'', ``n'', ``S'', ``s'', ``E'', ``e'', ``W'', ``w'').
}
}
\details{
Direction is not case sensitive; however, it must be of length one. Strings such as ``NW" will result in a return of ``NA.''
}
\value{
Returns a two column object consisting of the strike azimuth and dip respectively. Incompatible dip directions will return ``NA.''
}
\author{
Jeffrey R. Webber
}
\seealso{\code{\link{RHR2DD}}, \code{\link{DD2RHR}}
}
\examples{
Strike<-sample(0:360, 8)
Dip<-sample(0:90, 8)
Direction<-c("n","N","e","E","s","S","w","W")
Direction<-Direction[order(sample(1:8, 8))]

attDat<-NULL
i<-0
while(i < 8){
  i<-i + 1
  att<-ADD2RHR(Strike[i], Dip[i], Direction[i])
  attDat<-rbind(attDat,att)
}
print(attDat)
}
