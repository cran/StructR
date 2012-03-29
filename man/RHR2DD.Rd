\name{RHR2DD}
\alias{RHR2DD}
\title{Strike-dip to dip dip-direction
}
\description{
Converts planar orientation data in right hand rule convention to dip dip-direction format.
}
\usage{
RHR2DD(strike, dip)
}
\arguments{
  \item{strike}{
The strike of the planar feature in degrees.
}
  \item{dip}{
The dip of the planar feature in degrees.
}
}
\value{
Returns a two column object containing the dip and dip-direction respectively in degrees.
}

\author{
Jeffrey R. Webber
}

\seealso{
\code{\link{DD2RHR}}, \code{\link{ADD2RHR}}
}
\examples{
Strike<-sample(0:360, 8)
Dip<-sample(0:90, 8)

attDat<-NULL
i<-0
while(i < 8){
  i<-i + 1
  att<-RHR2DD(Strike[i], Dip[i])
  attDat<-rbind(attDat,att)
}

print(attDat)
}
