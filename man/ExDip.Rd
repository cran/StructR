\name{ExDip}
\alias{ExDip}
\title{Exaggerated dip calculator
}
\description{
Calculates the dip of a line in a vertically exaggerated cross section.
}
\usage{
ExDip(vert.ex = 2, dip = 45)
}
\arguments{
  \item{vert.ex}{
The factor of vertical exaggeration.
}
  \item{dip}{
The apparent dip (may be true in specific cases) of a plane trace in cross section.
}
}
\details{
\code{vert.ex} is calculated by dividing the denominator of the horizontal axis rational scale by the denominator of the vertical axis rational scale.
}
\value{
Returns a numeric value in degrees of the exaggerated dip.
}
\author{
Jeffrey R. Webber
}

\seealso{
\code{\link{AppDip}}
}
\examples{
v.scale<-(1 / 10000)
h.scale<-(1 / 15000)
v.exaggeration<-(v.scale / h.scale)
Dip<-sample(0:90, 1)

ExDip(v.exaggeration, Dip)
}
