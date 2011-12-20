\name{AppDip}
\alias{AppDip}
\title{Apparent dip calculator
}
\description{
Calculates the apparent dip of a plane in a vertical section.
}
\usage{
AppDip(acute = 90, dip = 45)
}
\arguments{
  \item{acute}{
The acute angle in degrees between the line of section and the strike of the plane.
}
  \item{dip}{
The true dip of the plane.
}
}
\details{
Does not provide directionality, which must be determined based on the dip direction.
}
\value{
Returns the apparent dip in degrees.
}

\author{
Jeffrey R. Webber
}

\seealso{
\code{\link{ExDip}}
}
\examples{
acute.angle <- sample(0:90, 1)
true.dip <- sample(0:90, 1)

AppDip(acute.angle, true.dip)
}
