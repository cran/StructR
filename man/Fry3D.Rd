\name{Fry3D}
\alias{Fry3D}

\title{
Three dimensional fry plotter
}
\description{
Plots coordinates of a Fry plot in three dimensional space from an oriented section.
}
\usage{
Fry3D(fry.data, strike = 0, dip = 0, color = "black")
}

\arguments{
  \item{fry.data}{
A data frame object with two columns named ``x'' and ``y'' containing the Cartesian coordinates of a Fry plot.
}
  \item{strike}{
The strike of the oriented section in degrees.
}
  \item{dip}{
The dip of the oriented section in degrees.
}
  \item{color}{
Color of points to plot.
}
}
\details{
\code{fry.data} is directly returned from the function \code{\link{FryImg}}.
}
\value{
Adds 3D fry points to an ``rgl'' environment.
}
\author{
Jeffrey R. Webber
}


\seealso{
\code{\link{FryPLOT}}, \code{\link{FryImg}}
}
\examples{
p.x <- runif(100, -10, 10)
p.y <- runif(100, -10, 10)
fry <- data.frame(p.x, p.y)
names(fry) = c("x", "y")
str <- sample(0:360, 1)
dip <- sample(0:90, 1)

Fry3D(fry, str, dip)
}
