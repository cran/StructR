\name{Nadai}
\alias{Nadai}
\title{
Nadai graph plotter
}
\description{
Plots a Nadai diagram of a fabric ellipsoid.
}
\usage{
Nadai(Lx = 2, Ly = 1, Lz = 0.5, Plot = TRUE)
}
\arguments{
  \item{Lx}{
Normalized length of the fabric ellipsoid x-axis.
}
  \item{Ly}{
Normalized length of the fabric ellipsoid y-axis.
}
  \item{Lz}{
Normalized length of the fabric ellipsoid z-axis.
}
  \item{Plot}{
Either TRUE or FALSE. False will not plot the diagram.
}
}
\details{
Implements a slightly modified function from the package ``plotrix.''
}
\value{
\item{comp1 }{Plot of the Nadai diagram.}
\item{comp2 }{If \code{Plot == FALSE}, returns a two column data frame object containing the Octahedral shear strain and the Lode's parameter.}
}
\references{
Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.\\

Nadai, (1963) A., Theory of fracture and flow of solids. McGraw-H111, New York
}
\author{
Jeffrey R. Webber
}

\seealso{
\code{\link{ElliRead}}
}
\examples{
Lx<-2.88
Ly<-0.72
Lz<-0.48

Nadai(Lx, Ly, Lz)

}
