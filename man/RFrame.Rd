\name{RFrame}
\alias{RFrame}
\title{
Reference frame
}
\description{
Adds a standardized geographic reference frame to plot in ``rgl.''
}
\usage{
RFrame(color = "#DDDDDD", lweight = 0.5)
}
\arguments{
  \item{color}{
Color of text and \code{box3d} frame.
}
  \item{lweight}{
Line weight of the \code{box3d} frame.
}
}
\details{
Use after objects are plotted.
}
\value{
Plotted reference frame
}
\author{
Jeffrey R. Webber
}

\seealso{
\code{\link{Fry3D}}, \code{\link{ElliRead}}, \code{\link{Ellipse3D}}
}
\examples{
func<-function(x,y,z) x^2+y^2+z^2
x<-seq(-2,2,len=20)
S0<-contour3d(func,4,x,x,x,draw=FALSE)

obj<-updateTriangles(S0)
drawScene.rgl(obj)
RFrame()
}
