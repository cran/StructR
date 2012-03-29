\name{FryImg}
\alias{FryImg}

\title{
Image based object selector and Fry generator
}
\description{
Imports an image and allows user to select object centroids.
}
\usage{
FryImg(img.path = NULL, limit = 3000)
}

\arguments{
  \item{img.path}{
A string specifying the path to image file.
}
  \item{limit}{
The maximum number of total data points returned (see details).
}
}
\details{
The total Fry coordinates will be limited based on \code{limit} determined by distance from origin thereby preserving the central void. 
}
\value{
Returns a two column data frame with Cartesian coordinates of generated Fry plot. 
}
\author{
Jeffrey R. Webber
}
\note{
Image files must be in .png format and have an aspect ratio of 1. Contact author for specific examples. 
}

\seealso{
\code{\link{FryPLOT}}, \code{\link{Fry3D}}
}
