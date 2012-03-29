\name{FryPLOT}
\alias{FryPLOT}
\title{
Fry plot central void selector
}
\description{
Generates a sectional Fry plot and interactively allows user to fit an ellipse to the central void.
}
\usage{
FryPLOT(fry.data, zoom)
}

\arguments{
  \item{fry.data}{
A data frame object with two columns named ``x'' and ``y'' containing the Cartesian coordinates of a Fry plot.
}
  \item{zoom}{
Fry plot zoom factor.
}
}
\details{
\code{fry.data} is directly returned from the function \code{\link{FryImg}}.
}
\value{
Returns a three column data frame containing the fitted ellipse half long axis, half short axis, and the long axis rake in radians (see note).
}
\author{
Jeffrey R. Webber
}
\note{
Rake is measured as positive increasing counter-clock-wise from positive x axis. Values are between 0 and pi.
}


\seealso{
\code{\link{FryImg}}, \code{\link{FryPLOT}}
}
\examples{
\dontrun{
xcoord<-runif(1, -5, 5)
ycoord<-runif(1, -5, 5)
conPar <- data.frame(xcoord, ycoord)
names(conPar)=c("xcoord","ycoord")

i <- 0
while(i < 100){
	i <- i + 1
	TEST <- (-1)
		while(TEST < 0){
		i.i <- i + 1
		xTest <- runif(1, -5, 5)
		yTest <- runif(1, -5, 5)
		DIS <- sqrt((xTest - conPar$xcoord)^2 + (yTest - conPar$ycoord)^2)
		BUF <- .75
		DIFF <- (DIS - BUF)
		TEST <- min(DIFF)
		}
	newPar<-data.frame(xTest,yTest)
	names(newPar)=c("xcoord","ycoord")
	conPar<-rbind(conPar,newPar)
	names(conPar)=c("xcoord","ycoord")
}
x <- conPar$x * .75
y <- conPar$y * .5

#Run loop to create Fry coords.#
x.fry <- NULL
y.fry <- NULL
i <- 0

while(i < 50){
i<-i+1

#Center points based on iterative step [i] location=(0,0)#
x.raw <- x[i] - x
y.raw <- y[i] - y

#Concatenate coord. lists#
x.fry <- c(x.fry,x.raw)
y.fry <- c(y.fry,y.raw)
}
coords.fry <- data.frame(x.fry, y.fry)

plot.new()
test.int <- dev.interactive()
if(test.int == TRUE){
	FryPLOT(coords.fry)
}
if(test.int == FALSE){
	print("Device not interactive")
}

}
}