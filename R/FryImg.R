FryImg <-
function(img.path = NULL, limit = 3000){
  #Plot image and create point locator#
  img <-readPNG(source = img.path)
  
  x11(xpos=25, ypos=0)
  par(mar=c(0,0,0,0))
  plot.new()
  rasterImage(img,0,0,1,1,interpolate=FALSE)
  coords <- locator(type="p",pch=20, col = "yellow")
  dev.off()

  #Run loop to create Fry coords.#
  x.fry <- NULL
  y.fry <- NULL
  i <- 0

  while(i < length(coords$x)){
    i<-i+1
    
    #Center points based on iterative step [i] location=(0,0)#
    x.raw<-coords$x[i] - coords$x
    y.raw<-coords$y[i] - coords$y
    
    #Concatenate coord. lists#
    x.fry <- c(x.fry,x.raw)
    y.fry <- c(y.fry,y.raw)
  }

  coords.fry <- data.frame(x.fry, y.fry)

  #Calculate center distance and limit data size#
  coords.fry$distance <- sqrt((coords.fry$x.fry)^2 + (coords.fry$y.fry)^2)
  coords.fry <- coords.fry[with(coords.fry, order(distance)), ]
  x <- coords.fry$x[(length(coords$x) + 1):length(coords.fry$x)]
  y <- coords.fry$y[(length(coords$x) + 1):length(coords.fry$x)]

  if( length(x) > limit){
    x <- x[1:limit]
    y <- y[1:limit]
  }
    
  coords.fry.lim <- data.frame(x, y)
  return(coords.fry.lim)
}

