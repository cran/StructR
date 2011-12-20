ElliRead <-
function(elli.file){
  Lnorm  <- read.table(file = elli.file, skip=17,sep='\t',nrows=1)
  OrDat  <- read.table(file = elli.file, skip=20,sep='\t',nrows=2)
  FolDat <- read.table(file = elli.file, skip=23,sep='\t',nrows=1)
  
  XL<-Lnorm$V2
  YL<-Lnorm$V3
  ZL<-Lnorm$V4
  TX<-round(OrDat$V2[1],2)
  TY<-round(OrDat$V3[1],2)
  TZ<-round(OrDat$V4[1],2)
  PX<-round(OrDat$V2[2],2)
  PY<-round(OrDat$V3[2],2)
  PZ<-round(OrDat$V4[2],2)

  #Calculate roation of principle axes#
  RX<-ifelse(TX<=180,
    ifelse(TY>TX,
      ifelse(TY<=(TX+180),
        PY/cos(PX*(pi/180)),
        PY/cos(PX*(pi/180))*(-1)),
      PY/cos(PX*(pi/180))*(-1)),
    ifelse(TY<TX,
      ifelse(TY<=(TX-180),
        PY/cos(PX*(pi/180))*(-1),
        PY/cos(PX*(pi/180))),
      PY/cos(PX*(pi/180)))
    )
  RX<-round(RX,2)
  RY<-round(PX,2)
  RZ<-round(((-1)*TX+90),2)
  
  #Plot stereonet of oriented elliposid#
  net()
  XY<-faultplane(FolDat$V2,FolDat$V3,PLOT=FALSE)
  lines(XY,lwd=2)
  title("FRY")
  text(0,1.04,"N",cex=.8)
  lab1<-focpoint(TX,PX,pch=20,col="blue")
  lab2<-focpoint(TY,PY,pch=20,col="blue")
  lab3<-focpoint(TZ,PZ,pch=20,col="blue")
  info1<-paste("XY(S&D):",round(FolDat$V2,0),round(FolDat$V3,0),"; X(T&P):",round(TX,0),round(PX,0),"; Y(T&P):",round(TY,0),round(PY,0),"; Z(T&P):",round(TZ,0),round(PZ,0),sep=" ")
  mtext(info1,side=1,cex=.8)
  text(lab1,"X",col="blue",pos=3)
  text(lab2,"Y",col="blue",pos=3)
  text(lab3,"Z",col="blue",pos=3)

  #Create non-oriented fabric ellipsoid#
  f<-function(x,y,z) x^2+y^2+z^2
  x<-seq(-2,2,len=20)
  S0<-contour3d(f,4,x,x,x,draw=FALSE)
  S1<-scaleTriangles(triangles=S0,.25,.25,.25)
  S2<-scaleTriangles(triangles=S1,XL,YL,ZL)

  #Establish rotaion matricies#
  rotxM<-rotationMatrix((RX*(pi/180)),1,0,0)
  rotyM<-rotationMatrix((RY*(pi/180)),0,1,0)
  rotzM<-rotationMatrix((RZ*(pi/180)),0,0,1)

  #Run transformations#
  TRANS1<-transformTriangles(triangles=S2,rotxM)
  TRANS2<-transformTriangles(triangles=TRANS1,rotyM)
  TRANS3<-transformTriangles(triangles=TRANS2,rotzM)
  obj<-updateTriangles(TRANS3)

  #Generate ellipsoid#
  open3d()
  drawScene.rgl(obj)
  
  #Function for drawing arcs from package:plotrix#
  draw.arc <- function (x = 1, y = NULL, radius = 1, angle1 = deg1 * pi/180, angle2 = deg2 * pi/180, deg1 = 0, deg2 = 45, n = 35, col = 1, ...){
    draw.arc.0 <- function(x, y, radius, angle1, angle2, n, col = col, ...){
      xylim <- par("usr")
      plotdim <- par("pin")
      ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
      angle <- angle1 + seq(0, length = n) * (angle2 - angle1)/n
      p1x <- x + radius * cos(angle)
      p1y <- y + radius * sin(angle) * ymult
      angle <- angle1 + seq(length = n) * (angle2 - angle1)/n
      p2x <- x + radius * cos(angle)
      p2y <- y + radius * sin(angle) * ymult
      segments(p1x, p1y, p2x, p2y, col = col, ...)
    }
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    a1 <- pmin(angle1, angle2)
    a2 <- pmax(angle1, angle2)
    angle1 <- a1
    angle2 <- a2
    args <- data.frame(x, y, radius, angle1, angle2, n, col, stringsAsFactors = FALSE)
    for (i in 1:nrow(args)) do.call("draw.arc.0", c(args[i, ], ...))
    invisible(args)
  }
  
  #Set up data,frame of axial lengths#
  axDat<-data.frame(XL, YL, ZL)
  names(axDat)=c("X","Y","Z")

  #Calculate Lode's parameter#
  lodes<-(2*(log(axDat$Y)-log(axDat$X)-log(axDat$Z)))/(log(axDat$X)-log(axDat$Z))

  #Calculate the octahedral shear strain#
  octSS<-(sqrt((log(axDat$X)-log(axDat$Y))^2+(log(axDat$Y)-log(axDat$Z))^2+(log(axDat$Z)-log(axDat$X))^2)/sqrt(3))

  #Calculate cartesian coords#
  xcoord<-octSS*sin((pi/6)*lodes)
  ycoord<-octSS*cos((pi/6)*lodes)

  #Create standardized plot#
  x11(width=10,height=7.5)
  par(mfrow=c(1,2),mar=c(0,0,0,0))
  plot(0,0, pch=NA,xlab=NA,ylab=NA,xlim=c(-2.25,2.25),ylim=c(0,4.5),asp=1,xaxt='n',yaxt='n',frame.plot=FALSE)
  text(0,4.75,"NADAI PLOT",cex=1.5)
  text(0,4.25,expression(nu),cex=1.5)
  text(-2,3.75,"-1")
  text(2,3.75,"1")
  text(-1,4.1,"Prolate",srt=15)
  text(1,4.1,"Oblate",srt=345)
  text(-1.75,1.75,expression(bar(epsilon)[s]),cex=1.5)
  text(.25,0,"0")
  text(.75,.866,"1")
  text(1.25,1.732,"2")
  text(1.75,2.598,"3")
  text(2.25,3.464,"4")
  lines(c(0,0),c(0,4),lwd=1.5)
  lines(c(0,2),c(0,3.46),lwd=1.5)
  lines(c(0,-2),c(0,3.46),lwd=1.5)
  lines(c(0,1.04),c(0,3.86),lwd=1)
  lines(c(0,-1.04),c(0,3.86),lwd=1)
  draw.arc(x=0,y=0,radius=4,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1.5)
  draw.arc(x=0,y=0,radius=3,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
  draw.arc(x=0,y=0,radius=2,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
  draw.arc(x=0,y=0,radius=1,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
  points(xcoord,ycoord,pch=20)
  index<-1:(length(axDat$X))
  
  #Create data table#
  nadPar<-data.frame(index,octSS,lodes)
  plot(0,0, pch=NA,xlab=NA,ylab=NA,xlim=c(-2,2),ylim=c(0,4.5),asp=1,xaxt='n',yaxt='n',frame.plot=FALSE)
  text(-1,4.25,"Index",cex=1.5,adj=1)
  text(-.375,4.25,expression(bar(epsilon)[s]),cex=1.5)
  text(.375,4.25,expression(nu),cex=1.5)
  lines(c(-2,2),c(4,4),lwd=1.5)
  
  #Add side table of compiled data#
  i<-0
  while(i<length(axDat$X)){
    i<-i+1
    text(-1,4.25-(i*.5),index[i],col='red',adj=1)
    text(-.375,4.25-(i*.5),round(octSS[i],3))
    text(.375,4.25-(i*.5),round(lodes[i],3))
    lines(c(-2,2),c(4-(i*.5),4-(i*.5)))
  }
}

