pMOD <-
function(strike = 0, dip = 45, x.loc = 0, y.loc = 0, z.loc = 0, n.faces=32, scale.factor = 1, color="orange"){
  a<-seq(0, 2 * pi, len = n.faces)
  x<-cos(a)
  y<-sin(a)

  vert<-NULL
  
  i<-0
  while(i < n.faces){
    i<-i + 1
    vert.i<-c(
      0, 0, 0, 1,
      x[i], y[i], 0, 1,
      x[i+1], y[i+1], 0, 1
      )
    vert<-c(vert, vert.i)
  }
  
  i<-0
  while(i < length(strike)){
    i<-i + 1
    ind<-1:(n.faces * 3)
    obj.a<-tmesh3d(vert, ind)
    
    obj.b<-rotate3d(obj.a, dip[i] * (pi / 180), 0, -1, 0)
    obj.c<-rotate3d(obj.b, strike[i] * (pi / 180), 0, 0, 1)
    obj.d<-translate3d(obj.c, x.loc[i], y.loc[i], z.loc[i])
    obj.e<-scale3d(obj.d, scale.factor, scale.factor, scale.factor)
    
    if(length(color) == 1){
    shade3d(obj.e, col = color)
    }
    else(shade3d(obj.e, col = color[i]))
  }
}

