RFrame <-
function(color = "#DDDDDD", lweight = .5){
  box3d(col = color, lwd = lweight)
  mtext3d("N", edge = 'x++', col = color)
  mtext3d("S", edge = 'x-+', col = color)
  mtext3d("E", edge = 'y++', col = color)
  mtext3d("W", edge = 'y-+', col = color)
}

