Fry3D <-
function(fry.data, strike = 0, dip = 0, color = "black"){
  fry3d <- matrix(cbind(fry.data$x, fry.data$y, seq(0, 0, len = length(fry.data$x))), ncol = 3)
  fry3d <- rotate3d(fry3d, dip * (pi / 180), -1, 0, 0)
  fry3d <- rotate3d(fry3d, (strike - 90) * (pi / 180), 0, 0, 1)
  points3d(fry3d, col = color)
}

