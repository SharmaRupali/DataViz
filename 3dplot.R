## 3D plotting test ##
# load required library
require(rgl)
require(colorspace)

fake <- function() {
  X <- c(1, 1, 2, 2)
  X <- rnorm(500)
  Y <- c(3, 3, 5, 5)
  Y <- rnorm(500)
  Z <- c(7, 8, 8, 3)
  Z <- rnorm(500)
  
  radi <- rnorm(500, mean = 5, sd = 2)
  
  geom.data <- data.frame(X, Y, Z)
  plot3d(x = geom.data$X, y = geom.data$Y, z = geom.data$Z, type = "h", col = c("black", "blue", "red"))
  
  rgl.open()
  rgl.bg(color = "white", fogtype = "linear") # set background
  #rgl.points(X, Y, Z, color = "blue", size = "6") # scatter/point plot
  #rgl.spheres(X, Y, Z, radius = 0.2, color = "blue") # well, just a plain sphere
  #rgl.quads(X, Y, Z, color = "red")
  
  #rgl.triangles(c(0,0,0), c(5, 0, 0), c(0, 0, -2), color = "green")
  triangles3d(c(0,0,0), c(5, 0, 0), c(0, 0, -2), color = "green")
  quads3d(c(0,0,0, 0), c(-8, 3, 5, 1), c(2, 1, -1, 2), color = "blue")
  rgl.points(1, 2, 3)
  rgl.lines(c(-10, 10), c(0, 0), c(0, 0), color = "black") # x
  rgl.lines(c(0, 0), c(-10,10), c(0, 0), color = "red") # y
  rgl.lines(c(0, 0), c(0, 0), c(-10,10), color = "green") # z
  
  surface3d(c(0,0,0, 0), c(-8, 3, 5, 1), c(2, 1, -1, 2), color = "blue")
}


## 05/07/2019 ##
masterCC <- read.csv2("/home/lfko/git/2ndSemester/DataVis/first-project/MasterColorCard.csv")
rgl.bg(color = "white", fogtype = "linear") # set background
# create the ColorCard with the supplied L*a*b color values


# xlab = "L"
# ylab = "a"
# zlab = "b"# Color card has 8x8 tiles
# one tile is one color patch/measurement (L*a*b)
cc = NULL
for(i in 1:64){
  # sample(seq(from = 0, to = 100, by = 5), size = 3, replace = TRUE)
  cc <- rbind(cc, c(masterCC[i,"L"], masterCC[i,"a"], masterCC[i,"b"]))
}
  
  
# x axis
rgl.lines(c(0, 10), c(0, 0), c(0, 0), color = "black")
# y axis
rgl.lines(c(0, 0), c(-10, 10), c(0, 0), color = "red")
# z axis
rgl.lines(c(0, 0), c(0, 0), c(-10, 10), color = "green")
title3d(xlab="L", ylab="a", zlab="b")
#q1 <- rbind(c(1,2,3), c(3,2,1), c(-1,-2,-3), c(-3,-2,-1))
#q2 <- rbind(c(5,6,7), c(7,6,5), c(-5,-6,-7), c(-7,-6,-5))
#q1 <- rbind(c(2,1,4), c(4,1,2), c(-2,-1,-4), c(-4,-2,-1))
q1 <- rbind(cc[1,], cc[8,], cc[58,], cc[64,])
#for(i in 1:4){
#  M <- rbind(rnorm(n = 3), rnorm(n = 3), rnorm(n = 3), rnorm(n = 3))
#  quads3d(M, col = "green")
#}
#quads3d(q1, col = "green")
#quads3d(q2, col = "red")
#quads3d(q3, col = "blue")
labCol <- NULL
for(i in 1:64){
  #points3d(x = cc[i,1], y = cc[i,2], z = cc[i,3], size = 3, col = "cyan")
  labCol <- rbind(labCol, c(cc[i,1], cc[i,2], cc[i,3]))
  #print(labCol)
  #shapelist3d(shapes = cube3d(), x = cc[i,1], y = cc[i,2], z = cc[i,3], col = convertColor(labCol, from = "Lab", to = "sRGB", clip = NA))
  #arrow3d(c(0, 0, 0), c(sum(q1[,1])/4, sum(q1[,2])/4, sum(q1[,3])/4), type = "lines", col = "purple")
  #arrow3d(c(0, 0, 0), c(cc[i,1], cc[i,2], cc[i,3]), type = "lines", thickness = 0.1, width = 0.1, col = "purple")
  
}

srgb <- convertColor(labCol,from="Lab",to="sRGB",clip=NA)
clipped <- attr(na.omit(srgb),"na.action")
srgb[clipped,] <- 0
cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
for(i in 1:64){
  points3d(x = cc[i,1], y = cc[i,2], z = cc[i,3], size = 30, col = cols[i])
  #shapelist3d(shapes = cube3d(), x = cc[i,1], y = cc[i,2], z = cc[i,3], col = cols[i])
  #arrow3d(c(0, 0, 0), c(cc[i,1], cc[i,2], cc[i,3]), type = "lines", thickness = 0.1, width = 0.1, col = "purple")
  
}
#rgl.surface(masterCC[,"L"], masterCC[,"a"], masterCC[,"b"], color = "steelblue", alpha = 0.5, lit = FALSE)
for(k in 1:8){
  A = c(-1+k,0, 0)
  B = c(k,0, 0)
  C = c(k,1,0)
  D = c(-1+k,1,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k])
}

for(k in 1:8){
  A = c(-1+k,1, 0)
  B = c(k,1, 0)
  C = c(k,2,0)
  D = c(-1+k,2,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+8])
}

for(k in 1:8){
  A = c(-1+k,2, 0)
  B = c(k,2, 0)
  C = c(k,3,0)
  D = c(-1+k,3,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+16])
}

for(k in 1:8){
  A = c(-1+k,3, 0)
  B = c(k,3, 0)
  C = c(k,4,0)
  D = c(-1+k,4,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+24])
}

for(k in 1:8){
  A = c(-1+k,4, 0)
  B = c(k,4, 0)
  C = c(k,5,0)
  D = c(-1+k,5,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+32])
}

for(k in 1:8){
  A = c(-1+k,5, 0)
  B = c(k,5, 0)
  C = c(k,6,0)
  D = c(-1+k,6,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+40])
}

for(k in 1:8){
  A = c(-1+k,6, 0)
  B = c(k,6, 0)
  C = c(k,7,0)
  D = c(-1+k,7,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+48])
}

for(k in 1:8){
  A = c(-1+k,7, 0)
  B = c(k,7, 0)
  C = c(k,8,0)
  D = c(-1+k,8,0)
  rect = rbind(A, B, C, D)
  quads3d(rect, col = cols[k+56])
}

#for(i in 1:4){
#  arrow3d(c(0, 0, 0), c(i*0.5, i*0.5, i),type = "lines", col = "green")
#}