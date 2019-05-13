## 05/11/2019 ##
# load required library
require(rgl)
require(colorspace)
allMeasures <- read.csv2("/home/lfko/git/2ndSemester/DataVis/first-project/LabMeasurements-Color-Card.csv")
masterData <- read.csv2("/home/lfko/git/2ndSemester/DataVis/first-project/MasterColorCard.csv")
n <- 64 ## how many cards/measurements to open
masterCC = NULL
for(i in 1:64){
  masterCC <- rbind(masterCC, c(masterData[i,"L"], masterData[i,"a"], masterData[i,"b"]))
}

card = NULL
cardsList = list()
for(k in 1:n){

  # read one row (just the color values) - this is a color card
  colVec <- t(allMeasures[k, 3:194]) # transpose the rows to a one column vector

    for(i in seq(1, 192, 3)){
      card <- rbind(card, c(colVec[i,], colVec[i+1,], colVec[i+2,]))  
      
      ## create new class ColorCord
      #colCard <- setRefClass("ColorCard", fields = list(id = "numeric", L = "numeric", a = "numeric", b = "numeric"))
      #cc <- colCard(id = 1, L = colVec[i,], a = colVec[i+1,], b = colVec[i+2,])
      # this is maybe not intended/possible
      #cardsList <- rbind(cardsList, cc)
    }
  cardsList[[k]] = card # TODO maybe not use a list() here
  card = NULL # reset the card list, so it can be filled again
}

## get MasterCard mean values ##
L_mean_mc <- mean(masterCC[,1])
a_mean_mc <- mean(masterCC[,2])
b_mean_mc <- mean(masterCC[,3])

## MasterCard
prep.plot()
plot.ColorCard(masterCC, scale = 10, cardName = "MasterCard", plotDispVec = FALSE)
for(cnt in 1:n){
  name <- paste("Measure", cnt, sep = "")
  plot.ColorCard(cardsList[[cnt]], scale = 10, cardName = name, plotDispVec = TRUE)
  #plot.ColorCard(cardsList[[1]], scale = 10, cardName = "Measure1", plotDispVec = TRUE)
  #plot.ColorCard(cardsList[[2]], scale = -10, cardName = "Measure2")
  #plot.ColorCard(cardsList[[3]], scale = -20, cardName = "Measure3")
}

plot.ColorCard <- function(colorCard, scale = 10, cardName, plotDispVec = TRUE){
  
  ## calculate the mean for L, a and b
  ## then compute a vector, starting from the MasterCard to the card in question, with the mean values as coordinates
  ccCent <- NaN ## to avoid crashing
  if(plotDispVec == TRUE)
    ccCent <- plot.DispersionVecs(colorCard, scale = scale)
  
  ## now use the centroid/the vector end as the starting point for constructing the square
  ## here I assume that the centroid should point to the center of the square
  ## so the starting point for the first square needs to be normalized
  #first_point = NULL
  #last_point = NULL
  ## convert L*a*b to sRGB for plotting
  cols <- convert.Lab(colorCard)
  
  if(is.nan(ccCent) == TRUE){
    x = 0
    y = 0
    z = 0
  }
  else {
    print(ccCent)
    x = ccCent[1] - 4 # starting x coordinate
    y = ccCent[2] - 5# starting y coordinate
    z = ccCent[3] # starting z coordinate
  }
  currCol = 1 # current color

  # prints a text a the beginning of the first row to show the name of the card
  text3d(x = x, y = y, z = 1,texts = cardName, col = "red")
  
  ## outer loop: rows of a color card
  for(cardRow in 1:8){
    ## inner loop: colums of a color card
    for(cardCol in 1:8){
      ## don't fill the middle square (2x2)
      if(cardRow %in% c(4,5) && cardCol %in% c(4,5))
        next
      
      A = c(x + cardCol, y, z)
      B = c(x + cardCol + 1, y, z)
      C = c(x + cardCol+ 1, y + 1, z)
      D = c(x + cardCol, y + 1, z)
      ## build a rectangle out of the 4 coordinate points
      rect = rbind(A, B, C, D)
      ## this will actually plot the square
      quads3d(rect, col = cols[currCol])
      
      currCol = currCol + 1
#      if(incr == 8 && x == 1)
#        last_point <- A
    }
#    if(incr == 1)
#      first_point <- D
    y = y + 1
  }
  
  # mark the midpoint/center
  #points3d(x = (first_point[1] + last_point[1])/2 , y = (first_point[2] + last_point[2])/2, z = (first_point[3] + last_point[3])/2, size = 30, col = "red")
}

plot.DispersionVecs <- function(colorCard, scale = 10){
  L_mean <- mean(colorCard[,1])
  a_mean <- mean(colorCard[,2])
  b_mean <- mean(colorCard[,3])
  centroid <- c(L_mean_mc - L_mean, a_mean_mc - a_mean, b_mean_mc - b_mean) * scale
  arrow3d(c(0, 0, 0), centroid, type = "lines", thickness = 0.1, width = 0.1, col = "purple")
  
  return(centroid)
}

convert.Lab <- function(labCol){
  
  srgb <- convertColor(labCol,from="Lab",to="sRGB",clip=NA)
  clipped <- attr(na.omit(srgb),"na.action")
  srgb[clipped,] <- 0
  cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
  
  return(cols)
}

prep.plot <- function(bgColor = "grey", fogtype = "none"){
  rgl.bg(color = bgColor, fogtype = fogtype) # set background
  # x axis
  rgl.lines(c(0, 10), c(0, 0), c(0, 0), color = "black")
  # y axis
  rgl.lines(c(0, 0), c(-10, 10), c(0, 0), color = "red")
  # z axis
  rgl.lines(c(0, 0), c(0, 0), c(-10, 10), color = "green")
  title3d(xlab="L", ylab="a", zlab="b")
}


