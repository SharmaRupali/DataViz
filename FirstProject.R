setwd("C:/Users/rupal/Google Drive/Beuth Uni/Semester II/Data Visualization/First Project")

require(data.table)
require(colorscience)
require(tcR)

lab_measure <- fread("LabMeasurements-Color-Card.csv", dec = ",")
master_color_card <- fread("MasterColorCard.csv", dec = ",")

XX <- data.matrix(lab_measure)
YY <- data.matrix(master_color_card)


## distance (deltaE) & similarity (cosine similarity) between color patches

## each color patch on each card of each paper is compared to the respective color patch on the master color card
## matrix aa (546,64): rows corresponding to the rows from XX (lab_measure) (1 row = 1 color card with 64 patches), 
## and cols corresponding to the deltaE values for each color patch: 3 cols (XX)(L, a, b) = 1 col (aa) deltaE

### Correct one --
aa <- matrix(nrow = dim(XX)[1], ncol = 64)
bb <- matrix(nrow = 8, ncol = 8)

for (n_row in 1:dim(XX)[1]) {
  for (i in 1:8) {
    for (j in 1:8) {
      bb[i,j] <- deltaE2000(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                            c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
      aa[n_row, ] <- c(t(bb))
    }
  }
}

dist_mat <- cbind(rep(1:13, 42), XX[, c(1,2)], aa)
cols <- unique(substr(colnames(XX), 2, 3))[c(-1,-2)]
colnames(dist_mat) <- c("Sheet", "Rows", "Columns", cols)
### -- Correct one


### correct similarity ---
ll <- matrix(nrow = dim(XX)[1], ncol = 64)
cc <- matrix(nrow = 8, ncol = 8)

for (n_row in 1:dim(XX)[1]) {
  for (i in 1:8) {
    for (j in 1:8) {
      cc[i,j] <- cosine.similarity(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                                   c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]), .do.norm = TRUE)
      ll[n_row, ] <- c(t(cc))
    }
  }
}

simi_mat <- cbind(dist_mat[, 1:3], ll)
colnames(simi_mat) <- colnames(dist_mat)
### --- correct similarity


# ignore middle blocks at all times 44, 45, 54, 55
# all colors
# without corners 11, 18, 81, 88
# without borders 1st row: 11,12,13-18 1st col: 11,21,31-81 8th row: 81,82,83-88 8th col: 18,28,38-88

# intra sheet inter color
# plot every sheet pattern






########## tests distance

# calculate the distances between the colors (Lab), print master and other cards on sheets

Lab1 <- c(51.641, 0.169, 3.054)


which(lab_measure$Row == 1 & lab_measure$Column == 1)
which(master_color_card$Crow == 1 & master_color_card$Ccol == 1)

which(substr(names(lab_measure), 2,3) == 11)

lab_measure[1:13, which(substr(names(lab_measure), 2,3) == 11)]

lab_measure[1:13, 1:5]
lab_measure[1:13, substr(names(lab_measure), 2,3)]


lab_measure[1, c(1,2,6:8)]

xx <- cbind(lab_measure[1:13, 1:5], master_color_card[1, c(1:2, 9:11)])

XX <- data.matrix(lab_measure)
XX[1:13, 1:5]

YY <- data.matrix(master_color_card)
YY[1, c(1:2, 9:11)]

deltaE2000(c(XX[1,3], XX[1,4], XX[1,5]), c(YY[1,9], YY[1,10], YY[1,11])) # 1  # 1 1  # 1 # 1 1
deltaE2000(c(XX[2,3], XX[2,4], XX[2,5]), c(YY[1,9], YY[1,10], YY[1,11])) # 2  # 1 1  # 1 # 1 1

deltaE2000(c(XX[1,6], XX[1,7], XX[1,8]), c(YY[2,9], YY[2,10], YY[2,11])) # 1 # 1 2  # 2 # 1 2



deltaE2000(c(XX[14,3], XX[14,4], XX[14,5]), c(YY[9,9], YY[9,10], YY[9,11])) # 14 # 2 1  # 9 # 2 1
deltaE2000(c(XX[15,3], XX[15,4], XX[15,5]), c(YY[9,9], YY[9,10], YY[9,11])) # 15 # 2 1  # 9 # 2 1




for (i in 1:13){
  del[i] <- deltaE2000(c(XX[i,3], XX[i,4], XX[i,5]), c(YY[1,9], YY[1,10], YY[1,11]))
}

aa = matrix(nrow = dim(XX)[1], ncol = 0)
for (pos_row in 1:7) {
  for (pos_col in 1:6) {
    for (i in 1:8) {
      for (j in 1:8) {
        
      }
    }
  }
}


XX[ , c(paste("L", 1, 1, sep=""), paste("a", 1, 1, sep=""), paste("b", 1, 1, sep=""))]
YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, c(2:3, 9:11)]
c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 11])

aa <- matrix(nrow = dim(XX)[1], ncol = 64)

for (i in 1:8) {
  for (j in 1:8) {
    aa <- deltaE2000(c(XX[paste("L", i, j, sep="")], XX[paste("a", i, j, sep="")], XX[paste("b", i, j, sep="")]), 
                     c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
  }
}


aa <- matrix(nrow = dim(XX)[1], ncol = 64)

for (n_row in 1:dim(XX)[1]) {
  for (i in 1:8) {
    for (j in 1:8) {
      aa[n_row] <- deltaE2000(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                       c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
    }
  }
}



# color(sheet->card) - color(master)
YY[1, 9:11]
XX[1:13, 1:5]


## trials (distances)
aa <- matrix(nrow = dim(XX)[1], ncol = 64)
bb <- matrix(nrow = dim(XX)[1], ncol = 64)

for (n_row in 1:dim(XX)[1]) {
  for (n_col in 1:64) {
    for (i in 1:8) {
      for (j in 1:8) {
        aa[n_row] <- deltaE2000(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                                c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
      }
    }
  }
}

for (n_row in 1:dim(XX)[1]) {
  for (n_col in 1:63) {
    for (i in 1:8) {
      for (j in 1:8) {
        bb[n_row] <- deltaE2000(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                                c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
        n_col = n_col + 1
      }
    }
  }
}


cc <- matrix(nrow = 8, ncol = 8)
for (i in 1:8) {
  for (j in 1:8) {
    cc[i,j] <- deltaE2000(c(XX[1 ,paste("L", i, j, sep="")], XX[1 ,paste("a", i, j, sep="")], XX[1 ,paste("b", i, j, sep="")]),
                            c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]))
  }
}





bb[1,1] <- deltaE2000(c(XX[1,paste("L", 1, 1, sep="")], XX[1,paste("a", 1, 1, sep="")], XX[1,paste("b", 1, 1, sep="")]), 
                      c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 11]))


bb[1,2] <- deltaE2000(c(XX[1,paste("L", 1, 2, sep="")], XX[1,paste("a", 1, 2, sep="")], XX[1,paste("b", 1, 2, sep="")]), 
                      c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 11]))


bb[1,3] <- deltaE2000(c(XX[1,paste("L", 1, 3, sep="")], XX[1,paste("a", 1, 3, sep="")], XX[1,paste("b", 1, 3, sep="")]), 
                      c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 3, 11]))

bb[1,9] <- deltaE2000(c(XX[1,paste("L", 2, 1, sep="")], XX[1,paste("a", 2, 1, sep="")], XX[1,paste("b", 2, 1, sep="")]), 
                      c(YY[YY[, "Crow"] == 2 & YY[, "Ccol"] == 1, 9], YY[YY[, "Crow"] == 2 & YY[, "Ccol"] == 1, 10], YY[YY[, "Crow"] == 2 & YY[, "Ccol"] == 1, 11]))


deltaE2000(c(XX[1,paste("L", 8, 8, sep="")], XX[1,paste("a", 8, 8, sep="")], XX[1,paste("b", 8, 8, sep="")]), 
           c(YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 9], YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 10], YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 11]))

deltaE2000(c(XX[2,paste("L", 8, 8, sep="")], XX[2,paste("a", 8, 8, sep="")], XX[2,paste("b", 8, 8, sep="")]), 
           c(YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 9], YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 10], YY[YY[, "Crow"] == 8 & YY[, "Ccol"] == 8, 11]))


deltaE2000(c(XX[1,paste("L", 6, 5, sep="")], XX[1,paste("a", 6, 5, sep="")], XX[1,paste("b", 6, 5, sep="")]), 
           c(YY[YY[, "Crow"] == 6 & YY[, "Ccol"] == 5, 9], YY[YY[, "Crow"] == 6 & YY[, "Ccol"] == 5, 10], YY[YY[, "Crow"] == 6 & YY[, "Ccol"] == 5, 11]))


# macter color card: color patches
require(colorspace)
LAB(51.641, 0.169, 3.054)






########## tests similarity

# similarities color(sheet->card) - color(master)
XX[1, 1:5]
YY[1, c(2:3, 9:11)]

cosine.similarity(XX[1, 3:5], YY[1, 9:11])
cosine.similarity(XX[1, 6:8], YY[2, 9:11])


cosine.similarity(c(XX[n_row ,paste("L", 1, 1, sep="")], XX[n_row ,paste("a", 1, 1, sep="")], XX[n_row ,paste("b", 1, 1, sep="")]),
                  c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 11]), .do.norm = TRUE)

cosine.similarity(c(XX[n_row ,paste("L", 1, 2, sep="")], XX[n_row ,paste("a", 1, 2, sep="")], XX[n_row ,paste("b", 1, 2, sep="")]),
                  c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 2, 11]), .do.norm = TRUE)

cosine.similarity(c(XX[n_row ,paste("L", 1, 1, sep="")], XX[n_row ,paste("a", 1, 1, sep="")], XX[n_row ,paste("b", 1, 1, sep="")]),
                  c(YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 9], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 10], YY[YY[, "Crow"] == 1 & YY[, "Ccol"] == 1, 11]), .do.norm = TRUE)


### correct similarity ---
ll <- matrix(nrow = dim(XX)[1], ncol = 64)
cc <- matrix(nrow = 8, ncol = 8)

for (n_row in 1:dim(XX)[1]) {
  for (i in 1:8) {
    for (j in 1:8) {
      cc[i,j] <- cosine.similarity(c(XX[n_row ,paste("L", i, j, sep="")], XX[n_row ,paste("a", i, j, sep="")], XX[n_row ,paste("b", i, j, sep="")]),
                            c(YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 9], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 10], YY[YY[, "Crow"] == i & YY[, "Ccol"] == j, 11]), .do.norm = TRUE)
      ll[n_row, ] <- c(t(cc))
    }
  }
}

similarity_mat <- cbind(distance_mat[, 1:3], ll)
colnames(similarity_mat) <- colnames(distance_mat)
### ---

hist(ll)



## means
simi_mat[1, 4:dim(simi_mat)[2]] # for all rows
