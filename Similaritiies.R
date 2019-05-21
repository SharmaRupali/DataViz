require(data.table)
require(colorscience)
require(tcR)

lab_measure <- fread("Data/LabMeasurements-Color-Card.csv", dec = ",")
master_color_card <- fread("Data/MasterColorCard.csv", dec = ",")

XX <- data.matrix(lab_measure)
YY <- data.matrix(master_color_card)

# column names
cols <- c("Sheet", "Rows", "Columns", unique(substr(colnames(XX), 2, 3))[c(-1,-2)])

mid_blocks = c() # store the indices of middle elements
for (i in c("44", "45", "54", "55")) {
  mid_blocks[i] = which(cols == i)
}

corners = c() # store the indices of corner elements
for (i in c("11", "18", "81", "88")) {
  corners[i] = which(cols[-mid_blocks] == i)
}

borders = c() # store the indices of border elements
for (i in unique(c(paste(1, 1:8, sep=""), paste(8, 1:8, sep=""), paste(1:8, 1, sep=""), paste(1:8, 8, sep="")))) {
  borders[i] = which(cols[-mid_blocks] == i)
}


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

simi_mat <- cbind(rep(1:13, 42), XX[, c(1,2)], ll)
colnames(simi_mat) <- cols

## without the middle part: cols: 44, 45, 54, 55
## keeping the same name (for without the middle because we're excluding the middle part at all times)
simi_mat <- simi_mat[, -mid_blocks]

## without the corners
simi_mat_no_corners <- simi_mat[, -corners]

## without the borders
simi_mat_no_borders <- simi_mat[, -borders]

## means
simi_means <- matrix(nrow = dim(simi_mat)[1], ncol = 6)
colnames(simi_means) <- c("Sheet", "Rows", "Columns", "Means_simi_all_colors", "Means_simi_no_corners", "Means_simi_no_borders")
simi_means[, 1:3] <- simi_mat[, 1:3]

for (i in 1:dim(simi_mat)[1]) {
  simi_means[i, 4] <- mean(simi_mat[i, 4:dim(simi_mat)[2]])
  simi_means[i, 5] <- mean(simi_mat_no_corners[i, 4:dim(simi_mat_no_corners)[2]])
  simi_means[i, 6] <- mean(simi_mat_no_borders[i, 4:dim(simi_mat_no_borders)[2]])
}
### --- correct similarity
