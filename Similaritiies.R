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


## similarity (cosine similarity) between color patches

## each color patch on each card of each paper is compared to the respective color patch on the master color card
## matrix aa (546,64): rows corresponding to the rows from XX (lab_measure) (1 row = 1 color card with 64 patches), 
## and cols corresponding to the cosine.similarity values for each color patch: 3 cols (XX)(L, a, b) = 1 col (aa) cosine.similarity

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


## boxplot similarities all colors
row_names <- paste(simi_means[which(simi_means[, "Sheet"] == 1), "Rows"], simi_means[which(simi_means[, "Sheet"] == 1), "Columns"], sep = "")

simi_frame_means_all = data.frame()
for (i in 1:13) {
  simi_frame_means_all <- rbind(simi_frame_means_all, as.list(simi_means[which(simi_means[, "Sheet"] == i), 4]))
}
simi_frame_means_all <- t(simi_frame_means_all)
row.names(simi_frame_means_all) <- row_names
write.csv(simi_frame_means_all, "Similarity_frame_means_all.csv")

boxplot(simi_frame_means_all, horizontal = TRUE, main = "Mean similarities wrt cards by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
abline(v = mean(simi_frame_means_all))


## boxplot similarities no corners
simi_frame_means_no_corners = data.frame()
for (i in 1:13) {
  simi_frame_means_no_corners <- rbind(simi_frame_means_no_corners, as.list(simi_means[which(simi_means[, "Sheet"] == i), 5]))
}
simi_frame_means_no_corners <- t(simi_frame_means_no_corners)
row.names(simi_frame_means_no_corners) <- row_names
write.csv(simi_frame_means_no_corners, "Similarity_frame_means_no_corners.csv")

boxplot(simi_frame_means_no_corners, horizontal = TRUE, main = "Mean similarities wrt cards (no corners) by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
abline(v = mean(simi_frame_means_no_corners))


## boxplot similarities no borders
simi_frame_means_no_borders = data.frame()
for (i in 1:13) {
  simi_frame_means_no_borders <- rbind(simi_frame_means_no_borders, as.list(simi_means[which(simi_means[, "Sheet"] == i), 6]))
}
simi_frame_means_no_borders <- t(simi_frame_means_no_borders)
row.names(simi_frame_means_no_borders) <- row_names
write.csv(simi_frame_means_no_borders, "Similarity_frame_means_no_borders.csv")

boxplot(simi_frame_means_no_borders, horizontal = TRUE, main = "Mean similarities wrt cards (no borders) by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
abline(v = mean(simi_frame_means_no_borders))


