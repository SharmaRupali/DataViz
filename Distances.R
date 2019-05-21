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


## distance (deltaE) between color patches

## each color patch on each card of each paper is compared to the respective color patch on the master color card
## matrix aa (546,64): rows corresponding to the rows from XX (lab_measure) (1 row = 1 color card with 64 patches), 
## and cols corresponding to the deltaE values for each color patch: 3 cols (XX)(L, a, b) = 1 col (aa) deltaE

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
colnames(dist_mat) <- cols

## without the middle part: cols: 44, 45, 54, 55
## keeping the same name (for without the middle because we're excluding the middle part at all times)
dist_mat <- dist_mat[, -mid_blocks]

## without the corners
dist_mat_no_corners <- dist_mat[, -corners]

## without the borders
dist_mat_no_borders <- dist_mat[, -borders]

## means
dist_means <- matrix(nrow = dim(dist_mat)[1], ncol = 6)
colnames(dist_means) <- c("Sheet", "Rows", "Columns", "Means_dist_all_colors", "Means_dist_no_corners", "Means_dist_no_borders")
dist_means[, 1:3] <- dist_mat[, 1:3]

for (i in 1:dim(dist_mat)[1]) {
  dist_means[i, 4] <- round(mean(dist_mat[i, 4:dim(dist_mat)[2]]), 2)
  dist_means[i, 5] <- mean(dist_mat_no_corners[i, 4:dim(dist_mat_no_corners)[2]])
  dist_means[i, 6] <- mean(dist_mat_no_borders[i, 4:dim(dist_mat_no_borders)[2]])
}


## boxplot distances all colors
row_names <- paste(dist_means[which(dist_means[, "Sheet"] == 1), "Rows"], dist_means[which(dist_means[, "Sheet"] == 1), "Columns"], sep = "")

dist_frame_means_all = data.frame()
for (i in 1:13) {
  dist_frame_means_all <- rbind(dist_frame_means_all, as.list(dist_means[which(dist_means[, "Sheet"] == i), 4]))
}
dist_frame_means_all <- t(dist_frame_means_all)
row.names(dist_frame_means_all) <- row_names
#write.csv(dist_frame_means_all, "Distance_frame_means_all.csv")

boxplot(dist_frame_means_all, col = color, horizontal = TRUE, main = "Mean distances wrt cards (all colors) by sheets", xlab = "Mean distances wrt cards", ylab = "Sheet")
abline(v = mean(dist_frame_means_all))




hist(dist_frame_means_all)

require(reshape2)
require(ggplot2)

#buc <- list("1" = dist_frame_means_all[, 1], "2" = dist_frame_means_all[, 2])

buc <- list()
for (i in 1:13) {
  buc[[i]] <- dist_frame_means_all[, i]
}

#ggplot(melt(buc), aes(value, fill = L1)) + geom_histogram(position = "stack") + scale_fill_manual(breaks = melt(buc)$L1, values = rainbow(13))

#ggplot(dist_frame_means_all, aes(x = ))

mel <- melt(buc)
tab <- table(mel$L1, mel$value)
barplot(tab, col = color)

color <- rainbow(13)

cut(dist_frame_means_all, seq(min(dist_frame_means_all), max(dist_frame_means_all), 0.05))












































## boxplot distances no corners
dist_frame_means_no_corners = data.frame()
for (i in 1:13) {
  dist_frame_means_no_corners <- rbind(dist_frame_means_no_corners, as.list(dist_means[which(dist_means[, "Sheet"] == i), 5]))
}
dist_frame_means_no_corners <- t(dist_frame_means_no_corners)
row.names(dist_frame_means_no_corners) <- row_names
write.csv(dist_frame_means_no_corners, "Distance_frame_means_no_corners.csv")

boxplot(dist_frame_means_no_corners, horizontal = TRUE, main = "Mean distances wrt cards (no corners) by sheets", xlab = "Mean distances wrt cards", ylab = "Sheet")
abline(v = mean(dist_frame_means_no_corners))


## boxplot distances no borders
dist_frame_means_no_borders = data.frame()
for (i in 1:13) {
  dist_frame_means_no_borders <- rbind(dist_frame_means_no_borders, as.list(dist_means[which(dist_means[, "Sheet"] == i), 6]))
}
dist_frame_means_no_borders <- t(dist_frame_means_no_borders)
row.names(dist_frame_means_no_borders) <- row_names
write.csv(dist_frame_means_no_borders, "Distance_frame_means_no_borders.csv")

boxplot(dist_frame_means_no_borders, horizontal = TRUE, main = "Mean distances wrt cards (no borders) by sheets", xlab = "Mean distances wrt cards", ylab = "Sheet")
abline(v = mean(dist_frame_means_no_borders))










