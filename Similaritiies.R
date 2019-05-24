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

## colors for the sheets
colors <- rainbow(13)

## rownames for simi_means columns
row_names <- paste(simi_means[which(simi_means[, "Sheet"] == 1), "Rows"], simi_means[which(simi_means[, "Sheet"] == 1), "Columns"], sep = "")

## boxplot similarities all colors
simi_frame_means_all = data.frame()
for (i in 1:13) {
  simi_frame_means_all <- rbind(simi_frame_means_all, as.list(simi_means[which(simi_means[, "Sheet"] == i), 4]))
}
simi_frame_means_all <- t(simi_frame_means_all)
row.names(simi_frame_means_all) <- row_names
#write.csv(simi_frame_means_all, "Similarity_frame_means_all.csv")

boxplot(simi_frame_means_all, col = colors, horizontal = TRUE, main = "Mean similarities wrt cards by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
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

boxplot(simi_frame_means_no_borders, col = colors, horizontal = TRUE, main = "Mean similarities wrt cards (no borders) by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
abline(v = mean(simi_frame_means_no_borders))







############# check colors
library("data.table")
library("plotrix")


plot(c(1, 8), c(1, 8), type = "n", xlab = "", ylab = "", main = "MasterCard")
## draw rectangles with bottom left (100, 300)+i
## and top right (150, 380)+i
rect(2, 2, 3, 1, col = cols, border = 4)
     rect(3, 2, 4, 1, col = "blue", border = 0)
     rect(4, 2, 5, 1, col = "red", border = 0)




XX[XX[, "Row"] == 4 & XX[, "Column"] == 2, 6]

     
L <- XX[XX[, "Row"] == 4 & XX[, "Column"] == 2, 6]     
a <- XX[XX[, "Row"] == 4 & XX[, "Column"] == 2, 7]
b <- XX[XX[, "Row"] == 4 & XX[, "Column"] == 2, 8]
cols <- convertColor(cbind(L,a,b),from="Lab",to="sRGB",clip=NA)
cols <- sapply(1:nrow(cols), function(obj) ifelse(any(is.na(cols[obj,])),NA,rgb(matrix(cols[obj,], nrow=1))))
L <- L[!is.na(cols)]
a <- a[!is.na(cols)]
b <- b[!is.na(cols)]
cols <- cols[!is.na(cols)]
boxplot(simi_frame_means_all, col = cols, horizontal = TRUE, main = "Mean distances wrt cards (all colors) by sheets", xlab = "Mean distances wrt cards", ylab = "Sheet")




simi_means[simi_means[, "Rows"] == 4 & simi_means[, "Columns"] == 2, ]

simi_means[simi_means[, "Sheet"] == 1 && simi_means[, "Rows"] == 5 && simi_means[, "Columns"] == 2, 1]


DD <- cbind(rep(1:13, 42), XX[, c(1,2,6:8)])
colnames(DD) <- (c("Sheet","Row","Column", "L12",    "a12" ,   "b12" ))



barplot(CC[CC[, "Sheet"] == 1, 4], 4)


simi_mat


simi_mat[simi_mat_no_borders[, "Sheet"] == 1, 5]



simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 1, 3:dim(simi_mat_no_borders)[2]]
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 1, 4:dim(simi_mat_no_borders)[2]], horizontal = TRUE)
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 2, 4:dim(simi_mat_no_borders)[2]], horizontal = TRUE)
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 3, 4:dim(simi_mat_no_borders)[2]], horizontal = TRUE)
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 4, 4:dim(simi_mat_no_borders)[2]], horizontal = TRUE)
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 5, 4:dim(simi_mat_no_borders)[2]], horizontal = TRUE)


cbind(aaa$names,aaa$stats[1,])



boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 1, 4], horizontal = TRUE)



ylim = c(0,3)
xlim = c(0,2)


########33 color 22
dddd_sim <- c()
dddd_sim[1] <- mean(simi_mat[simi_mat[, "Sheet"] == 1, "22"])
dddd_sim[2] <- mean(simi_mat[simi_mat[, "Sheet"] == 2, "22"])

dddd_dist <- c()
dddd_dist[1] <- mean(dist_mat[dist_mat[, "Sheet"] == 1, "22"])
dddd_dist[2] <- mean(dist_mat[dist_mat[, "Sheet"] == 2, "22"])

plot(dddd_dist, dddd_sim, col = "red", pch = 16, ylim = ylim, xlim=xlim)

plot(dddd, col = "red", pch = 16, ylim = ylim)

###### color 23
ee <- c()
ee[1] <- mean(simi_mat[simi_mat[, "Sheet"] == 1, "23"])
ee[2] <- mean(simi_mat[simi_mat[, "Sheet"] == 2, "23"])

ee_dist <- c()
ee_dist[1] <- mean(dist_mat[dist_mat[, "Sheet"] == 1, "23"])
ee_dist[2] <- mean(dist_mat[dist_mat[, "Sheet"] == 2, "23"])

points(ee_dist, ee, col = "blue", pch = 16)



col_sim_temp <- c()
col_sim <- list()

col_dist_temp <- c()
col_dist <- list()

for (i in 1:13) {
  for (j in 4:dim(simi_mat_no_borders)[2]) {
    col_sim_temp[j-3] <- mean(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == i, j])
  }
  col_sim[[i]] <- col_sim_temp
}

for (i in 1:13) {
  for (j in 4:dim(dist_mat_no_borders)[2]) {
    col_dist_temp[j-3] <- mean(dist_mat_no_borders[dist_mat_no_borders[, "Sheet"] == i, j])
  }
  col_dist[[i]] <- col_dist_temp
}



plot(col_dist[[1]], col_sim[[1]], col = "red", pch = 16)
plot(1,1, type = "n", )




# calculate min/max for x/y for a proper plot
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(col_dist[[i]])
  max_x[i] <- max(col_dist[[i]])
  min_y[i] <- min(col_sim[[i]])
  max_y[i] <- max(col_sim[[i]])
}

plot(1, 1, type = "b", xlab = "", ylab = "", log = "x", pch = 19,
     xlim = c(min(unlist(min_x)),max(unlist(max_x))),
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "???")

col_col <- rainbow(32)


for (i in 1:13) {
  for(j in 1:32) {
    lines(col_dist[[i]][j], col_sim[[i]][j], col = col_col[j], type = "b", cex=1.5)
  }
}


legend("topright", legend = 1:13, col=color, pch=1) # optional legend
dev.copy(png,filename="Images/DensityMeanDistCardsBySheets.png");
dev.off()
