# defaults for plots
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))

## BOXPLOT

## boxplot similarities no borders
png(filename="Images/Simi/BoxMeanSimiCardsBySheets.png")
boxplot(simi_frame_means_no_borders, col = color, horizontal = TRUE, main = "Mean similarities wrt cards by sheets", xlab = "Mean similarities", ylab = "Sheet")
abline(v = mean(simi_frame_means_no_borders), col = "red")
dev.off()


## HISTOGRAM

png(filename="Images/Simi/HistMeanSimiCardsBySheets.png")
hist(simi_frame_means_no_borders, main = "Mean similarities wrt cards", xlab = "", ylab = "")
abline(v = mean(simi_frame_means_no_borders), col = "red")
dev.off()


## DENSITY LINES

# calculate min/max for x/y for a proper plot
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(simi_frame_means_no_borders[,i])$x)
  max_x[i] <- max(density(simi_frame_means_no_borders[,i])$x)
  min_y[i] <- min(density(simi_frame_means_no_borders[,i])$y)
  max_y[i] <- max(density(simi_frame_means_no_borders[,i])$y)
}

plot(1, 1, type = "n", xlab = "Mean Similarities", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "Mean similarity density wrt cards by sheets")

for (i in 1:13) {
  lines(density(simi_frame_means_no_borders[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5) # optional legend
dev.copy(png,filename="Images/Simi/DensityMeanSimiCardsBySheets.png")
dev.off()


## VIOLIN PLOT

png(filename="Images/Simi/ViolinMeanSimiCardsBySheets.png")
vioplot(simi_frame_means_no_borders[, 1],
        simi_frame_means_no_borders[, 2],
        simi_frame_means_no_borders[, 3],
        simi_frame_means_no_borders[, 4],
        simi_frame_means_no_borders[, 5],
        simi_frame_means_no_borders[, 6],
        simi_frame_means_no_borders[, 7],
        simi_frame_means_no_borders[, 8],
        simi_frame_means_no_borders[, 9],
        simi_frame_means_no_borders[, 10],
        simi_frame_means_no_borders[, 11],
        simi_frame_means_no_borders[, 12],
        simi_frame_means_no_borders[, 13],
        horizontal = TRUE, col = color,
        main = "Mean similarities wrt cards by sheets", xlab = "Mean similarities", ylab = "Sheet")
abline(v = mean(simi_frame_means_no_borders), col = "red")
dev.off()



### HYPOTHESIS B

# comparing a single color for 1 sheet
## color from lab

## 37
simi_frame_color37 = data.frame()
for (i in 1:13) {
  simi_frame_color37 <- rbind(simi_frame_color37, as.list(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == i, "37"]))
}
simi_frame_color37 <- t(simi_frame_color37)
row.names(simi_frame_color37) <- row_names

## 57
simi_frame_color57 = data.frame()
for (i in 1:13) {
  simi_frame_color57 <- rbind(simi_frame_color57, as.list(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == i, "57"]))
}
simi_frame_color57 <- t(simi_frame_color57)
row.names(simi_frame_color57) <- row_names

## 65
simi_frame_color65 = data.frame()
for (i in 1:13) {
  simi_frame_color65 <- rbind(simi_frame_color65, as.list(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == i, "65"]))
}
simi_frame_color65 <- t(simi_frame_color65)
row.names(simi_frame_color65) <- row_names

## 77
simi_frame_color77 = data.frame()
for (i in 1:13) {
  simi_frame_color77 <- rbind(simi_frame_color77, as.list(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == i, "77"]))
}
simi_frame_color77 <- t(simi_frame_color77)
row.names(simi_frame_color77) <- row_names

# xlim for all charts 
xlim <- c(min(simi_frame_color37, simi_frame_color57, simi_frame_color65, simi_frame_color77), 
          max(simi_frame_color37, simi_frame_color57, simi_frame_color65, simi_frame_color77))

# obverall mean over the 4 selected colors 
mean_overall <- mean(mean(simi_frame_color37), mean(simi_frame_color57), mean(simi_frame_color65), mean(simi_frame_color77))


## BOXPLOT

png(filename="Images/Simi/BoxColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))


plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(simi_frame_color37, horizontal = TRUE, col = color_patches[12], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(simi_frame_color57, horizontal = TRUE, col = color_patches[20], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(simi_frame_color65, horizontal = TRUE, col = color_patches[24], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(simi_frame_color77, horizontal = TRUE, col = color_patches[32], add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Similarity of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## HISTOGRAM

ylim <- c(1,180)
png(filename="Images/Simi/HistColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(simi_frame_color37, col = color_patches[12], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(simi_frame_color57, col = color_patches[20], main = "", xlab = "", ylab = "", add = TRUE) 
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(simi_frame_color65, col = color_patches[24], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(simi_frame_color77, col = color_patches[32], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Similarity of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## VIOLIN PLOTS

png(filename="Images/Simi/ViolinColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(simi_frame_color37[, 1],
        simi_frame_color37[, 2],
        simi_frame_color37[, 3],
        simi_frame_color37[, 4],
        simi_frame_color37[, 5],
        simi_frame_color37[, 6],
        simi_frame_color37[, 7],
        simi_frame_color37[, 8],
        simi_frame_color37[, 9],
        simi_frame_color37[, 10],
        simi_frame_color37[, 11],
        simi_frame_color37[, 12],
        simi_frame_color37[, 13],
        horizontal = TRUE, col = color_patches[12],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(simi_frame_color57[, 1],
        simi_frame_color57[, 2],
        simi_frame_color57[, 3],
        simi_frame_color57[, 4],
        simi_frame_color57[, 5],
        simi_frame_color57[, 6],
        simi_frame_color57[, 7],
        simi_frame_color57[, 8],
        simi_frame_color57[, 9],
        simi_frame_color57[, 10],
        simi_frame_color57[, 11],
        simi_frame_color57[, 12],
        simi_frame_color57[, 13],
        horizontal = TRUE, col = color_patches[20],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(simi_frame_color65[, 1],
        simi_frame_color65[, 2],
        simi_frame_color65[, 3],
        simi_frame_color65[, 4],
        simi_frame_color65[, 5],
        simi_frame_color65[, 6],
        simi_frame_color65[, 7],
        simi_frame_color65[, 8],
        simi_frame_color65[, 9],
        simi_frame_color65[, 10],
        simi_frame_color65[, 11],
        simi_frame_color65[, 12],
        simi_frame_color65[, 13],
        horizontal = TRUE, col = color_patches[24],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(simi_frame_color77[, 1],
        simi_frame_color77[, 2],
        simi_frame_color77[, 3],
        simi_frame_color77[, 4],
        simi_frame_color77[, 5],
        simi_frame_color77[, 6],
        simi_frame_color77[, 7],
        simi_frame_color77[, 8],
        simi_frame_color77[, 9],
        simi_frame_color77[, 10],
        simi_frame_color77[, 11],
        simi_frame_color77[, 12],
        simi_frame_color77[, 13],
        horizontal = TRUE, col = color_patches[32],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Similarity of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## DENSITY LINES

png(filename="Images/Simi/DensityColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

# 37
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(simi_frame_color37[,i])$x)
  max_x[i] <- max(density(simi_frame_color37[,i])$x)
  min_y[i] <- min(density(simi_frame_color37[,i])$y)
  max_y[i] <- max(density(simi_frame_color37[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(simi_frame_color37[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[12]) # optional legend


# 57
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(simi_frame_color57[,i])$x)
  max_x[i] <- max(density(simi_frame_color57[,i])$x)
  min_y[i] <- min(density(simi_frame_color57[,i])$y)
  max_y[i] <- max(density(simi_frame_color57[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(simi_frame_color57[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[20]) # optional legend


# 65
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(simi_frame_color65[,i])$x)
  max_x[i] <- max(density(simi_frame_color65[,i])$x)
  min_y[i] <- min(density(simi_frame_color65[,i])$y)
  max_y[i] <- max(density(simi_frame_color65[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(simi_frame_color65[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[24]) # optional legend


# 77
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(simi_frame_color77[,i])$x)
  max_x[i] <- max(density(simi_frame_color77[,i])$x)
  min_y[i] <- min(density(simi_frame_color77[,i])$y)
  max_y[i] <- max(density(simi_frame_color77[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(simi_frame_color77[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[32]) # optional legend

mtext("Similarity of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


# defaults for plots
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))











### HYPOTHESIS C - Comparing a single color over all sheets
### plot all color distances ~ similarities
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
# var 1
plot(1, 1, type = "b", xlab = "Distance", ylab = "Similarity", log = "x", pch = 19,
     xlim = c(min(unlist(min_x)),max(unlist(max_x))+60),
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "Distance vs Similarity for each Color")

for (i in 1:13) {
  for(j in 1:32) {
    points(col_dist[[i]][j], col_sim[[i]][j], cex=1.5,pch = my_pch[j], col = adjustcolor(color_patches[j], alpha.f = 0.7))#,bg = color_patches[j])#type = "b", cex=1.5)
  }
  
}

legend(x = max(unlist(max_x)) + 5, y=max(unlist(max_y)), legend = 1:16, col=color_patches[1:16], pch = my_pch[1:16]) 
legend(x = max(unlist(max_x)) + 30, y=max(unlist(max_y)), legend = 17:32, col=color_patches[17:32], pch = my_pch[17:32]) # optional legend

dev.copy(png,filename="Images/ScatterDistColors1.png");
dev.off()

# var 2
plot(1, 1, type = "b", xlab = "Distance", ylab = "Similarity", log = "x", pch = 19,
     xlim = c(min(unlist(min_x)),max(unlist(max_x))+150),
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "Distance vs Similarity for each Color")

for (i in 1:13) {
  for(j in 1:32) {
    points(col_dist[[i]][j], col_sim[[i]][j], cex=1.5,pch = 23, col = color[i],bg = color_patches[j])#type = "b", cex=1.5)
  }
  
}

legend(x = max(unlist(max_x)) + 5, y=max(unlist(max_y)), legend = 1:13, col=color, pch = 23) 
legend(x = max(unlist(max_x)) + 30, y=max(unlist(max_y)), legend = 1:16, col=color_patches[1:16], pch = 23) 
legend(x = max(unlist(max_x)) + 80, y=max(unlist(max_y)), legend = 17:32, col=color_patches[17:32], pch = 23) # optional legend


dev.copy(png,filename="Images/ScatterDistColors2.png");
dev.off()









# ## boxplot similarities all colors
# simi_frame_means_all = data.frame()
# for (i in 1:13) {
#   simi_frame_means_all <- rbind(simi_frame_means_all, as.list(simi_means[which(simi_means[, "Sheet"] == i), 4]))
# }
# simi_frame_means_all <- t(simi_frame_means_all)
# row.names(simi_frame_means_all) <- row_names
# #write.csv(simi_frame_means_all, "Similarity_frame_means_all.csv")
# 
# boxplot(simi_frame_means_all, col = colors, horizontal = TRUE, main = "Mean similarities wrt cards by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
# abline(v = mean(simi_frame_means_all))
# 
# 
# ## boxplot similarities no corners
# simi_frame_means_no_corners = data.frame()
# for (i in 1:13) {
#   simi_frame_means_no_corners <- rbind(simi_frame_means_no_corners, as.list(simi_means[which(simi_means[, "Sheet"] == i), 5]))
# }
# simi_frame_means_no_corners <- t(simi_frame_means_no_corners)
# row.names(simi_frame_means_no_corners) <- row_names
# write.csv(simi_frame_means_no_corners, "Similarity_frame_means_no_corners.csv")
# 
# boxplot(simi_frame_means_no_corners, horizontal = TRUE, main = "Mean similarities wrt cards (no corners) by sheets", xlab = "Mean similarities wrt cards", ylab = "Sheet")
# abline(v = mean(simi_frame_means_no_corners))









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
boxplot(simi_mat_no_borders[simi_mat_no_borders[, "Sheet"] == 2, 4], horizontal = TRUE)


ylim = c(0,3)
xlim = c(0,2)




### TESTS
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




