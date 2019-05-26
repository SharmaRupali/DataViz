# defaults for plots
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))

## BOXPLOT

## boxplot distances no borders
png(filename="Images/Dist/BoxMeanDistCardsBySheets.png")
boxplot(dist_frame_means_no_borders, col = color, horizontal = TRUE, main = "Mean distances wrt cards by sheets", xlab = "Mean distances", ylab = "Sheet")
abline(v = mean(dist_frame_means_no_borders), col = "red")
dev.off()


## HISTOGRAM

png(filename="Images/Dist/HistMeanDistCardsBySheets.png")
hist(dist_frame_means_no_borders, main = "Mean distances wrt cards", xlab = "", ylab = "")
abline(v = mean(dist_frame_means_no_borders), col = "red")
dev.off()


## DENSITY LINES

# calculate min/max for x/y for a proper plot
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_means_no_borders[,i])$x)
  max_x[i] <- max(density(dist_frame_means_no_borders[,i])$x)
  min_y[i] <- min(density(dist_frame_means_no_borders[,i])$y)
  max_y[i] <- max(density(dist_frame_means_no_borders[,i])$y)
}

plot(1, 1, type = "n", xlab = "Mean distances", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "Mean distance density wrt cards by sheets")

for (i in 1:13) {
  lines(density(dist_frame_means_no_borders[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5) # optional legend
dev.copy(png,filename="Images/Dist/DensityMeanDistCardsBySheets.png")
dev.off()


## VIOLIN PLOT

png(filename="Images/Dist/ViolinMeanDistCardsBySheets.png")
vioplot(dist_frame_means_no_borders[, 1],
        dist_frame_means_no_borders[, 2],
        dist_frame_means_no_borders[, 3],
        dist_frame_means_no_borders[, 4],
        dist_frame_means_no_borders[, 5],
        dist_frame_means_no_borders[, 6],
        dist_frame_means_no_borders[, 7],
        dist_frame_means_no_borders[, 8],
        dist_frame_means_no_borders[, 9],
        dist_frame_means_no_borders[, 10],
        dist_frame_means_no_borders[, 11],
        dist_frame_means_no_borders[, 12],
        dist_frame_means_no_borders[, 13],
        horizontal = TRUE, col = color,
        main = "Mean distances wrt cards by sheets", xlab = "Mean distances", ylab = "Sheet")
abline(v = mean(dist_frame_means_no_borders), col = "red")
dev.off()



### HYPOTHESIS B

# comparing a single color for 1 sheet
## color from lab

## 37
dist_frame_color37 = data.frame()
for (i in 1:13) {
  dist_frame_color37 <- rbind(dist_frame_color37, as.list(dist_mat_no_borders[dist_mat_no_borders[, "Sheet"] == i, "37"]))
}
dist_frame_color37 <- t(dist_frame_color37)
row.names(dist_frame_color37) <- row_names

## 57
dist_frame_color57 = data.frame()
for (i in 1:13) {
  dist_frame_color57 <- rbind(dist_frame_color57, as.list(dist_mat_no_borders[dist_mat_no_borders[, "Sheet"] == i, "57"]))
}
dist_frame_color57 <- t(dist_frame_color57)
row.names(dist_frame_color57) <- row_names

## 65
dist_frame_color65 = data.frame()
for (i in 1:13) {
  dist_frame_color65 <- rbind(dist_frame_color65, as.list(dist_mat_no_borders[dist_mat_no_borders[, "Sheet"] == i, "65"]))
}
dist_frame_color65 <- t(dist_frame_color65)
row.names(dist_frame_color65) <- row_names

## 77
dist_frame_color77 = data.frame()
for (i in 1:13) {
  dist_frame_color77 <- rbind(dist_frame_color77, as.list(dist_mat_no_borders[dist_mat_no_borders[, "Sheet"] == i, "77"]))
}
dist_frame_color77 <- t(dist_frame_color77)
row.names(dist_frame_color77) <- row_names

# xlim for all charts 
xlim <- c(min(dist_frame_color37, dist_frame_color57, dist_frame_color65, dist_frame_color77), 
          max(dist_frame_color37, dist_frame_color57, dist_frame_color65, dist_frame_color77))

# obverall mean over the 4 selected colors 
mean_overall <- mean(mean(dist_frame_color37), mean(dist_frame_color57), mean(dist_frame_color65), mean(dist_frame_color77))


## BOXPLOT

png(filename="Images/Dist/BoxColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))


plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(dist_frame_color37, horizontal = TRUE, col = color_patches[12], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(dist_frame_color57, horizontal = TRUE, col = color_patches[20], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(dist_frame_color65, horizontal = TRUE, col = color_patches[24], add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
boxplot(dist_frame_color77, horizontal = TRUE, col = color_patches[32], add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Distance of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## HISTOGRAM

ylim <- c(1,130)
png(filename="Images/Dist/HistColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(dist_frame_color37, col = color_patches[12], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(dist_frame_color57, col = color_patches[20], main = "", xlab = "", ylab = "", add = TRUE) 
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(dist_frame_color65, col = color_patches[24], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim, ylim = ylim)
hist(dist_frame_color77, col = color_patches[32], main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Distance of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## VIOLIN PLOTS

png(filename="Images/Dist/ViolinColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(dist_frame_color37[, 1],
        dist_frame_color37[, 2],
        dist_frame_color37[, 3],
        dist_frame_color37[, 4],
        dist_frame_color37[, 5],
        dist_frame_color37[, 6],
        dist_frame_color37[, 7],
        dist_frame_color37[, 8],
        dist_frame_color37[, 9],
        dist_frame_color37[, 10],
        dist_frame_color37[, 11],
        dist_frame_color37[, 12],
        dist_frame_color37[, 13],
        horizontal = TRUE, col = color_patches[12],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(dist_frame_color57[, 1],
        dist_frame_color57[, 2],
        dist_frame_color57[, 3],
        dist_frame_color57[, 4],
        dist_frame_color57[, 5],
        dist_frame_color57[, 6],
        dist_frame_color57[, 7],
        dist_frame_color57[, 8],
        dist_frame_color57[, 9],
        dist_frame_color57[, 10],
        dist_frame_color57[, 11],
        dist_frame_color57[, 12],
        dist_frame_color57[, 13],
        horizontal = TRUE, col = color_patches[20],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(dist_frame_color65[, 1],
        dist_frame_color65[, 2],
        dist_frame_color65[, 3],
        dist_frame_color65[, 4],
        dist_frame_color65[, 5],
        dist_frame_color65[, 6],
        dist_frame_color65[, 7],
        dist_frame_color65[, 8],
        dist_frame_color65[, 9],
        dist_frame_color65[, 10],
        dist_frame_color65[, 11],
        dist_frame_color65[, 12],
        dist_frame_color65[, 13],
        horizontal = TRUE, col = color_patches[24],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

plot(1:13,1:13, type = "n", xlim = xlim)
vioplot(dist_frame_color77[, 1],
        dist_frame_color77[, 2],
        dist_frame_color77[, 3],
        dist_frame_color77[, 4],
        dist_frame_color77[, 5],
        dist_frame_color77[, 6],
        dist_frame_color77[, 7],
        dist_frame_color77[, 8],
        dist_frame_color77[, 9],
        dist_frame_color77[, 10],
        dist_frame_color77[, 11],
        dist_frame_color77[, 12],
        dist_frame_color77[, 13],
        horizontal = TRUE, col = color_patches[32],
        main = "", xlab = "", ylab = "", add = TRUE)
abline(v = mean_overall, col = "red")

mtext("Distance of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


## DENSITY LINES

png(filename="Images/Dist/DensityColorForCardsBySheet.png")

plot.new()
par(mfrow=c(2,2), oma=c(2.1, 2.1, 4.1, 2.1), mar=c(2,2,1,1))

# 37
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_color37[,i])$x)
  max_x[i] <- max(density(dist_frame_color37[,i])$x)
  min_y[i] <- min(density(dist_frame_color37[,i])$y)
  max_y[i] <- max(density(dist_frame_color37[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(dist_frame_color37[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[12]) # optional legend


# 57
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_color57[,i])$x)
  max_x[i] <- max(density(dist_frame_color57[,i])$x)
  min_y[i] <- min(density(dist_frame_color57[,i])$y)
  max_y[i] <- max(density(dist_frame_color57[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(dist_frame_color57[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[20]) # optional legend


# 65
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_color65[,i])$x)
  max_x[i] <- max(density(dist_frame_color65[,i])$x)
  min_y[i] <- min(density(dist_frame_color65[,i])$y)
  max_y[i] <- max(density(dist_frame_color65[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(dist_frame_color65[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[24]) # optional legend


# 77
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_color77[,i])$x)
  max_x[i] <- max(density(dist_frame_color77[,i])$x)
  min_y[i] <- min(density(dist_frame_color77[,i])$y)
  max_y[i] <- max(density(dist_frame_color77[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "")

for (i in 1:13) {
  lines(density(dist_frame_color77[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, lty = 1, cex = 0.5, bg = color_patches[32]) # optional legend

mtext("Distance of individual colors by cards for sheets", side = 3, line = 1, font = 2, outer = TRUE)
dev.off()


# defaults for plots
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))




# # cols: sheets, rows: mean distances 
# dist_frame_means_all = data.frame()
# for (i in 1:13) {
#   dist_frame_means_all <- rbind(dist_frame_means_all, as.list(dist_means[which(dist_means[, "Sheet"] == i), 4]))
# }
# dist_frame_means_all <- t(dist_frame_means_all)
# row.names(dist_frame_means_all) <- row_names
# #write.csv(dist_frame_means_all, "Distance_frame_means_all.csv")




# require(reshape2)
# require(ggplot2)
# 
# #buc <- list("1" = dist_frame_means_all[, 1], "2" = dist_frame_means_all[, 2])
# 
# buc <- list()
# for (i in 1:13) {
#   buc[[i]] <- dist_frame_means_all[, i]
# }
# 
# #ggplot(melt(buc), aes(value, fill = L1)) + geom_histogram(position = "stack") + scale_fill_manual(breaks = melt(buc)$L1, values = rainbow(13))
# 
# #ggplot(dist_frame_means_all, aes(x = ))
# 
# mel <- melt(buc)
# tab <- table(mel$L1, mel$value)
# 
# png(filename="Images/StackBarMeanDistCardsBySheets.png")
# barplot(tab, col = color)
# abline(v = mean(dist_frame_means_all), col = "red")
# dev.off()
# 
# # set some new bins
# 
# buc_cut <- list()
# 
# for (i in 1:13) {
#   buc_cut[[i]] <- table(cut(dist_frame_means_all[,i], seq(min(dist_frame_means_all[]), max(dist_frame_means_all), 0.05)))
# }
# buc_cut
# mel_cut <- melt(buc_cut);mel_cut
# tab_cut <- table(mel_cut$L1, mel_cut$value); tab_cut
# 
# # stacked barplot for the new bins
# # beside: a logical value. 
# # If FALSE, the columns of height are portrayed as stacked bars, 
# # and if TRUE the columns are portrayed as juxtaposed bars.
# barplot(height = mel_cut$value, names.arg = mel_cut$Var1, col = color, beside = F)
# ### does not work as exprected ###
# 
# # 1st: try the cut
# cut(dist_frame_means_all, seq(min(dist_frame_means_all), max(dist_frame_means_all), 0.05))




# ## boxplot distances no corners
# dist_frame_means_no_corners = data.frame()
# for (i in 1:13) {
#   dist_frame_means_no_corners <- rbind(dist_frame_means_no_corners, as.list(dist_means[which(dist_means[, "Sheet"] == i), 5]))
# }
# dist_frame_means_no_corners <- t(dist_frame_means_no_corners)
# row.names(dist_frame_means_no_corners) <- row_names
# write.csv(dist_frame_means_no_corners, "Distance_frame_means_no_corners.csv")
# 
# boxplot(dist_frame_means_no_corners, horizontal = TRUE, main = "Mean distances wrt cards (no corners) by sheets", xlab = "Mean distances wrt cards", ylab = "Sheet")
# abline(v = mean(dist_frame_means_no_corners))












