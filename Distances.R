## BOXPLOT

## boxplot distances no borders
png(filename="Images/Dist/BoxMeanDistCardsBySheets.png")
boxplot(dist_frame_means_no_borders, col = color, horizontal = TRUE, main = "Mean distances wrt cards by sheets", xlab = "Mean distances", ylab = "Sheet")
abline(v = mean(dist_frame_means_no_borders), col = "red")
dev.off()


## HISTOGRAM

png(filename="Images/Dist/HistMeanDistCardsBySheets.png")
hist(dist_frame_means_no_borders, main = "Mean distances wrt cards", xlab = "", ylab = "")
# par(mfrow=c(2,2))
# hist(dist_frame_means_no_borders[,1], main = "??")
# hist(dist_frame_means_no_borders[,2], main = "??")
# hist(dist_frame_means_no_borders[,3], main = "??")
# hist(dist_frame_means_no_borders[,4], main = "??")
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












