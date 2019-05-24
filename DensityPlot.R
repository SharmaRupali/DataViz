## run Distances.R first for the dataframe and colors
hist(dist_frame_means_all)#,  freq = FALSE)
rug(jitter(dist_frame_means_all))
lines(density(dist_frame_means_all), lwd = 2)

?hist
# density plot
# d <- list()
# calculate min/max for x/y for a proper plot
min_x = list()
max_x = list()
min_y = list()
max_y = list()

for(i in 1:13) {
  min_x[i] <- min(density(dist_frame_means_all[,i])$x)
  max_x[i] <- max(density(dist_frame_means_all[,i])$x)
  min_y[i] <- min(density(dist_frame_means_all[,i])$y)
  max_y[i] <- max(density(dist_frame_means_all[,i])$y)
}

plot(1, 1, type = "n", xlab = "", ylab = "", 
     xlim = c(min(unlist(min_x)),max(unlist(max_x))), 
     ylim = c(min(unlist(min_y)), max(unlist(max_y))), main = "Mean distance density wrt cards (all colors) by sheets")
for(i in 1:13) {
  lines(density(dist_frame_means_all[,i]), col = color[i])
}
legend("topright", legend = 1:13, col=color, pch=1) # optional legend
dev.copy(png,filename="Images/DensityMeanDistCardsBySheets.png");
dev.off()