
mean_dist_frame_means_no_borders <- round(mean(dist_frame_means_no_borders),3); mean_dist_frame_means_no_borders
min_dist_frame_means_no_borders <- round(min(dist_frame_means_no_borders),3); min_dist_frame_means_no_borders
max_dist_frame_means_no_borders <- round(max(dist_frame_means_no_borders),3); max_dist_frame_means_no_borders
outlier_dist_frame_means_no_borders <- round(boxplot.stats(dist_frame_means_no_borders)$out,3); outlier_dist_frame_means_no_borders

mean_simi_frame_means_no_borders <- round(mean(simi_frame_means_no_borders),3); mean_simi_frame_means_no_borders
min_simi_frame_means_no_borders <- round(min(simi_frame_means_no_borders),3); min_simi_frame_means_no_borders
max_simi_frame_means_no_borders <- round(max(simi_frame_means_no_borders),3); max_simi_frame_means_no_borders
outlier_simi_frame_means_no_borders <- round(boxplot.stats(simi_frame_means_no_borders)$out,3); outlier_simi_frame_means_no_borders

min_dist_col <- c()
max_dist_col<- c()
mean_dist_col<- c()
out_dist_col <- c()

for(i in 4:35) {
  min_dist_col <- c(min_dist_col,round(min(dist_mat_no_borders[,i]),3))
  max_dist_col <- c(max_dist_col,round(max(dist_mat_no_borders[,i]),3))
  mean_dist_col <- c(mean_dist_col,round(mean(dist_mat_no_borders[,i]),3))
  out_dist_col <-   c(out_dist_col, round(boxplot.stats(dist_mat_no_borders[,i])$out,3))
}

col_meas <- cbind(min_dist_col, max_dist_col, mean_dist_col)
rownames(col_meas) <- c(1:32)
colnames(col_meas) <- c("Min", "Max", "Mean")
col_meas

for (i in 1:32){
  print(paste(i,out_dist_col[i]))
}

round(boxplot.stats(dist_mat_no_borders[,10])$out,3)

#write.csv(col_meas, file = "Data/col_meas.csv")
