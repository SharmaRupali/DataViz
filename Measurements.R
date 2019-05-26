# DISTANCES

# cards
mean_dist_frame_means_no_borders <- round(mean(dist_frame_means_no_borders),3); mean_dist_frame_means_no_borders
min_dist_frame_means_no_borders <- round(min(dist_frame_means_no_borders),3); min_dist_frame_means_no_borders
max_dist_frame_means_no_borders <- round(max(dist_frame_means_no_borders),3); max_dist_frame_means_no_borders
outlier_dist_frame_means_no_borders <- round(boxplot.stats(dist_frame_means_no_borders)$out,3); outlier_dist_frame_means_no_borders

# colors
min_dist_col <- c()
max_dist_col<- c()
mean_dist_col<- c()
out_dist_col <- list()

for(i in 4:35) {
  min_dist_col <- c(min_dist_col,round(min(dist_mat_no_borders[,i]),3))
  max_dist_col <- c(max_dist_col,round(max(dist_mat_no_borders[,i]),3))
  mean_dist_col <- c(mean_dist_col,round(mean(dist_mat_no_borders[,i]),3))
  out_dist_col[[i-3]] <- round(boxplot.stats(dist_mat_no_borders[,i])$out,3)
}

col_dist_meas <- cbind(min_dist_col, max_dist_col, mean_dist_col)
rownames(col_dist_meas) <- c(1:32)
colnames(col_dist_meas) <- c("Min", "Max", "Mean")
col_dist_meas
#write.csv(col_dist_meas, "Data/col_dist_meas.csv")

# color distance outliers
for (i in 1:32){
  print(paste('-', i, '-'))
  print(length(boxplot.stats(dist_mat_no_borders[,i+3])$out))
}


# SIMILARITIES

# cards
mean_simi_frame_means_no_borders <- round(mean(simi_frame_means_no_borders),3); mean_simi_frame_means_no_borders
min_simi_frame_means_no_borders <- round(min(simi_frame_means_no_borders),3); min_simi_frame_means_no_borders
max_simi_frame_means_no_borders <- round(max(simi_frame_means_no_borders),3); max_simi_frame_means_no_borders
outlier_simi_frame_means_no_borders <- round(boxplot.stats(simi_frame_means_no_borders)$out,3); outlier_simi_frame_means_no_borders

# colors
min_simi_col <- c()
max_simi_col<- c()
mean_simi_col<- c()
out_simi_col <- list()

for(i in 4:35) {
  min_simi_col <- c(min_simi_col,round(min(simi_mat_no_borders[,i]),3))
  max_simi_col <- c(max_simi_col,round(max(simi_mat_no_borders[,i]),3))
  mean_simi_col <- c(mean_simi_col,round(mean(simi_mat_no_borders[,i]),3))
  out_simi_col[[i-3]] <- round(boxplot.stats(simi_mat_no_borders[,i])$out,3)
}

col_simi_meas <- cbind(min_simi_col, max_simi_col, mean_simi_col)
rownames(col_simi_meas) <- c(1:32)
colnames(col_simi_meas) <- c("Min", "Max", "Mean")
col_simi_meas
#write.csv(col_simi_meas, "Data/col_simi_meas.csv")

# color similarity outliers
for (i in 1:32){
  print(paste('-', i, '-'))
  print(length(boxplot.stats(simi_mat_no_borders[,i+3])$out))
}


       