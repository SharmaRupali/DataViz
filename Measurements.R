# DISTANCES

#colors
round(min(dist_mat_no_borders[,4:35]),3)
round(max(dist_mat_no_borders[,4:35]),3)
round(mean(dist_mat_no_borders[,4:35]),3)
median(dist_mat_no_borders[,4:35])
length(boxplot.stats(dist_mat_no_borders[,4:35])$out)

# cards
round(min(dist_frame_means_no_borders),3)
round(max(dist_frame_means_no_borders),3)
round(mean(dist_frame_means_no_borders),3)
median(dist_frame_means_no_borders)

# colors
min_dist_col <- c()
max_dist_col<- c()
mean_dist_col<- c()
med_dist_col<- c()
out_dist_col <- list()

for(i in 4:35) {
  min_dist_col <- c(min_dist_col,round(min(dist_mat_no_borders[,i]),3))
  max_dist_col <- c(max_dist_col,round(max(dist_mat_no_borders[,i]),3))
  mean_dist_col <- c(mean_dist_col,round(mean(dist_mat_no_borders[,i]),3))
  med_dist_col <- c(med_dist_col,median(dist_mat_no_borders[,i]))
  out_dist_col[[i-3]] <- round(boxplot.stats(dist_mat_no_borders[,i])$out,3)
}

for(i in 1:32) {
  print(med_dist_col[i])
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


### Sheets
min_dist_sheet <- c()
max_dist_sheet <- c()
mean_dist_sheet <- c()
median_dist_sheet <- c()
out_dist_sheet <- list()

for(i in 1:13) {
  min_dist_sheet <- c(min_dist_sheet, round(min(dist_frame_means_no_borders[,i]), 3))
  max_dist_sheet <- c(max_dist_sheet, round(max(dist_frame_means_no_borders[,i]), 3))
  mean_dist_sheet <- c(mean_dist_sheet, round(mean(dist_frame_means_no_borders[,i]), 3))
  median_dist_sheet <- c(median_dist_sheet, median(dist_frame_means_no_borders[,i]))
  out_dist_sheet[[i]] <- round(boxplot.stats(dist_frame_means_no_borders[,i])$out, 3)
}

sheet_dist_means <- cbind(min_dist_sheet, max_dist_sheet, mean_dist_sheet, median_dist_sheet, out_dist_sheet)
rownames(sheet_dist_means) <- c(1:13)
colnames(sheet_dist_means) <- c("Min", "Max", "Mean", "Median", "No. Outliers")
sheet_dist_means
# write.csv(sheet_dist_means, "sheet_dist_means.csv")

# sheet distance outliers
for (i in 1:13){
  print(paste('-', i, '-'))
  print(length(boxplot.stats(dist_frame_means_no_borders[,i])$out))
}



# SIMILARITIES

# colors
round(min(simi_mat_no_borders[,4:35]),3)
round(max(simi_mat_no_borders[,4:35]),3)
round(mean(simi_mat_no_borders[,4:35]),3)
median(simi_mat_no_borders[,4:35])
length(boxplot.stats(simi_mat_no_borders[,4:35])$out)

# cards
round(min(simi_frame_means_no_borders),3)
round(max(simi_frame_means_no_borders),3)
round(mean(simi_frame_means_no_borders),3)
median(simi_frame_means_no_borders)

# colors
min_simi_col <- c()
max_simi_col<- c()
mean_simi_col<- c()
med_simi_col<- c()
out_simi_col <- list()

for(i in 4:35) {
  min_simi_col <- c(min_simi_col,round(min(simi_mat_no_borders[,i]),3))
  max_simi_col <- c(max_simi_col,round(max(simi_mat_no_borders[,i]),3))
  mean_simi_col <- c(mean_simi_col,round(mean(simi_mat_no_borders[,i]),3))
  med_simi_col <- c(med_simi_col,median(simi_mat_no_borders[,i]))
  out_simi_col[[i-3]] <- round(boxplot.stats(simi_mat_no_borders[,i])$out,3)
}

for(i in 1:32) {
  print(med_simi_col[i])
}

med_simi_col
col_simi_meas <- cbind(min_simi_col, max_simi_col, mean_simi_col)
rownames(col_simi_meas) <- c(1:32)
colnames(col_simi_meas) <- c("Min", "Max", "Mean", "Median")
col_simi_meas
length(mean_simi_col)
round(median(simi_mat_no_borders[,5]),3)
#write.csv(col_simi_meas, "Data/col_simi_meas.csv")

# color similarity outliers
for (i in 1:32){
  print(paste('-', i, '-'))
  print(length(boxplot.stats(simi_mat_no_borders[,i+3])$out))
}

max_simi_col <- c(max_simi_col,round(max(simi_mat_no_borders[,i]),3))

round(max(simi_mat_no_borders[,4:35]),3)
round(min(simi_mat_no_borders[,4:35]), 3)  
round(mean(simi_mat_no_borders[,4:35]),3)
round(boxplot.stats(simi_mat_no_borders[,4:35])$out,3)
boxplot.stats(simi_mat_no_borders[,4:35])$out


### Sheets
min_simi_sheet <- c()
max_simi_sheet <- c()
mean_simi_sheet <- c()
median_simi_sheet <- c()
out_simi_sheet <- list()

for(i in 1:13) {
  min_simi_sheet <- c(min_simi_sheet, round(min(simi_frame_means_no_borders[,i]), 3))
  max_simi_sheet <- c(max_simi_sheet, round(max(simi_frame_means_no_borders[,i]), 3))
  mean_simi_sheet <- c(mean_simi_sheet, round(mean(simi_frame_means_no_borders[,i]), 3))
  median_simi_sheet <- c(median_simi_sheet, median(simi_frame_means_no_borders[,i]))
  out_simi_sheet[[i]] <- round(boxplot.stats(simi_frame_means_no_borders[,i])$out, 3)
}

sheet_simi_means <- cbind(min_simi_sheet, max_simi_sheet, mean_simi_sheet, median_simi_sheet, out_simi_sheet)
rownames(sheet_simi_means) <- c(1:13)
colnames(sheet_simi_means) <- c("Min", "Max", "Mean", "Median", "No. Outliers")
sheet_simi_means
# write.csv(sheet_simi_means, "sheet_simi_means.csv")

# sheet distance outliers
for (i in 1:13){
  print(paste('-', i, '-'))
  print(length(boxplot.stats(simi_frame_means_no_borders[,i])$out))
}

