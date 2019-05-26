
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

