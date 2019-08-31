max_cluster_fn<- function(t_stat,all.results = all.results,my_data = my_data){
  # returns count of pixels in max cluster....
  # input also my_data object from list_to_matrix_fn
  # added: save test statistic 
  if(missing(my_data)) my_data<- list(wh.sel = expand.grid(1:GRID_SIZE, 1:GRID_SIZE), nrow = GRID_SIZE, ncol = GRID_SIZE)
  thr = qnorm(1-ALPHA/2)
  if(is.list(t_stat)) t_stat<- unname(unlist(t_stat)) # compatibility issues for tibbles
  pixel_sign<- sign(t_stat)
  pixel_significant<- abs(t_stat)>thr
  pixel_result<- pixel_sign*pixel_significant
  
  pixel_result_matrix <- pixel_result_matrix_pos<- pixel_result_matrix_neg<- matrix(NA, nrow=my_data$nrow, ncol=my_data$ncol)
  pixel_result_matrix[my_data$wh.sel]<- pixel_result_matrix_pos[my_data$wh.sel]<- pixel_result_matrix_neg[my_data$wh.sel]<- pixel_result
  
  # using the clustering algorithm in osc package
  
  # Get rid of NA's by setting == -999, function doesnt like negative values, so change -1 to 10
  # only clusters of same sign
  
  # positive
  pixel_result_matrix_pos[is.na(pixel_result_matrix_pos)] = -999
  clusters_pos<- cca(pixel_result_matrix_pos,count.cells = TRUE, s=1, mode = 2)
  max_cluster_size_pos<- max(clusters_pos$cluster.count)
  
  n_pos_clust<- max(as.vector(clusters_pos$clusters)) # max cluster label
  clusters_sep<- vector(mode = "list", length = 2)
  
  # negative
  pixel_result_matrix_neg[pixel_result_matrix_neg == -1] = 10
  pixel_result_matrix_neg[pixel_result_matrix_neg == 1] = -10
  pixel_result_matrix_neg[is.na(pixel_result_matrix_neg)] = -999
  clusters_neg<- cca(pixel_result_matrix_neg,count.cells = TRUE, s=1, mode = 2)
  max_cluster_size_neg<- max(clusters_neg$cluster.count)
  
  clusters_neg$clusters[clusters_neg$clusters > 0]<- clusters_neg$clusters[clusters_neg$clusters > 0] + n_pos_clust
  
  clusters_sep[[1]]<- clusters_pos$clusters + clusters_neg$clusters
  clusters_sep[[2]]<- c(clusters_pos$cluster.count, clusters_neg$cluster.count)
  names(clusters_sep)<- c("clusters", "cluster.count")
  
  max_cluster_size_sep<- max(clusters_sep$cluster.count)
  
  if(all.results) out<- list(clusters = clusters_sep,t_stat = t_stat)
  if(!all.results) out<- max_cluster_size_sep
  return(out)
} 
