exploratory_maps<- function(data){
  # data is a tibble with a column named 'data', 
  # note: one row per grid_cell
  mk_from_list<- function(x) pw_fn(x$temp)
  sen_from_list<- function(x) sen0(pw_only_fn(x$temp))
  var_from_list<- function(x) var(pw_only_fn(x$temp))
  rho_from_list<- function(x) rk_fn(x$temp)
  
  data$mk_z_stat<- mapply(mk_from_list, data$data)
  data$sen<- mapply(sen_from_list, data$data)
  data$var<- mapply(var_from_list, data$data)
  data$snr<- abs(data$sen/sqrt(data$var)) # signal to noise ratio 
  data$rho<- mapply(rho_from_list, data$data)
  
  data
}