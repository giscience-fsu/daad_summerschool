################################################################################
#        Dealing with multiple testing problems in spatial trend analysis      #
################################################################################

# In this script we generate the images shown in the presentation.pdf
# and we show how to correct for multiple testing with several methods

#source libraries and functions (all in lib folder)
################################################################################
sapply(list.files("lib/", full.names = TRUE), source)


# load data 
################################################################################
temp_gistemp<- readRDS("data/temp_anomalies_1951_2018.rds")
str(temp_gistemp)


# select a random pixel
################################################################################
lon<- sample(dim(temp_gistemp)[1],1)
lat<- sample(dim(temp_gistemp)[2],1)
random_pixel<- temp_gistemp[lon, lat,] %>% 
  data.frame(anomaly = ., year = as.numeric(names(.)))

figure1<- random_pixel %>% 
  ggplot(data = ., aes(x = year, y = anomaly)) + 
  geom_line(size = 1.1) + 
  geom_point(size = 3) + 
  xlab("Years") + 
  ylab("Anomaly") + 
  ggtitle("Random pixel", subtitle = paste("lon =", lon, ", lat =", lat)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 21))
# save image as jpeg
jpeg("../figures/random_pixel.jpg", width = 800, height = 600)
figure1
dev.off()

# empirical and theoretical distribution of test statistic 
################################################################################

# theoretical is a standard normal distribution N(mean = 0, variance = 1)

# empirical distribution is derived with permutations
# steps: 
# (1) sample data (without replacement)
# (2) calculate test statistic
# (3) repeat N times (999 is usually enough)
# Original test statistic + 999 permutations make the empirical dsitribution
# of our test statistic

perm_S<- vector(length = 999)
for (i in 1:length(perm_S)) {
  tmp<- sample(random_pixel$anomaly, 
               length(random_pixel$anomaly))
  perm_S[i]<- mk_z_stat_von_storch_fn(tmp)
}
orig_S<- mk_z_stat_von_storch_fn(random_pixel$anomaly)
S_distr<- data.frame(S = c(perm_S, orig_S))

figure2<- ggplot(S_distr, aes(x = S)) + 
  geom_histogram(aes(y = ..density..) ,color = "grey", fill = "grey") +
  stat_function(fun = dnorm, n = 100, args = list(mean = 0, sd = 1), color = "red", size = 1.1) + 
  geom_vline(xintercept = c(qnorm(.025), qnorm(.975), orig_S, quantile(S_distr$S, c(0.025,.975))), 
             linetype = c("dashed", "dashed", "solid", "dashed", "dashed"), 
             color = c("red", "red", "black", "grey", "grey"), size = 1.1) + 
  ggtitle("Empirical and theoretical distribution of Z", subtitle = paste("lon =", lon, ", lat =", lat)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 21))

jpeg("../figures/distr_S.jpg", width = 800, height = 400)
figure2
dev.off()
  
# Now we move to a global scale
################################################################################

#maps of yearly temperature
################################################################################
yearly_map<- function(x, t){
  lon = as.numeric(dimnames(x)$lon)
  lat = as.numeric(dimnames(x)$lat)
  
  tmp<- data.frame(expand.grid(lon,lat), z= as.vector(x[,,t]))
  names(tmp)<- c("x","y","temp")                   
  
  tmp<- tmp %>% reraster(categorical = FALSE) %>% 
    make_maps(palette = "-RdBu", n = 6, title = "anomalies", legend.hist = F) %>% 
    .[[1]] + tm_layout(main.title = 1950+t, main.title.size = 3, 
                       main.title.position = "center", scale = 3)
  jpeg(paste0("../figures/temp_gif/temp_",1950+t, ".jpg"), width = 1600, height = 800)
  print(tmp)
  dev.off()
  return(tmp)
}
temp_maps<- vector(mode = "list", length = dim(temp_gistemp)[3])
for(i in 1:length(temp_maps)) temp_maps[[i]]<- yearly_map(temp_gistemp,i)
temp_maps[[2]]
# note scales are not equal


#exploratory maps
################################################################################
# nicer data format
temp_gistemp<- as_tibble(melt(temp_gistemp, value.name = "temp")) %>% group_by(lon, lat,t) %>%
  dplyr::summarise(temp = mean(temp, na.rm = TRUE)) %>%
  # mutate(temp = ifelse(!is.finite(temp), NA, temp)) %>% # for looking at min/max
  group_by(lon, lat) %>%
  filter(sum(is.na(temp)) == 0) %>% 
  nest()
temp_gistemp

temp_gistemp<- exploratory_maps(temp_gistemp)

temp_gistemp

# permutation method
################################################################################

temp_gistemp_R <- temp_gistemp %>% dplyr::select(-data) %>%
  dplyr::select(x = lon, y = lat, mk_z_stat) %>%
  rasterFromXYZ(crs = CRS("+init=epsg:4326"))

temp_analysis_format<- raster_to_list_fn(temp_gistemp_R)
temp_analysis_format$Y<- tibble_list_to_matrix(temp_gistemp)
NPERM<- 1000
BLOCK_SIZE<- NULL
ALPHA = 0.05
# NCORES <- 45
temp_analysis_format$Y<- temp_analysis_format$Y %>% apply(2, pw_only_fn)
# temp_res<- perm_distr_trend_fn(temp_analysis_format)
# save(temp_res, file = "data/temp_res.R")

# analysis takes too long, we just load the results
load("data/temp_res.R")

# data's result
temp_res %>% lapply(function(x) x[length(x)])

# threshold from the permutation distribution
temp_res %>% lapply(function(x) quantile(x, probs = 1-ALPHA))

# histograms (of the max statistic)
temp_res %>% lapply(hist)

# significance tests 
################################################################################

pval<- 2*pnorm(unname(unlist(-abs(temp_gistemp['mk_z_stat']))))

temp_gistemp$no_corr<- abs(temp_gistemp$mk_z_stat) > qnorm(ALPHA/2, lower.tail = F)
temp_gistemp$bonferroni<- bonf_fn(pval)
temp_gistemp$walker<- walker_fn(pval)
temp_gistemp$hochberg<- hochberg_fn(pval)
temp_gistemp$holmes<- holm_fn(pval)
temp_gistemp$by<- by_fn(pval)
temp_gistemp$bh<- bh_fn(pval)
temp_gistemp$perm<- abs(temp_gistemp$mk_z_stat) > quantile(temp_res$mk_stat, probs = 1- ALPHA)

clusters<- max_cluster_fn(unname(unlist(temp_gistemp['mk_z_stat'])) , all.results = TRUE, my_data = temp_analysis_format)
stcs_thr<- quantile(temp_res$mk_max_cluster, 1-ALPHA)
wh_cluster<- which(clusters$clusters$cluster.count > stcs_thr)
wh_cluster_sel<- clusters$clusters$clusters %in% wh_cluster

temp_gistemp$stcs<- wh_cluster_sel[temp_analysis_format$wh.sel]
temp_gistemp

# count of significant pixels
################################################################################

temp_gistemp %>% dplyr::select(9:17) %>% apply(2, sum)

# maps!
############################################################################

temp_gistemp_R <- temp_gistemp %>% dplyr::select(-data) %>%
  reraster(categorical  = FALSE)

temp_gistemp_R_maps<- make_maps(temp_gistemp_R)
map_names<- names(temp_gistemp_R_maps)

# exploratory maps
a<- temp_gistemp_R$mk_z_stat %>%  make_maps(palette = "-RdBu", n = 6, title = "", legend.hist = F) %>% .[[1]]
b<- temp_gistemp_R$sen %>% make_maps(palette = "-RdBu", n = 6, title = "", legend.hist = F) %>%  .[[1]]
c<- temp_gistemp_R$snr %>% make_maps(palette = "Blues", n = 6, title = "", legend.hist = F)%>%  .[[1]]
d<- temp_gistemp_R$rho %>% make_maps(palette = "RdBu", n = 6, title = "", legend.hist = F) %>%  .[[1]]

pdf(paste0('../figures/MK_exploratory_maps.pdf'), width = 6, height = 8)
tmap_arrange(a + tm_layout(main.title = "MK Z stat", main.title.size = 1, 
                           main.title.position = "center", legend.text.size	= 10),
             b + tm_layout(main.title = "Sen slope", main.title.size = 1, 
                           main.title.position = "center"),
             c + tm_layout(main.title = "SNR", main.title.size = 1, 
                           main.title.position = "center"),
             d + tm_layout(main.title = "Rho", main.title.size = 1, 
                           main.title.position = "center"),
             nrow = 2)
dev.off()

# analysis maps
names<- names(temp_gistemp_R_maps)[6:length(names(temp_gistemp_R_maps))]
for(name in names) temp_gistemp_R_maps[[name]]<- remap(temp_gistemp, name)

pdf(paste0('../figures/analysis_nasa_gistemp.pdf'), width = 6, height = 8)
tmap_arrange(temp_gistemp_R_maps$bonferroni + tm_legend(show = FALSE) +
               tm_layout(main.title = "Bonferroni",main.title.size = 1, main.title.position = "center"),
             temp_gistemp_R_maps$walker + tm_legend(show = FALSE) +
               tm_layout(main.title = "Walker",main.title.size = 1, main.title.position = "center"), 
             temp_gistemp_R_maps$hochberg + tm_legend(show = FALSE) +
               tm_layout(main.title = "Hochberg",main.title.size = 1, main.title.position = "center"), 
             temp_gistemp_R_maps$holmes + tm_legend(show = FALSE) +
               tm_layout(main.title = "Holm",main.title.size = 1, main.title.position = "center"), 
             temp_gistemp_R_maps$by + tm_legend(show = FALSE) +
               tm_layout(main.title = "BY",main.title.size = 1, main.title.position = "center"),
             temp_gistemp_R_maps$bh + tm_legend(show = FALSE) +
               tm_layout(main.title = "BH",main.title.size = 1, main.title.position = "center"),
             temp_gistemp_R_maps$perm + tm_legend(show = FALSE) +
               tm_layout(main.title = "max T",main.title.size = 1, main.title.position = "center"),
             temp_gistemp_R_maps$stcs + tm_legend(show = FALSE) +
               tm_layout(main.title = "STCS",main.title.size = 1, main.title.position = "center"),
             nrow = 4)
dev.off()

# The end!
################################################################################