# summarize dung data for Kenya Ticks
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()
print_theme_l <- theme_bw() + theme(axis.title=element_text(size=22),
                                    axis.text=element_text(size=22))

count <- function(values){
  return(length(values))
}

# OPC data
metadata_csv <- "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_metadata.csv"
PDM_csv <- "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_PDM_dung.csv"

meta_df <- read.csv(metadata_csv)
dung_df <- read.csv(PDM_csv)
dung_df <- dung_df[which(dung_df$Position_m > 0), ]

dung_df[, 9:32][is.na(dung_df[, 9:32])] <- 0

dung_ids <- unique(dung_df[, c("Date", "Site")])

# make sure there are 10 samples in each transect
sample_count <- aggregate(dung_df$PDM, by=list(dung_df$transect), FUN=length)
trouble <- sample_count[which(sample_count$PDM != 10), ]  # should be 0 rows

dung_sum <- aggregate(dung_df[, 9:32], by=list(dung_df$transect), FUN=sum)
colnames(dung_sum)[1] <- 'transect'

meta_subs <- meta_df[, c('transect', 'Lat', 'Long')]
date_only <- dung_df[, c('transect', 'Date')]
date_only <- date_only[!duplicated(date_only[, 'transect']), ]

comb_df <- merge(dung_sum, meta_subs, by='transect')
comb_df <- merge(comb_df, date_only, by='transect')
comb_df$bovid <- rowSums(comb_df[, c(2, 4)])
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/dung_summary.csv"
write.csv(comb_df, file=save_as, row.names=FALSE)

# dung around weather stations summarized in python 
outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/OPC_weather_stations"
points_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/OPC_weather_stations_coordinates.csv"
group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)

# bovids, all transects around weather station 2.15.17
points_df <- read.csv(points_file)
df_list <- list()
for(stn in points_df$Name){
  dung_file <- paste(outerdir, paste("dung_2.0km_", stn, ".csv", sep=""), sep="/")
  df <- read.csv(dung_file)
  df$bovid <- df$Buf + df$Cow
  df_sub <- df[, c('transect', 'Date', 'bovid')]
  df_sub$site <- stn
  df_list[[stn]] <- df_sub
}
combined <- do.call(rbind, df_list)
write.csv(combined, "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/bovid_dung_weather_2km.csv",
          row.names=FALSE)

# weather station means
points_df <- read.csv(points_file)
df_list <- list()
for(stn in points_df$Name){
  dung_file <- paste(outerdir, paste("dung_2.0km_", stn, ".csv", sep=""), sep="/")
  df <- read.csv(dung_file)
  df_sub <- df[, c(3:26)]
  df_means <- as.data.frame.list(colMeans(df_sub))
  df_means$site <- stn
  df_means$n_transect <- dim(df_sub)[1]
  df_list[[stn]] <- df_means
}
combined <- do.call(rbind, df_list)

# test excluding buffalo and cattle
combined <- combined[, c(-1, -3)]
means_t <- as.data.frame(t(combined[, c(1:22)]))

# test combining buffalo and cattle
combined$bovid <- combined$Buf + combined$Cow

means_t <- as.data.frame(t(combined[, c(1:24, 27)]))
means_t$Abbrev <- rownames(means_t)
colnames(means_t)[which(colnames(means_t) == ' Serat gate')] <- "Serat"
colnames(means_t)[which(colnames(means_t) == 'Loirugurugu')] <- "Loirugu"
colnames(means_t)[which(colnames(means_t) == 'Rongai gate')] <- "Rongai"
colnames(means_t)[which(colnames(means_t) == 'Golf 7')] <- "Golf_7"
colnames(means_t)[which(colnames(means_t) == 'Simira')] <- "Sirima"

gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group4', 'Group3')]
comb <- merge(means_t, gr_subs, by='Abbrev')

gr1_means <- aggregate(comb[, 2:11], by=list(comb$Group1), FUN=sum)
gr2_means <- aggregate(comb[, 2:11], by=list(comb$Group4), FUN=sum)
gr3_means <- aggregate(comb[, 2:11], by=list(comb$Group3), FUN=sum)
colnames(gr1_means)[1] <- "group"
colnames(gr2_means)[1] <- "group"
colnames(gr3_means)[1] <- "group"

# reshape for plotting
gr1_res <- reshape(gr1_means, idvar="group", varying=list(2:11), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:11], direction="long",
                   new.row.names=1:1000)
gr2_res <- reshape(gr2_means, idvar="group", varying=list(2:11), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:11], direction="long",
                   new.row.names=1:1000)
gr3_res <- reshape(gr3_means, idvar="group", varying=list(2:11), timevar='site',
                   v.names="mean_dung", times=colnames(gr3_means)[2:11], direction="long",
                   new.row.names=1:1000)

gr1_plot <- gr1_res[which(gr1_res$group == 'non-cattle'), ]
gr1_plot <- gr1_plot[which(gr1_plot$site %in% c('Kamok', 'Loidien',
                                                'Loirugurugu', 'Research',
                                                'Rongai gate', ' Serat gate')), ]
p <- ggplot(gr1_plot, aes(x=site, y=mean_dung))
p <- p + geom_point()
print(p)

gr2_plot <- gr2_res[which(gr2_res$group != 'cattle'), ]
gr2_plot <- gr2_plot[which(gr2_plot$site %in% c('Kamok', 'Loidien',
                                                'Loirugurugu', 'Research',
                                                'Rongai gate', ' Serat gate')), ]
p <- ggplot(gr2_plot, aes(x=site, y=mean_dung, group=group))
p <- p + geom_point(aes(colour=group))
print(p)

# compare different functional groups to back-calculated grazing intensity
result_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/comparison_w_empirical_density/correlation_w_dung"
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/back_calc_match_last_measurement/summary_figs/bc_12_mo_intensity.csv"
c_df <- read.csv(comparison_csv)

g1_df <- gr1_res[which(gr1_res$group %in% c('browser', 'grazer', 'carnivore', 'mixed')), ]
g2_df <- gr2_res
g3_df <- gr3_res

fig_dir <- paste(result_dir, "figs", sep="/")
sum_df <- data.frame('group'=character(), 'spearman_rho'=numeric(),
                     'spearman_pval'=numeric(), 'pearson_cor'=numeric(),
                     'pearson_pval'=numeric(), stringsAsFactors=FALSE)
i <- 1
dung_join_df <- g1_df
gr <- 'grazer'
for(gr in unique(dung_join_df$group)){
  sub_df <- dung_join_df[which(dung_join_df$group == gr), ]
  joined <- merge(sub_df, c_df, by="site")
  p <- ggplot(joined, aes(x=total_rem, y=mean_dung))
  p <- p + geom_point()
  p <- p + xlab("Modeled grazing intensity") + ylab(paste(gr, " dung per transect", sep=""))
  p <- p + ylab("Grazer (ex. bovid) dung per transect")
  p <- p + print_theme
  pngname <- paste(fig_dir, paste(gr, "ex_bovid_x_back_calc.png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=5, height=5)
  print(p)
  dev.off()
  sp_test <- cor.test(joined$mean_dung, joined$total_rem, method="spearman")
  pe_test <- cor.test(joined$mean_dung, joined$total_rem, method="pearson")
  sum_df[i, 'group'] <- gr
  sum_df[i, 'spearman_rho'] <- sp_test[[4]]
  sum_df[i, 'spearman_pval'] <- sp_test[[3]]
  sum_df[i, 'pearson_cor'] <- pe_test[[4]]
  sum_df[i, 'pearson_pval'] <- pe_test[[3]]
  i <- i + 1
}
save_as <- paste(result_dir, "correlation_summary.csv", sep="/")
write.csv(sum_df, save_as, row.names=FALSE)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/comparison_w_empirical_density/correlation_w_dung/figs/bovid_x_back_calc.png"
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

p <- ggplot(c_df, aes(x=total_rem_back.calc, y=average_monthly_GPS_density))
p <- p + geom_point()
p <- p + xlab("Back-calc grazing intensity") + ylab("GPS-derived cattle density")
pngname <- paste(fig_dir, "GPS_density_x_back_calc.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()
test <- cor.test(c_df$average_monthly_GPS_density, c_df$total_rem_back.calc, method="spearman")
sum_df[6, 'group'] <- 'GPS-derived cattle density'
sum_df[6, 'rho'] <- test[[4]]
sum_df[6, 'p'] <- test[[3]]
  
test <- cor.test(joined$mean_dung, joined$total_rem_back.calc, method="spearman")
sum_df[7, 'group'] <- 'cattle'
sum_df[7, 'rho'] <- test[[4]]
sum_df[7, 'p'] <- test[[3]]

filename <- paste(result_dir, "correlation_summary.csv", sep="/")
write.csv(sum_df, filename, row.names=FALSE)

# summarize OPC dung data for calculating correlation with GPS-derived stocking density
dung_subs <- dung_sum[, c('transect', 'Buf', 'Cow')]
dung_subs$bovid <- rowSums(dung_subs[, c(2, 3)])

meta_subs <- meta_df[, c('transect', 'Lat', 'Long')]
date_only <- dung_df[, c('transect', 'Date')]
date_only <- date_only[!duplicated(date_only[, 'transect']), ]
for (r in (1:NROW(date_only))){
  date <- as.Date(date_only[r, "Date"], format="%d-%b-%y")
  date_only[r, 'year_month'] <- format(date, "%Y_%m")
}

comb_df <- merge(dung_subs, meta_subs, by='transect')
comb_df <- merge(comb_df, date_only, by='transect')
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_bovid_dung_sum.csv"
write.csv(comb_df, file=save_as, row.names=TRUE)

comb_df <- read.csv(save_as)

# Moran's I: spatial autocorrelation in dung density
imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/semivariograms"
library(ape)
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_bovid_dung_sum.csv"
comb_df <- read.csv(save_as)

# create distance matrix
transect_dists <- as.matrix(dist(cbind(comb_df$Long, comb_df$Lat)))
transect_dists_inv <- 1/transect_dists
diag(transect_dists_inv) <- 0
transect_dists_inv[is.infinite(transect_dists_inv)] <- 25000 # max(transect_dists_inv) = 23570.23
Moran.I(comb_df$bovid, transect_dists_inv)
Moran.I(comb_df$Cow, transect_dists_inv)

# Moran's I with spdep
library(spdep)
dists_listw <- mat2listw(transect_dists_inv)
moran.mc(comb_df$bovid, dists_listw, nsim=9999)
moran.mc(comb_df$Cow, dists_listw, nsim=9999)

# semivariogram
library(geoR)
dists <- dist(cbind(comb_df$Long, comb_df$Lat))
shortd <- dists[which(dists < max(dists)/2)]
hist(shortd)  # look for lag distances with >= 30 points in each bin
hist(shortd, breaks=50)
breaks <- seq(0, max(dists)/2, length.out=15)
cow_geodat <- jitterDupCoords(as.geodata(comb_df, coords.col=5:6, data.col=3),
                              max=0.0001)
variogram <- variog(cow_geodat, breaks=breaks, option="cloud")
plot(variogram, main='Cow dung')
pngname <- paste(imgdir, "cow_dung_semivariogram_cloud.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
plot(variogram, main='Cow dung')
dev.off()
variogram <- variog(cow_geodat, breaks=breaks, option="bin")
pngname <- paste(imgdir, "cow_dung_semivariogram_bin.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
plot(variogram, type="b", main='Cow dung')
dev.off()
breaks <- seq(0, max(dists), length.out=30)
variogram <- variog(cow_geodat, breaks=breaks, option="bin")
pngname <- paste(imgdir, "cow_dung_semivariogram_bin_maxdist.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
plot(variogram, type="b", main='Cow dung')
dev.off()

# semivariogram by month

for(ym in unique(comb_df$year_month)){
  subdf <- comb_df[which(comb_df$year_month == ym), ]
  dists <- dist(cbind(subdf$Long, subdf$Lat))
  shortd <- dists[which(dists < max(dists)/2)]
  pngname <- paste(imgdir, paste("cow_dung_hist_maxdist_",
                                 ym, ".png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=7, height=5)
  hist(shortd, main=ym, breaks=10) 
  dev.off()
  breaks <- seq(0, max(dists), length.out=10)
  cow_geodat <- jitterDupCoords(as.geodata(subdf, coords.col=4:5, data.col=3),
                                max=0.0001)
  variogram <- variog(cow_geodat, breaks=breaks, option="bin")
  pngname <- paste(imgdir, paste("cow_dung_semivariogram_bin_maxdist_",
                                 ym, ".png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=7, height=5)
  plot(variogram, type="b", main=paste('Cow dung ', ym, sep=""))
  dev.off()
}


# cattle density calculated from GPS
density_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/correlation_with_GPS_records"

# summarize comparison between dung and GPS-derived density
summary_df <- data.frame('distance (km)'=numeric(6), 'days_lag'=numeric(6),
                         'num_obs_densities'=numeric(6), 'rho_cow'=numeric(6),
                         'rho_bovid'=numeric(6), 'p_cow'=numeric(6),
                         'p_bovid'=numeric(6), 'rho_buf'=numeric(6),
                         'p_buf'=numeric(6))
i <- 1
for(distance in c(0.1, 0.3, 0.5)){
  for(days_lag in c(28, 35)){
    density_file <- paste(density_dir, paste("GPS_density_", distance,
                                             "km_", days_lag, "days.csv", sep=""),
                          sep="/")
    dens_df <- read.csv(density_file)
    dens_df$total_anim <- rowSums(dens_df[, c(1:6, 8)])
    cor_df <- merge(comb_df, dens_df, by='row_id', all=TRUE)
    num_non_na <- dim(cor_df[!is.na(cor_df$total_anim), ])[1]
    cor_df[, 9:16][is.na(cor_df[, 9:16])] <- 0
    if(num_non_na > 1){
      rho_cow = cor.test(cor_df$Cow, cor_df$total_anim, method='spearman')[[4]]
      p_cow = cor.test(cor_df$Cow, cor_df$total_anim, method='spearman')[[3]]
      p <- ggplot(cor_df, aes(x=Cow, y=total_anim))
      p <- p + geom_point()
      p <- p + xlab("Cattle dung") + ylab("GPS-derived animal density")
      p <- p + ggtitle(paste(distance, " km, ", days_lag, " days (cow dung only)", sep=""))
      pngname <- paste(density_dir, 'figs',
                       paste(distance, "km_", days_lag, "days.png", sep=""), sep="/")
      png(file=pngname, units="in", res=300, width=8, height=5)
      print(p)
      dev.off()
      
      rho_buf = cor.test(cor_df$Buf, cor_df$total_anim, method='spearman')[[4]]
      p_buf = cor.test(cor_df$Buf, cor_df$total_anim, method='spearman')[[3]]
      
      rho_bovid = cor.test(cor_df$bovid, cor_df$total_anim, method='spearman')[[4]]
      p_bovid = cor.test(cor_df$bovid, cor_df$total_anim, method='spearman')[[3]]
    }
    else{
      rho_cow = NA
      rho_bovid = NA
    }
    summary_df[i, 'distance'] <- distance
    summary_df[i, 'days_lag'] <- days_lag
    summary_df[i, 'num_obs_densities'] <- num_non_na
    summary_df[i, 'rho_cow'] <- rho_cow
    summary_df[i, 'rho_bovid'] <- rho_bovid
    summary_df[i, 'rho_buf'] <- rho_buf
    summary_df[i, 'p_cow'] <- p_cow
    summary_df[i, 'p_bovid'] <- p_bovid
    summary_df[i, 'p_buf'] <- p_buf
    i <- i + 1
  }
}
save_as <- paste(density_dir, "correlation_summary.csv", sep='/')
write.csv(summary_df, save_as)

distance <- 0.5
days_lag <- 35
density_file <- paste(density_dir, paste("GPS_density_", distance,
                                         "km_", days_lag, "days.csv", sep=""),
                      sep="/")
dens_df <- read.csv(density_file)
dens_df$total_anim <- rowSums(dens_df[, c(1:6, 8)])
cor_df <- merge(comb_df, dens_df, by='row_id', all=TRUE)
num_non_na <- dim(cor_df[!is.na(cor_df$total_anim), ])[1]
cor_df[, 9:16][is.na(cor_df[, 9:16])] <- 0

p <- ggplot(cor_df, aes(x=total_anim, y=Buf))
p <- p + geom_point()
p <- p + xlab("GPS-derived cattle density") + ylab("Buffalo dung")
print(p)

p <- ggplot(cor_df, aes(x=Buf, y=total_anim))
p <- p + geom_point()
p <- p + ylab("GPS-derived cattle density") + xlab("Buffalo dung")
pngname <- paste(density_dir, 'figs', 'buf_dung_vs_GPS_density_0.5_km_35_days.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# comparing grazer density from dung, GPS, and back-calc management
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC_integrated_test/density_summary_GPS_dung_back-calc.csv"
c_df <- read.csv(comparison_csv)

# pairwise correlations
t1 <- cor.test(c_df$total_rem_back.calc, c_df$total_dung, method="spearman")
t2 <- cor.test(c_df$mean_cattle_dung_density_per_transect, c_df$average_monthly_GPS_density,
               method="spearman")
t3 <- cor.test(c_df$total_rem_back.calc, c_df$mean_cattle_dung_density_per_transect,
               method="spearman")

p <- ggplot(c_df, aes(x=total_rem_back.calc, y=total_dung))
p <- p + geom_point()
print(p)

p <- ggplot(c_df, aes(x=mean_cattle_dung_density_per_transect,
                      y=average_monthly_GPS_density))
p <- p + geom_point()
print(p)

p <- ggplot(c_df, aes(x=total_rem_back.calc, y=mean_cattle_dung_density_per_transect))
p <- p + geom_point()
print(p)

### Regional properties: dung
dung_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_dung_2015.csv"
rdung_df <- read.csv(dung_csv)
rdung_df <- rdung_df[which(rdung_df$Position_m > 0), ]
rdung_df <- rdung_df[, c(1:4, 6:29)]
rdung_df[, 5:28][is.na(rdung_df[, 5:28])] <- 0
rdung_df$Date <- as.Date(rdung_df$Date, format="%d-%b-%y")

# make sure there are 10 samples in each transect
rdung_df$unique_id <- paste(rdung_df$Property, rdung_df$Transect, sep="-")
sample_count <- aggregate(Buf~unique_id, data=rdung_df, FUN=count)
trouble <- sample_count[which(sample_count$Buf != 10), ]  # should be 0 rows

# sum within transect
# combine cow and buffalo into bovid
rdung_df$bovid <- rdung_df$Buf + rdung_df$Cow
rdung_df <- rdung_df[ , -which(names(rdung_df) %in% c("Cow","Buf"))]
dung_sum <- aggregate(rdung_df[, c(5:26, 28)], by=list(rdung_df$unique_id), FUN=sum)
colnames(dung_sum)[1] <- 'transect'

# average across transect within property
for (r in (1:NROW(dung_sum))){
  a_list <- unlist(strsplit(dung_sum[r, "transect"], split="-"))
  dung_sum[r, 'property'] <- a_list[1]
}
property_means <- aggregate(dung_sum[, 2:24], by=list(dung_sum$property), FUN=mean)
colnames(property_means)[1] <- "Property"
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_dung_2015_property_means.csv"
write.csv(property_means, save_as, row.names=FALSE)

property_means <- read.csv(save_as)
FID_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Property_FID_match.csv"
FID_list <- read.csv(FID_csv)
property_means <- merge(property_means, FID_list, by.x="Property",
                        by.y="NAME", all.x=TRUE)[, 2:25]

group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)

propmeans_t <- as.data.frame(t(property_means[1:24]))
colnames(propmeans_t) <- property_means$FID
propmeans_t$Abbrev <- rownames(propmeans_t)

gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group2', 'Group3')]
comb <- merge(propmeans_t, gr_subs, by='Abbrev')
comb <- comb[which(comb$Abbrev != "UK"), ]

gr1_means <- aggregate(comb[, 2:25], by=list(comb$Group1), FUN=sum)
colnames(gr1_means)[1] <- "group"
gr3_means <- aggregate(comb[, 2:25], by=list(comb$Group3), FUN=sum)
colnames(gr3_means)[1] <- "group"

# reshape for plotting
gr1_res <- reshape(gr1_means, idvar="group", varying=list(2:25), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:25], direction="long",
                   new.row.names=1:1000)
gr3_res <- reshape(gr3_means, idvar="group", varying=list(2:25), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:25], direction="long",
                   new.row.names=1:1000)

# summary of back-calc intensity
bc_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis/match_2015_intensity_summary.csv"
bc_summary <- read.csv(bc_csv)

# correlations and scatterplots out the wazoo
results_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/Regional_properties_dung_analysis/comparison_with_back_calc_mgmt"
dung_df <-  gr3_res # gr1_res

nrows <- length(unique(dung_df$group)) * 2 * 2
cor_record <- data.frame('bc_source'=character(nrows), 'time_period'=numeric(nrows),
                     'animal_type'=numeric(nrows), 'spearman_rho'=numeric(nrows),
                     'spearman_p_val'=numeric(nrows), 'pearson_cor'=numeric(nrows),
                     'pearson_p_val'=numeric(nrows), stringsAsFactors=FALSE)
i <- 1
for(bc_source in c('constrained', 'unconstrained')){
  for(per in c(6, 12)){
    bc_subs <- bc_summary[which(bc_summary$bc_source == bc_source), ]
    bc_subs <- bc_subs[which(bc_subs$time_period == per), ]
    for(animal in unique(dung_df$group)){
      dung_subs <- dung_df[which(dung_df$group == animal), ]
      cor_df <- merge(bc_subs, dung_subs, by='site')
      sp_test <- cor.test(cor_df$average_gm2_removed, cor_df$mean_dung,
                       method="spearman")
      sp_rho = sp_test[[4]]
      sp_p_val = sp_test[[3]]
      pe_test <- cor.test(cor_df$average_gm2_removed, cor_df$mean_dung,
                          method="pearson")
      pe_cor = pe_test[[4]]
      pe_p_val = pe_test[[3]]
      p <- ggplot(cor_df, aes(x=average_gm2_removed, y=mean_dung))
      p <- p + geom_point()
      p <- p + xlab("Back-calc intensity") + ylab("Mean dung per transect")
      pngname <- paste(results_dir,
                       paste(paste("back-calc_intensity_v", animal, "dung", per, "mos", bc_source, sep="_"),
                             '.png', sep=""),
                       sep="/")
      png(file=pngname, units="in", res=300, width=5, height=5)
      print(p)
      dev.off()
      cor_record[i, 'bc_source'] <- bc_source
      cor_record[i, 'time_period'] <- per
      cor_record[i, 'animal_type'] <- animal
      cor_record[i, 'spearman_rho'] <- sp_rho
      cor_record[i, 'spearman_p_val'] <- sp_p_val
      cor_record[i, 'pearson_cor'] <- pe_cor
      cor_record[i, 'pearson_p_val'] <- pe_p_val
      i <- i + 1
    }
  }
}
cor_r1 <- cor_record
cor_r3 <- cor_record

cor_record <- rbind(cor_r3, cor_r1)
save_as <- paste(results_dir, 'correlation_summary.csv', sep='/')
write.csv(cor_record, save_as, row.names=FALSE)

# ratio of bovid to other dung on each property
ratio_df <- data.frame('site'=numeric(24), 'bovid_to_non_bovid_ratio'=numeric(24))
for(i in c(1:24)){
  site <- colnames(gr3_means)[i + 1]
  ratio = gr3_means[1, site] / gr3_means[2, site]
  ratio_df[i, 'site'] <- site
  ratio_df[i, 'bovid_to_non_bovid_ratio'] <- ratio
}

# reported cattle
est_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/Regional_properties_dung_analysis/reg_cattle_estimates_11.30.16.csv"
est_df <- read.csv(est_csv)
est_df$property_cattle <- (est_df$PropCattle0 + est_df$PropCattleT.6) / 2
est_df$non_property_cattle <- (est_df$NonPropCattle0 + est_df$NonPropCattleT.6) / 2
est_df$property_non_property_cattle <- est_df$property_cattle + est_df$non_property_cattle
est_df$prop_density <- est_df$property_cattle / est_df$LwfPropSizeHa
est_df$prop_non_density <- est_df$property_non_property_cattle / est_df$LwfPropSizeHa
est_df <- merge(est_df, FID_list, by.x="Property", by.y="NAME", all.x=TRUE) # [, 2:25]
est_df[which(est_df$Confirmed == 'yes'), 'Confirmed'] <- 'Yes'
est_df[which(est_df$Confirmed == 'no'), 'Confirmed'] <- 'No'

# try with only confirmed cattle
est_subs <- est_df[which(est_df$Confirmed == "Yes"),
                   c("Property", "FID", "prop_density", "prop_non_density")]
# est_subs <- est_df[, c("Property", "FID", "prop_density", "prop_non_density")]
bovid_dung <- gr3_res[which(gr3_res$group == 'bovid'), ]
bovid_est <- merge(bovid_dung, est_subs, by.x="site", by.y="FID")

p <- ggplot(bovid_est, aes(x=mean_dung, y=prop_density, label=Property))
p <- p + geom_point()
p <- p + geom_text(check_overlap=TRUE)
p <- p + xlab("mean bovid dung per transect")
p <- p + ylab("property cattle per ha")
print(p)

pngname <- "C:/Users/Ginger/Desktop/property_cattle_vs_bovid_dung_GK.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

p <- ggplot(bovid_est, aes(x=mean_dung, y=prop_non_density, label=Property))
p <- p + geom_point()
p <- p + geom_text(check_overlap=TRUE)
print(p)

t1 <- cor.test(bovid_est$mean_dung, bovid_est$prop_density, method="pearson")
t2 <- cor.test(bovid_est$mean_dung, bovid_est$prop_non_density, method="pearson")

# ratio of bovid to all other dung v reported numbers
ratio_est <- merge(ratio_df, est_df, by.x="site", by.y="FID")
ratio_subs <- ratio_est[which(ratio_est$Property != "Makurian"), ]
p <- ggplot(ratio_subs, aes(x=bovid_to_non_bovid_ratio, y=prop_density))
p <- p + geom_point()
p <- p + xlab("ratio of bovid to non-bovid dung") + ylab("property cattle per ha")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/Regional_properties_dung_analysis/bovid_dung_ratio_vs_density.png"
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

p <- ggplot(ratio_subs, aes(x=bovid_to_non_bovid_ratio, y=prop_non_density))
p <- p + geom_point()
p <- p + xlab("ratio of bovid to non-bovid dung") + ylab("property + non-property cattle per ha")
print(p)


## throwaway
for (r in (1:NROW(meta_df))){
  date <- as.Date(meta_df[r, "Date"], format="%d-%b-%y")
  meta_df[r, 'year_month'] <- format(date, "%Y_%m")
}
# cattle density inside classes of movement density
throwaway_df <- merge(comb_df, meta_df)
# across months
throwaway_df$Movement_d <- factor(throwaway_df$Movement_d,
                                  levels=c("None", "Low", "Low medium",
                                           "Medium", "Medium high",
                                           "High"))
p <- ggplot(throwaway_df, aes(x=Movement_d, y=bovid))
p <- p + geom_boxplot() + ggtitle("all transects")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/bovid_dung_by_movement_d.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# by month
for(m in unique(throwaway_df$year_month)){
  sub_df <- throwaway_df[which(throwaway_df$year_month == m), ]
  sub_df$Movement_d <- factor(sub_df$Movement_d, levels=c("None", "Low", "Low medium",
                                                          "Medium", "Medium high",
                                                          "High"))
  p <- ggplot(sub_df, aes(x=Movement_d, y=bovid))
  p <- p + geom_boxplot() + ggtitle(m)
  print(p)
}