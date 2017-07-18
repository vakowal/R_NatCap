# OPC spatial analysis

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_bovid_dung_sum.csv"
comb_df <- read.csv(save_as)

# semivariogram for smaller time periods
# try 3-month periods, staggered (produced by hand) 
month_gr <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/months_grouping.csv")
comb_gr <- merge(comb_df, month_gr, by='year_month', all.x=TRUE)
imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/semivariograms/3_month_groups"
library(geoR)
for(gr_col in c('gr_1', 'gr_2', 'gr_3')){
  for(group in unique(comb_gr[, gr_col])){
    subdf <- comb_gr[comb_gr[, gr_col] == group, ]
    dists <- dist(cbind(subdf$Long, subdf$Lat))
    breaks <- seq(0, max(dists), length.out=10)
    cow_geodat <- jitterDupCoords(as.geodata(subdf, coords.col=7:8, data.col=6),
                                  max=0.0001)
    variogram <- variog(cow_geodat, breaks=breaks, option="bin")
    pngname <- paste(imgdir, paste("bovid_dung_semivariogram_bin_maxdist_",
                                   gr_col, "_", group, ".png", sep=""), sep="/")
    png(file=pngname, units="in", res=300, width=7, height=5)
    plot(variogram, type="b", main=paste("Bovid dung", gr_col, group, sep=" "))
    dev.off()
  }
}

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

# ecological data on OPC by management unit
# pinframe data: sum within position, then average within transect
pin_df <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_pinframe.csv")
pin_df <- pin_df[, (1:59)]
pin_df$trans_pos <- paste(pin_df$Date, pin_df$Site, pin_df$Position_m, sep="_")
pin_pos_sum <- aggregate(pin_df[, 6:59], by=list(pin_df$trans_pos), FUN=sum)
pin_pos_sum <- data.frame(pin_pos_sum,
                          do.call(rbind, strsplit(as.character(pin_pos_sum$Group.1),'_')))
pin_pos_sum$transect <- paste(pin_pos_sum$X1, pin_pos_sum$X2, sep="_")
pin_mean <- aggregate(pin_pos_sum[, 2:55], by=list(pin_pos_sum$transect), FUN=mean)
colnames(pin_mean)[1] <- 'transect'
find_green <- function(val){
  letters <- strsplit(val, split="")
  if(length(letters[[1]]) > 3){
    test_letter <- letters[[1]][4]
  }
  else{
    test_letter <- letters[[1]][3]
  }
  if(test_letter == 'G'){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
green_col_idx <- sapply(colnames(pin_mean), find_green, USE.NAMES=FALSE)
brown_cols <- pin_mean[, (!green_col_idx)]
green_brown_summary <- data.frame(pin_mean[, 1])
colnames(green_brown_summary) <- 'transect'
green_brown_summary$green_sum <- rowSums(pin_mean[, (green_col_idx)])
green_brown_summary$brown_sum <- rowSums(brown_cols[, -1])
green_brown_summary$perc_green <- green_brown_summary$green_sum / (green_brown_summary$green_sum + green_brown_summary$brown_sum)
from_rs <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17/summarized/pinframe_summary.csv")
green_brown_summary <- rbind(green_brown_summary, from_rs)

# dung data: Sum within transect
PDM_csv <- "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_PDM_dung.csv"
dung_df <- read.csv(PDM_csv)
dung_df <- dung_df[which(dung_df$Position_m > 0), ]
dung_df[, 9:32][is.na(dung_df[, 9:32])] <- 0
dung_sum <- aggregate(dung_df[, 9:32], by=list(dung_df$transect), FUN=sum)
colnames(dung_sum)[1] <- 'transect'

# sum by functional groups
group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)
means_t <- as.data.frame(t(dung_sum[, c(2:25)]))
colnames(means_t) <- dung_sum$transect
means_t$Abbrev <- rownames(means_t)
gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group5', 'Group6', 'Group7')]
comb <- merge(means_t, gr_subs, by='Abbrev')
gr1_means <- aggregate(comb[, 2:287], by=list(comb$Group1), FUN=sum)
gr5_means <- aggregate(comb[, 2:287], by=list(comb$Group5), FUN=sum)
gr6_means <- aggregate(comb[, 2:287], by=list(comb$Group6), FUN=sum)
gr7_means <- aggregate(comb[, 2:287], by=list(comb$Group7), FUN=sum)
colnames(gr1_means)[1] <- "group"
colnames(gr5_means)[1] <- "group"
colnames(gr6_means)[1] <- "group"
colnames(gr7_means)[1] <- "group"
gr1_res <- as.data.frame(t(gr1_means))
colnames(gr1_res) <- gr1_means$group
gr1_res$transect <- rownames(gr1_res)
gr1_res <- gr1_res[-1, ]
gr5_res <- as.data.frame(t(gr5_means))
colnames(gr5_res) <- gr5_means$group
gr5_res$transect <- rownames(gr5_res)
gr5_res <- gr5_res[-1, ]
gr6_res <- as.data.frame(t(gr6_means))
colnames(gr6_res) <- gr6_means$group
gr6_res$transect <- rownames(gr6_res)
gr6_res <- gr6_res[-1, ]
gr7_res <- as.data.frame(t(gr7_means))
colnames(gr7_res) <- gr7_means$group
gr7_res$transect <- rownames(gr7_res)
gr7_res <- gr7_res[-1, ]
grouped_dung <- merge(gr1_res, gr6_res, all=TRUE)
gr5_res <- gr5_res[, c('transect', setdiff(colnames(gr5_res), colnames(gr1_res)))]
grouped_dung <- merge(grouped_dung, gr5_res, all=TRUE)
gr7_res <- gr7_res[, c('transect', setdiff(colnames(gr7_res), colnames(grouped_dung)))]
grouped_dung <- merge(grouped_dung, gr7_res, all=TRUE)
from_rs <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17/summarized/grouped_dung.csv")
colnames(from_rs)[6] <- 'Other'
colnames(from_rs)[13] <- 'Dik-dik'
for(col in colnames(grouped_dung)){
  if(col %in% colnames(from_rs)){
    next
  }
  else {
    from_rs[, col] <- 0
  }
}
for(col in colnames(from_rs)){
  if(col %in% colnames(grouped_dung)){
    next
  }
  else {
    grouped_dung[, col] <- 0
  }
}
grouped_dung <- rbind(grouped_dung, from_rs)

# PDM data: average within transect
PDM_mean <- aggregate(PDM~transect, data=dung_df, FUN=mean)
PDM_mean$biomass_kgha <- PDM_mean$PDM * 332.35 + 15.857
PDM_mean <- PDM_mean[, c('transect', 'biomass_kgha')]
from_rs <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17/summarized/pdm_biomass.csv")
from_rs$transect <- paste(from_rs$Date, from_rs$Transect, sep="_")
from_rs <- from_rs[, c('transect', 'biomass_kgha')]
PDM_mean <- rbind(PDM_mean, from_rs)

metadata_csv <- "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_metadata.csv"
meta_df <- read.csv(metadata_csv)
meta_df <- meta_df[, c("transect", "mgmt_zone")]
from_rs <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17/summarized/metadata_mgmt_zone.csv")
from_rs$transect <- paste(from_rs$Date_sampl, from_rs$Transect, sep="_")
from_rs <- from_rs[, c("transect", "mgmt_zone")]
meta_df <- rbind(meta_df, from_rs)

# derive sampling periods (months, except March-April 2015 are combined)
samp_per <- as.data.frame(green_brown_summary[, 'transect'])
colnames(samp_per)[1] <- 'transect'
samp_per <- data.frame(samp_per,
                       do.call(rbind, strsplit(as.character(samp_per$transect),'_')))
samp_per$Date <- as.Date(samp_per$X1, format="%d-%b-%y")
samp_per$year_month <- format(samp_per$Date, "%Y-%m")
samp_per <- samp_per[!duplicated(samp_per$transect), ]
year_mo_per <- as.data.frame(unique(samp_per$year_month))
colnames(year_mo_per)[1] <- 'year_month'
year_mo_per$sampling_period <- c(1, 2, 3, 3, 4, 5, 7, 8, 9, 10, 11) # TODO REPLACE THIS
samp_per <- merge(samp_per, year_mo_per, by='year_month')
samp_per <- samp_per[, c('transect', 'sampling_period')]

ecol_df <- merge(meta_df, green_brown_summary, by='transect')
ecol_df <- merge(ecol_df, grouped_dung, by='transect')
ecol_df <- merge(ecol_df, PDM_mean, by='transect')
ecol_df <- merge(ecol_df, samp_per, by='transect')
ecol_df$zone_period <- paste(ecol_df$mgmt_zone, as.character(ecol_df$sampling_period),
                             sep="-")

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/ecol_df.csv"
write.csv(ecol_df, save_as, row.names=FALSE)

# sample size by sampling period
samp_per_key <- aggregate(year_month~sampling_period, data=year_mo_per, FUN='paste')
ecol_df$sampling_period <- as.factor(ecol_df$sampling_period)
sample_size <- table(ecol_df[, c('sampling_period', 'mgmt_zone')])

# tick pathogen data
# TBD

# average all values across transects by sampling period

for(col in 25:60){
  ecol_df[[col]] <- as.numeric(ecol_df[[col]])
}
ecol_by_samp <- aggregate(ecol_df[, 25:60], by=list(ecol_df$zone_period),
                          FUN=mean, na.rm=TRUE)
colnames(ecol_by_samp)[1] <- 'zone_period'
ecol_by_samp <- merge(ecol_by_samp,
                      ecol_df[!duplicated(ecol_df$zone_period), c('zone_period', 'sampling_period', 'mgmt_zone')],
                      by='zone_period', all.x=TRUE)
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/ecol_by_samp.csv"
write.csv(ecol_by_samp, save_as, row.names=FALSE)

# various groups vs cow dung at various lag times
dung_df <- ecol_by_samp[, c("mgmt_zone", "sampling_period", colnames(ecol_by_samp)[5:36])]
cattle_subs <- ecol_by_samp[, c('Cattle', "sampling_period", "mgmt_zone")]
# make df of bovid dung density by different lag times
bind_list <- list()
for(lag in 0:7){
  sub_df <- cattle_subs
  sub_df$sampling_period <- as.numeric(sub_df$sampling_period)
  sub_df$samp_per_of_response <- sub_df$sampling_period + lag  # sampling period of the response
  sub_df$lag <- lag
  bind_list[[lag + 1]] <- sub_df
}
lag_df <- do.call(rbind, bind_list)
lag_df <- lag_df[lag_df$samp_per_of_response < 9, ]
lag_df <- lag_df[, c('Cattle', 'mgmt_zone', 'samp_per_of_response', 'lag')]
colnames(lag_df) <- c('Cattle', 'mgmt_zone', 'sampling_period', 'lag')
# join lag df to response df
dung_lag_df <- merge(dung_df, lag_df, by=c('sampling_period', 'mgmt_zone'), all=TRUE)
colnames(dung_lag_df)[13] <- 'cattle_response'
colnames(dung_lag_df)[35] <- 'lagged_cattle_dung'

# make plots
mean_dung <- as.data.frame(colMeans(dung_lag_df[, c(3:6, 8:13, 15:34)], na.rm=TRUE))
colnames(mean_dung)[1] <- 'mean_dung'
mean_dung$type <- row.names(mean_dung)
abundant_groups <- mean_dung[mean_dung$mean_dung > 4.031, 'type']

library(ggplot2)
imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/dung_by_cattle_lag"
for(col in abundant_groups){
  p <- ggplot(dung_lag_df, aes_string(x='lagged_cattle_dung', y=col))
  p <- p + geom_point()
  p <- p + facet_wrap(~lag) + ggtitle(col)
  pngname <- paste(imgdir, paste(col, '.png', sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=7, height=7)
  print(p)
  dev.off()
}
  
# calculate correlation for each animal type, for each lag distance
r <- 1
cor_df <- data.frame('dung_type'=c(), 'sampling_period_lag'=c(),
                     'pearson_r'=c(), 'pearson_p'=c(), 'sample_size'=c())
for(col in abundant_groups){
  for(lag in unique(dung_lag_df$lag)){
    subs <- dung_lag_df[dung_lag_df$lag == lag, ]
    t1 <- cor.test(subs[[col]], subs$lagged_cattle_dung, method="pearson")
    sample_size <- length(subs[!is.na(subs[[col]]), col])
    cor_df[r, 'dung_type'] <- col
    cor_df[r, 'sampling_period_lag'] <- lag
    cor_df[r, 'pearson_p'] <- t1[[3]]
    cor_df[r, 'pearson_r'] <- t1[[4]]
    cor_df[r, 'sample_size'] <- sample_size
    r <- r + 1
  }
}
write.csv(cor_df, 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/dung_lag_cor_table.csv',
          row.names=FALSE)

# calc veg metrics, join lag_df to veg metrics to make similar plot
veg_df <- ecol_by_samp[, c('green_sum', 'brown_sum', 'perc_green',
                           'biomass_kgha', 'sampling_period',
                           'mgmt_zone')]
veg_lag_df <- merge(veg_df, lag_df, by=c('sampling_period', 'mgmt_zone'), all=TRUE)
colnames(veg_lag_df)[7] <- 'lagged_cattle_dung'
r <- 1
cor_df <- data.frame('veg_metric'=c(), 'sampling_period_lag'=c(),
                     'pearson_r'=c(), 'pearson_p'=c(), 'sample_size'=c())
for(col in colnames(veg_lag_df)[3:6]){
  for(lag in unique(veg_lag_df$lag)){
    subs <- veg_lag_df[veg_lag_df$lag == lag, ]
    t1 <- cor.test(subs[[col]], subs$lagged_cattle_dung, method="pearson")
    sample_size <- length(subs[!is.na(subs[[col]]), col])
    cor_df[r, 'dung_type'] <- col
    cor_df[r, 'sampling_period_lag'] <- lag
    cor_df[r, 'pearson_p'] <- t1[[3]]
    cor_df[r, 'pearson_r'] <- t1[[4]]
    cor_df[r, 'sample_size'] <- sample_size
    r <- r + 1
  }
}
write.csv(cor_df, 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/veg_lag_cor_table.csv',
          row.names=FALSE)

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_spatial_analysis/veg_by_cattle_lag"
for(col in colnames(veg_lag_df)[3:6]){
  p <- ggplot(veg_lag_df, aes_string(x='lagged_cattle_dung', y=col))
  p <- p + geom_point()
  p <- p + facet_wrap(~lag) + ggtitle(col)
  pngname <- paste(imgdir, paste(col, '.png', sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=7, height=7)
  print(p)
  dev.off()
}

# variation among transects within mgmt zone - sampling period bins
library(ggplot2)
abundant_groups <- c("grazer_inc_bovid", "mixed", "all_dung", "bovid", "grazer_ex_bovid",  
                     "Buffalo", "Cattle", "Elephant", "Giraffe", "Grants_gazelle",
                     "Impala", "Thompsons_gazelle", "Warthog", "Zebra")
veg_cols <- c('green_sum', 'brown_sum', 'perc_green', 'biomass_kgha')
imgdir <- "C:/Users/Ginger/Desktop/var_plots"
for(col in c(abundant_groups, veg_cols)){
  p <- ggplot(ecol_df, aes_string(x='sampling_period', y=col))
  p <- p + geom_jitter(width=0.1, height=0)
  p <- p + facet_wrap(~mgmt_zone)
  pngname <- paste(imgdir, paste(col, '.png', sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=7, height=6)
  print(p)
  dev.off()
}

# throwaway

