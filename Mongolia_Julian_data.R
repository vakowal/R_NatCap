# Process field data from Julian Ahlborn
data_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn"
processed_dir <- paste(data_dir, 'summaries_GK')
dir.create(processed_dir)

# create processed cover data for comparison with RPM results
veg_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dat.txt', sep='/'),
                      header=TRUE)
cover_df <- veg_dat[, c('site', 'code', 'plot', 'date_2014', 'date_2015', 'cover_perc_2014', 'cover_perc_2015')]
cover_long_df <- reshape(cover_df, idvar=c('site', 'code', 'plot'), varying=c('date_2014', 'date_2015'),
                         v.names='date', timevar='year', times=c('2014', '2015'),
                         direction="long")
cover_long_df$cover <- 0
cover_long_df[cover_long_df$year == 2014, 'cover'] <- cover_long_df[cover_long_df$year == 2014, 'cover_perc_2014']
cover_long_df[cover_long_df$year == 2015, 'cover'] <- cover_long_df[cover_long_df$year == 2014, 'cover_perc_2015']
date_out <- strsplit(as.character(cover_long_df$date), '[.]')
date_df <- data.frame(do.call(rbind, date_out))
colnames(date_df) <- c('day_c', 'month_c', 'year')
date_df$month <- as.numeric(as.character(date_df$month_c))
date_df$day <- as.numeric(as.character(date_df$day_c))
merged_df <- cbind(cover_long_df, date_df)
veg_df <- merged_df[, c('site', 'plot', 'year', 'month', 'day', 'cover')]
veg_df_path <- paste(processed_dir, 'veg_cover_data.csv', sep='/')
write.csv(veg_df, veg_df_path, row.names=FALSE)
# process Julian's veg dat to compare with RPM results
# reclassify month for sites sampled in first half of the month, to better match Century dates
veg_df <- read.csv(veg_df_path)
veg_df$month_reclass <- NA
veg_df[veg_df$day < 15, 'month_reclass'] <- veg_df[veg_df$day < 15, 'month'] - 1
veg_df[veg_df$day >= 15, 'month_reclass'] <- veg_df[veg_df$day >= 15, 'month']
veg_df$plotid <- paste(veg_df$site, veg_df$plot)
veg_agg_df <- aggregate(cover~plotid + year + month, veg_df, FUN=mean)
colnames(veg_agg_df) <- c('plotid', 'year', 'month', 'mean_perc_cover')
write.csv(veg_agg_df, paste(processed_dir, 'veg_cover_monthly.csv', sep='/'),
          row.names=FALSE)

# CHIRPS precip data at sampling plots
chirps_raw_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn/CHIRPS_precip_plot_centroids.csv")
chirps_df <- merge(chirps_raw_df, step_key_df, all.x=TRUE)
veg_agg_df <- read.csv(paste(processed_dir, 'veg_cover_monthly.csv', sep='/'))
veg_agg_step <- merge(veg_agg_df, step_key_df, all.x=TRUE)
veg_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dat.txt', sep='/'),
                      header=TRUE)
metadata_df <- veg_dat[!duplicated(veg_dat$code), c('site', 'plot', 'distance', 'hotspot_type', 'code')]
metadata_df$plotid <- paste(metadata_df$site, metadata_df$plot)
metadata_df <- metadata_df[!duplicated(metadata_df$plotid), c('site', 'plotid')]
veg_agg_step <- merge(veg_agg_step, metadata_df, all.x=TRUE)
n_months <- 12
df_list = list()
for (step_id in unique(veg_agg_step$step)) {
  cover_subs <- veg_agg_step[veg_agg_step$step == step_id, ]
  chirps_subs <- chirps_df[(chirps_df$step > step_id - n_months) &
                           (chirps_df$step <= step_id), ]
  chirps_cumulative <- aggregate(precip_cm~plotid, data=chirps_subs, FUN=sum)
  chirps_merge_cover <- merge(chirps_cumulative, cover_subs)
  df_list[[step_id]] <- chirps_merge_cover
}
cum_chirps_cover_df <- do.call(rbind, df_list)
cum_chirps_cover_df <- cum_chirps_cover_df[, c('site', 'plotid', 'precip_cm', 'year', 'month', 'mean_perc_cover')]
colnames(cum_chirps_cover_df)[3] <- 'precip_cm_12mo'

precip_lm_test <- lm(mean_perc_cover~precip_cm_12mo, data=cum_chirps_cover_df)
summary(precip_lm_test)$r.squared  # 0.38
precip_2014_df <- cum_chirps_cover_df[cum_chirps_cover_df$year == 2014, ]
precip_2014_lm_test <- lm(mean_perc_cover~precip_cm_12mo, data=precip_2014_df)
summary(precip_2014_lm_test)$r.squared  # 0.62
precip_2015_df <- cum_chirps_cover_df[cum_chirps_cover_df$year == 2015, ]
precip_2015_lm_test <- lm(mean_perc_cover~precip_cm_12mo, data=precip_2015_df)
summary(precip_2015_lm_test)$r.squared  # 0.08

cum_chirps_cover_df$year <- factor(cum_chirps_cover_df$year)
p <- ggplot(cum_chirps_cover_df, aes(x=precip_cm_12mo, y=mean_perc_cover))
p <- p + geom_point(aes(color=site)) + facet_grid(~year, scales="free")
p <- p + xlab("Precip in previous 12 months (cm)") + ylab("Mean % cover")
print(p)
pngname <- paste(processed_dir, "percent_cover_v_precip_by_year.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# match up dung with veg
dung_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dung_counts.txt', sep='/'),
                       header=TRUE)
dung_df <- dung_dat[, c('code', 'MLU14', 'MLU15')]
dung_long_df <- reshape(dung_df, idvar='code', varying=c('MLU14', 'MLU15'),
                        v.names='MLU', timevar='year', times=c(2014, 2015),
                        direction="long")
site_from_veg <- merged_df[!duplicated(merged_df$code), c('site', 'plot', 'code')]
dung_df <- merge(dung_long_df, site_from_veg, by='code')
dung_df_path <- paste(processed_dir, 'dung_data.csv', sep='/')
write.csv(dung_df, dung_df_path, row.names=FALSE)

# mean livestock numbers across plots
dung_df <- read.csv(dung_df_path)
dung_df$plotid <- paste(dung_df$site, dung_df$plot)
dung_agg_df <- aggregate(MLU~plotid + year, dung_df, FUN=mean)
colnames(dung_agg_df)[3] <- 'mean_MLU'
dung_agg_wide <- reshape(dung_agg_df, idvar='plotid', timevar='year', direction="wide")
dung_df_path <- paste(processed_dir, 'mean_dung_by_plot.csv', sep='/')
write.csv(dung_agg_wide, dung_df_path, row.names=FALSE)

# mean livestock numbers within sites across plots, across years
dung_site_agg_df <- aggregate(MLU~site, dung_df, FUN=mean)
colnames(dung_site_agg_df)[2] <- 'mean_MLU'
dung_site_agg_df$mMLU_per_ha <- dung_site_agg_df$mean_MLU * 100
dung_site_df_path <- paste(processed_dir, 'mMLU_per_ha_by_site.csv', sep='/')
write.csv(dung_site_agg_df, dung_site_df_path, row.names=FALSE)

# an alternate approach: assign livestock density from 1500 distance to site
dung_df_1500 <- dung_df[dung_df$plot == 'E', ]
dung_1500_site_agg_df <- aggregate(MLU~site, dung_df_1500, FUN=mean)
dung_1500_site_agg_df$mMLU_per_ha_1500 <- dung_1500_site_agg_df$MLU * 100
dung_1500_site_agg_df <- dung_1500_site_agg_df[, c('site', 'mMLU_per_ha_1500')]
dung_1500_site_df_path <- paste(processed_dir, 'mMLU_per_ha_1500_location_by_site.csv', sep='/')
write.csv(dung_1500_site_agg_df, dung_1500_site_df_path, row.names=FALSE)

# join sfu_per_ha from soum-level livestock statistics
sfu_per_ha_soum <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn summaries_GK/site_centroid_join_soum_sfu_per_ha.csv")
sfu_per_ha_soum <- sfu_per_ha_soum[, c('site', 'sfu_per_ha')]
sfu_per_ha_soum[sfu_per_ha_soum$sfu_per_ha == 0, 'sfu_per_ha'] <- NA
animals_per_ha <- merge(dung_1500_site_agg_df, sfu_per_ha_soum)
animals_per_ha$ratio <- animals_per_ha$mMLU_per_ha_1500 / animals_per_ha$sfu_per_ha
animals_per_ha$mMLU_per_ha_corr <- animals_per_ha$mMLU_per_ha_1500 / 758
hist(animals_per_ha$ratio, breaks=50)
p <- ggplot(animals_per_ha, aes(x=mMLU_per_ha_corr, y=sfu_per_ha))
p <- p + geom_point() # + xlim(150, 590)
print(p)
# write mMLU_per_ha, corrected by comparison with soum-level livestock densities
corrected_path <- paste(processed_dir, 'mMLU_per_ha_1500_location_by_site.csv', sep='/')
animals_per_ha_corr <- animals_per_ha[, c('site', 'mMLU_per_ha_corr')]
write.csv(animals_per_ha_corr, corrected_path, row.names=FALSE)

# export coordinates to make a shapefile
sampling_coords <- veg_dat[, c('code', 'site', 'plot', 'latitude', 'longitude')]
write.csv(sampling_coords, paste(data_dir, 'Julian_sampling_sites.csv', sep='/'),
          row.names=FALSE)

# calculate centroid of precip treatments by site
library(reshape2)
spatial_subs <- veg_dat[, c('site', 'latitude', 'longitude')]
spatial_melt <- melt(spatial_subs, id='site')
site_centroid <- dcast(spatial_melt, site ~ variable, mean)
write.csv(site_centroid, paste(data_dir, 'site_centroids.csv', sep='/'),
          row.names=FALSE)
# calculate centroid of distance treatments by plot
veg_dat_copy <- veg_dat[, c('site', 'plot', 'latitude', 'longitude')]
veg_dat_copy$plotid <- paste(veg_dat_copy$site, veg_dat_copy$plot)
spatial_subs <- veg_dat_copy[, c('plotid', 'latitude', 'longitude')]
spatial_melt <- melt(spatial_subs, id='plotid')
plot_centroid <- dcast(spatial_melt, plotid ~ variable, mean)
write.csv(plot_centroid, paste(data_dir, 'plot_centroids.csv', sep='/'),
          row.names=FALSE)

# drivers of animal density: temporal vs spatial
# site metadata
veg_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dat.txt', sep='/'),
                      header=TRUE)
metadata_df <- veg_dat[!duplicated(veg_dat$code), c('site', 'plot', 'distance', 'hotspot_type', 'code')]
dung_df <- read.csv(paste(processed_dir, 'dung_data.csv', sep='/'))
dung_site_df <- merge(metadata_df, dung_df, by=c('site', 'plot', 'code'), all=TRUE)
dung_lm <- lm(MLU~site + distance + hotspot_type + year, data=dung_site_df)
summary(dung_lm)

dung_site_df$year <- factor(dung_site_df$year)
dung_site_df$site <- factor(dung_site_df$site)
p <- ggplot(dung_site_df, aes(x=site, y=MLU))
p <- p + geom_boxplot() + ylab("Livestock density (MLU)")
p <- p + facet_grid(~year)
print(p)
pngname <- paste(processed_dir, "MLU_by_site_year.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

p <- ggplot(dung_site_df, aes(x=year, y=MLU))
p <- p + geom_boxplot() + ylab("Livestock density (MLU)")
p <- p + facet_wrap(~site, scales='free')
print(p)
pngname <- paste(processed_dir, "MLU_by_year_site.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=8)
print(p)
dev.off()

# explore Century results at Julian's sites
biomass_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/Century_outputs/biomass_summary.csv")
biomass_df$year <- floor(biomass_df$time)
biomass_df$month <- round((biomass_df$time - biomass_df$year) * 12, digits=0)
biomass_df[biomass_df$month == 0, 'month'] <- 12
biomass_df$total_biomass_kgha <- biomass_df$total_biomass * 10
biomass_df$live_biomass_kgha <- biomass_df$live_biomass * 10
peak_biomass_df <- biomass_df[biomass_df$month == 8,
                              c('live_biomass', 'total_biomass',
                                'total_biomass_kgha', 'live_biomass_kgha', 'site_id', 'month')]

# reported productivity at each site, Julian's J Arid Env. paper
j_table1 <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn/J_arid_env_table_1.csv")
colnames(j_table1) <- c('site_id', 'latitude', 'longitude', 'altitude', 'hotspot_type',
                        'ecosystem', 'average_temp', 'average_precip', 'precip_cv',
                        'richness', 'cover', 'biomass_kg_ha')

merged_df <- merge(peak_biomass_df, j_table1, by='site_id')
library(ggplot2)
p <- ggplot(merged_df, aes(x=total_biomass, y=biomass_kg_ha))
p <- p + geom_point()
print(p)

p <- ggplot(merged_df, aes(x=live_biomass, y=biomass_kg_ha))
p <- p + geom_point()
print(p)

p <- ggplot(merged_df, aes(x=total_biomass, y=cover))
p <- p + geom_point()
print(p)

# compare reported productivity at each site to precip: Worldclim and CHIRPS
worldclim_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_inputs/Ahlborn_sites/intermediate_data/worldclim_precip.csv")
worldclim_df <- aggregate(prec~site, worldclim_df, FUN=sum)
colnames(worldclim_df) <- c('site_id', 'annual_prec_worldclim')
# annual precip in 2014 from CHIRPS
zero_sd_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd"
step_key_df <- read.csv(
  paste(zero_sd_dir, "step_key.csv", sep='/'))
colnames(step_key_df) <- c('year', 'month', 'step')
chirps_raw_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn/CHIRPS_precip_plot_centroids.csv")
chirps_df <- merge(chirps_raw_df, step_key_df, all.x=TRUE)
chirps_df <- chirps_df[chirps_df$year == 2014, ]
plot_out <- strsplit(as.character(chirps_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
chirps_df <- cbind(chirps_df, plot_df)
chirps_mean_monthly <- aggregate(precip_cm~month+site, chirps_df, FUN=mean)
chirps_annual_sum <- aggregate(precip_cm~site, chirps_mean_monthly, FUN=sum)
colnames(chirps_annual_sum) <- c('site_id', 'annual_prec_chirps')
prec_df <- merge(worldclim_df, chirps_annual_sum)
plot_df <- merge(prec_df, merged_df)
plot_df$site_id <- as.factor(plot_df$site_id)
p <- ggplot(plot_df, aes(x=average_precip, y=annual_prec_worldclim))
p <- p + geom_point() + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.5)
p <- p + xlab("Average precip (Julian's table 1)") + ylab("Annual precip (Worldclim)")
pngname <- paste(fig_dir, "Julian_table1_precip_v_Worldclim_annual_precip.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=6)
print(p)
dev.off()
p <- ggplot(plot_df, aes(x=annual_prec_worldclim, y=biomass_kg_ha))
p <- p + geom_point() + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.5)
p <- p + xlab("Annual average precip (Worldclim)") + ylab("Biomass table 1")
pngname <- paste(fig_dir, "Julian_table1_biomass_v_annual_precip.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=6)
print(p)
dev.off()
p <- ggplot(plot_df, aes(x=annual_prec_chirps, y=biomass_kg_ha))
p <- p + geom_point() + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.5)
p <- p + xlab("Annual average precip (Worldclim)") + ylab("Avg August biomass from Century")
pngname <- paste(fig_dir, "Century_biomass_v_annual_precip.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=6)
print(p)
dev.off()

# RPM results
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"

# zero density
zero_sd_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd"
step_key_df <- read.csv(
  paste(zero_sd_dir, "step_key.csv", sep='/'))
colnames(step_key_df) <- c('year', 'month', 'step')

# site centroid results
sim_centroid_df <- read.csv(
  paste(zero_sd_dir, "RPM_biomass_site_centroids_zero_sd.csv", sep='/'))
sim_centroid_df <- merge(step_key_df, sim_centroid_df)
# restrict to complete years only
sim_centroid_df_full_years <- sim_centroid_df[sim_centroid_df$step >= 4, ]
sim_centroid_df_full_years$total_biomass_kgha <- sim_centroid_df_full_years$total_biomass_gm2 * 10
max_total_biomass_df <- aggregate(total_biomass_kgha~site + year, sim_centroid_df_full_years, FUN=max)
max_total_biomass_df$site <- factor(max_total_biomass_df$site)
p <- ggplot(max_total_biomass_df, aes(x=site, y=total_biomass_kgha))
p <- p + geom_boxplot()
p <- p + geom_point(data=j_table1, aes(x=site_id, y=biomass_kg_ha), color="red")
p <- p + geom_point(data=peak_biomass_df, aes(x=site_id, y=total_biomass_kgha), color="blue")
pngname <- paste(fig_dir, "biomass_boxplot_zero_sd.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# plot centroid results
veg_agg_df <- read.csv(paste(processed_dir, 'veg_cover_monthly.csv', sep='/'))
sim_plot_df <- read.csv(
  paste(zero_sd_dir, "RPM_biomass_plot_centroids_zero_sd.csv", sep='/'))
sim_plot_df <- merge(step_key_df, sim_plot_df)
sim_plot_df <- sim_plot_df[, c(
  'year', 'month', 'plotid',
  'aboveground_live_biomass_gm2', 'total_biomass_gm2')]
merged_df <- merge(veg_agg_df, sim_plot_df, by=c('plotid', 'year', 'month'))

# separate comparison by year
zero_sd_2014 <- merged_df[merged_df$year == 2014, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=zero_sd_2014)
summary(total_lm_test)$r.squared  # 0.66
live_lm_test <- lm(mean_perc_cover~aboveground_live_biomass_gm2, data=zero_sd_2014)
summary(live_lm_test)$r.squared  # 0.63

zero_sd_2015 <- merged_df[merged_df$year == 2015, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=zero_sd_2015)
summary(total_lm_test)$r.squared  # 0.43
live_lm_test <- lm(mean_perc_cover~aboveground_live_biomass_gm2, data=zero_sd_2015)
summary(live_lm_test)$r.squared  # 0.47

total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=merged_df)
rsq <- summary(total_lm_test)$r.squared
p <- ggplot(merged_df, aes(x=total_biomass_gm2, y=mean_perc_cover))
p <- p + geom_point() + facet_grid(~year, scales="free")
p <- p + xlab("Total simulated biomass (g/m2)") + ylab("Mean % cover")
# p <- p + geom_text(x=35, y=82, label=paste("R sq: ", round(rsq, 2), sep=""))
print(p)
pngname <- paste(fig_dir, "total_biomass_vs_cover_per_plot_zero_sd.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# RPM results with uniform (corrected) mean MLU per ha average across 2014 and 2015
uniform_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/uniform_density_mMLU_per_site"
uniform_df <- read.csv(
  paste(uniform_dir,
    "RPM_biomass_plot_centroids_uniform_density_mMLU_per_site.csv", sep='/'))
uniform_df <- merge(step_key_df, uniform_df)
merged_df <- merge(veg_agg_df, uniform_df, by=c('plotid', 'year', 'month'))

total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=merged_df)
summary(total_lm_test)$r.squared  # 0.57

# separate test by year
m2014_df <- merged_df[merged_df$year == 2014, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=m2014_df)
rsq_2014 <- summary(total_lm_test)$r.squared  # 0.66

m2015_df <- merged_df[merged_df$year == 2015, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=m2015_df)
rsq_2015 <- summary(total_lm_test)$r.squared  # 0.46

p <- ggplot(merged_df, aes(x=total_biomass_gm2, y=mean_perc_cover))
p <- p + geom_point() + facet_grid(~year, scales="free")
p <- p + xlab("Total simulated biomass (g/m2)") + ylab("Mean % cover")
print(p)
pngname <- paste(fig_dir, "total_biomass_vs_cover_per_plot_uniform_sd.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# RPM results using NDVI to disaggregate animals
via_ndvi_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/compare_to_ndvi"
ndvi_df = read.csv(
  paste(via_ndvi_dir, "RPM_biomass_plot_centroids_compare_to_ndvi.csv", sep='/'))
ndvi_step_key_df = read.csv(paste(via_ndvi_dir, 'step_key.csv', sep='/'))
colnames(ndvi_step_key_df) <- c('year', 'month', 'step')
ndvi_df <- merge(ndvi_df, ndvi_step_key_df)
merged_df <- merge(veg_agg_df, ndvi_df, by=c('plotid', 'year', 'month'))

total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=merged_df)
summary(total_lm_test)$r.squared  # 0.57
live_lm_test <- lm(mean_perc_cover~aboveground_live_biomass_gm2, data=merged_df)
summary(live_lm_test)$r.squared  # 0.53

# separate test by year
m2014_df <- merged_df[merged_df$year == 2014, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=m2014_df)
summary(total_lm_test)$r.squared  # 0.69
live_lm_test <- lm(mean_perc_cover~aboveground_live_biomass_gm2, data=m2014_df)
summary(live_lm_test)$r.squared  # 0.65

m2015_df <- merged_df[merged_df$year == 2015, ]
total_lm_test <- lm(mean_perc_cover~total_biomass_gm2, data=m2015_df)
summary(total_lm_test)$r.squared  # 0.46
live_lm_test <- lm(mean_perc_cover~aboveground_live_biomass_gm2, data=m2015_df)
summary(live_lm_test)$r.squared  # 0.45

p <- ggplot(merged_df, aes(x=total_biomass_gm2, y=mean_perc_cover))
p <- p + geom_point() + facet_grid(~year, scales="free")
p <- p + xlab("Total simulated biomass (g/m2)") + ylab("Mean % cover")
print(p)
pngname <- paste(fig_dir, "total_biomass_vs_cover_per_plot_compare_to_ndvi.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# compare animal density estimated via NDVI to empirical
sim_density_df <- read.csv(
  paste(via_ndvi_dir, "RPM_density_plot_centroids_compare_to_ndvi.csv", sep='/'))
sim_density_df <- merge(sim_density_df, ndvi_step_key_df)
plot_out <- strsplit(as.character(sim_density_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
sim_density_df <- cbind(sim_density_df, plot_df)

# restrict plot to steps in 2014-2015
subs_density <- sim_density_df[sim_density_df$year >= 2014, ]
subs_density$site <- as.factor(subs_density$site)
# include only the extreme distance treatments, combine A+B
low_distance <- subs_density[(subs_density$plot=='A') |
                               (subs_density$plot=='B'), ]
low_dist_mean <- aggregate(animal_density~site + step, low_distance, FUN=mean)
low_dist_mean$plot <- 'AB'
high_dist <- subs_density[(subs_density$plot=='E'), colnames(low_dist_mean)]
low_dist_mean$distance <- '50-150 m'
high_dist$distance <- '1500 m'
dist_subs <- rbind(low_dist_mean, high_dist)
dist_subs$distance <- factor(dist_subs$distance, levels=c('50-150 m', '1500 m'))
# empirical dung data
dung_df_path <- paste(processed_dir, 'dung_data.csv', sep='/')
dung_df <- read.csv(dung_df_path)
code_out <- strsplit(as.character(dung_df$code), '[[:alpha:]]')
code_df <- data.frame(do.call(rbind, code_out))
colnames(code_df) <- c('site', 'replicate')
dung_df <- cbind(dung_df, code_df)
emp_low_dist <- dung_df[(dung_df$plot=='A') |
                        (dung_df$plot=='B'), c('site', 'replicate', 'year', 'MLU')]
emp_low_dist$plot <- 'AB'
high_dist <- dung_df[(dung_df$plot=='E'), colnames(emp_low_dist)]
emp_low_dist$distance <- '50-150 m'
high_dist$distance <- '1500 m'
emp_subs <- rbind(emp_low_dist, high_dist)
emp_subs$step <- 0
emp_subs[emp_subs$year == 2014, 'step'] <- ndvi_step_key_df[
  (ndvi_step_key_df$year == 2014) & (ndvi_step_key_df$month == 8), 'step']
emp_subs[emp_subs$year == 2015, 'step'] <- ndvi_step_key_df[
  (ndvi_step_key_df$year == 2015) & (ndvi_step_key_df$month == 8), 'step']
# emp_subs$step <- as.factor(emp_subs$step)
# correct MLU by the same factor I used above to calculate site-level total animals
emp_subs$MLU_cor <- emp_subs$MLU / 7.58
emp_subs$distance <- factor(emp_subs$distance, levels=c('50-150 m', '1500 m'))

p <- ggplot(dist_subs, aes(x=step, y=animal_density, group=distance))
p <- p + geom_line(aes(color=distance)) + facet_wrap(~site, nrow=3, scales='free')
p <- p + xlab("Step (month)") + ylab("Animal density (animals/ha)")
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50-150m_1500m.png", sep='/')
png(file=pngname, units="in", res=300, width=8, height=4.5)
print(p)
dev.off()

p <- p + geom_jitter(data=emp_subs, aes(x=step, y=MLU_cor, color=distance), width=1)
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50-150m_1500m_with_empirical.png", sep='/')
png(file=pngname, units="in", res=300, width=8, height=4.5)
print(p)
dev.off()

# distances A and E only
low_distance <- subs_density[(subs_density$plot=='A'), ]
high_dist <- subs_density[(subs_density$plot=='E'), colnames(low_distance)]
low_distance$distance <- '50 m'
high_dist$distance <- '1500 m'
dist_subs <- rbind(low_distance, high_dist)
dist_subs$distance <- factor(dist_subs$distance, levels=c('50 m', '1500 m'))
# empirical numbers, continuing using the dung_df read above
emp_low_dist <- dung_df[(dung_df$plot=='A'), c('site', 'replicate', 'year', 'MLU')]
high_dist <- dung_df[(dung_df$plot=='E'), colnames(emp_low_dist)]
emp_low_dist$distance <- '50 m'
high_dist$distance <- '1500 m'
emp_subs <- rbind(emp_low_dist, high_dist)
emp_subs$step <- 0
emp_subs[emp_subs$year == 2014, 'step'] <- ndvi_step_key_df[
  (ndvi_step_key_df$year == 2014) & (ndvi_step_key_df$month == 8), 'step']
emp_subs[emp_subs$year == 2015, 'step'] <- ndvi_step_key_df[
  (ndvi_step_key_df$year == 2015) & (ndvi_step_key_df$month == 8), 'step']
# correct MLU by the same factor I used above to calculate site-level total animals
emp_subs$MLU_cor <- emp_subs$MLU / 7.58
emp_subs$distance <- factor(emp_subs$distance, levels=c('50 m', '1500 m'))

p <- ggplot(dist_subs, aes(x=step, y=animal_density, group=distance))
p <- p + geom_line(aes(color=distance)) + facet_wrap(~site, nrow=3, scales='free')
p <- p + xlab("Step (month)") + ylab("Animal density (animals/ha)")
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50m_1500m.png", sep='/')
png(file=pngname, units="in", res=300, width=8, height=4.5)
print(p)
dev.off()

p <- p + geom_jitter(data=emp_subs, aes(x=step, y=MLU_cor, color=distance), width=1)
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50m_1500m_with_empirical.png", sep='/')
png(file=pngname, units="in", res=300, width=8, height=4.5)
print(p)
dev.off()
