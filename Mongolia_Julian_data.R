# Process field data from Julian Ahlborn
data_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn"
processed_dir <- paste(data_dir, 'summaries_GK')
dir.create(processed_dir)
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"

library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10),
                     strip.text.x=element_text(size=9),
                     axis.title.x=element_text(size=10),
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# all runs share the same starting date and number of steps
step_key_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_inputs/Ahlborn_sites/RPM_inputs/step_key.csv")
colnames(step_key_df) <- c('year', 'month', 'step')

# composition data
pfg_df <- read.csv(paste(data_dir, 'raw_data', 'biomassPFGs_SHARED.csv', sep='/'),
                   sep=';')

# get grass biomass only
pfg_df$grass_kgha <- pfg_df$Grasses_tha * 1000
grass_biomass <- pfg_df[, c('Site', 'year', 'grass_kgha')]
colnames(grass_biomass) <- c('site_id', 'year', 'grass_kgha')
grass_by_site <- aggregate(grass_kgha~site_id + year, data=grass_biomass, FUN=mean)

# summarize percent of biomass by functional type
pfg_cols_2015 <- c('Site', 'Grasses_tha', 'Sedges_tha', 'Legumes_tha', 'Herbs_tha', 'Shrubs_tha')
pfg_subs <- pfg_df[pfg_df$year == 2015, pfg_cols_2015]
pfg_total <- apply(pfg_subs[, -1], 1, sum)
pfg_percent = as.data.frame(lapply(pfg_subs[, -1], function(x) {x / pfg_total * 100}))
pfg_percent$site <- pfg_subs$Site
pfg_site_means <- aggregate(.~site, data=pfg_percent, FUN=mean)
site_means_res <- reshape(pfg_site_means, idvar='site',
                          varying=colnames(pfg_site_means)[-1],
                          v.names='percent', timevar='PFG',
                          times=c('Grasses', 'Sedges', 'Legumes', 'Herbs', 'Shrubs'),
                          direction='long')
library(ggplot2)
p <- ggplot(site_means_res, aes(x=PFG, y=percent))
p <- p + geom_point() + facet_wrap(~site, nrow=5)
p <- p + xlab("") + ylab("Percent of total biomass")
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)
pngname <- paste(processed_dir, "plant_functional_type_by_site_2015.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=8)
print(p)
dev.off()

pfg_cols_2014 <- c('Site', 'Grasses_tha', 'Sedges_tha', 'Forbs_tha', 'Wormwood_tha', 'Legumes_tha')
pfg_subs <- pfg_df[pfg_df$year == 2014, pfg_cols_2014]
pfg_total <- apply(pfg_subs[, -1], 1, sum)
pfg_percent = as.data.frame(lapply(pfg_subs[, -1], function(x) {x / pfg_total * 100}))
pfg_percent$site <- pfg_subs$Site
pfg_site_means <- aggregate(.~site, data=pfg_percent, FUN=mean)
site_means_res <- reshape(pfg_site_means, idvar='site',
                          varying=colnames(pfg_site_means)[-1],
                          v.names='percent', timevar='PFG',
                          times=c('Grasses', 'Sedges', 'Forbs', 'Wormwood', 'Legumes'),
                          direction='long')
p <- ggplot(site_means_res, aes(x=PFG, y=percent))
p <- p + geom_point() + facet_wrap(~site, nrow=5)
p <- p + xlab("") + ylab("Percent of total biomass")
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)
pngname <- paste(processed_dir, "plant_functional_type_by_site_2014.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=8)
print(p)
dev.off()

# CHIRPS precip data at sampling plots
chirps_raw_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn/CHIRPS_precip_plot_centroids.csv")
chirps_df <- merge(chirps_raw_df, step_key_df, all.x=TRUE)
plot_out <- strsplit(as.character(chirps_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
chirps_df <- cbind(chirps_df, plot_df)
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
veg_date_df <- merge(veg_date_df, step_key_df, all.x=TRUE)
veg_df <- merge(veg_df, veg_date_df)
n_months <- 12
df_list = list()
for (step_id in unique(veg_df$step)) {
  veg_subs <- veg_df[veg_df$step == step_id, ]
  chirps_subs <- chirps_df[(chirps_df$step > step_id - n_months) &
                           (chirps_df$step <= step_id), ]
  chirps_cumulative <- aggregate(precip_cm~site + plot, data=chirps_subs, FUN=sum)
  chirps_merge_veg <- merge(chirps_cumulative, veg_subs)
  df_list[[step_id]] <- chirps_merge_veg
}
cum_chirps_veg_df <- do.call(rbind, df_list)
cum_chirps_veg_df <- cum_chirps_veg_df[, c('site', 'plot', 'precip_cm', 'year', 'biomass_gm2', 'cover')]
colnames(cum_chirps_veg_df)[3] <- 'precip_cm_12mo'

precip_cover_test <- lm(cover~precip_cm_12mo, data=cum_chirps_veg_df)
summary(precip_cover_test)$r.squared  # 0.41
precip_biomass_test <- lm(biomass_gm2~precip_cm_12mo, data=cum_chirps_veg_df)
summary(precip_biomass_test)$r.squared  # 0.25
precip_2014_df <- cum_chirps_veg_df[cum_chirps_veg_df$year == 2014, ]
precip_2014_cover_test <- lm(cover~precip_cm_12mo, data=precip_2014_df)
summary(precip_2014_cover_test)$r.squared  # 0.68
precip_2014_biomass_test <- lm(biomass_gm2~precip_cm_12mo, data=precip_2014_df)
summary(precip_2014_biomass_test)$r.squared  # 0.47
precip_2015_df <- cum_chirps_veg_df[cum_chirps_veg_df$year == 2015, ]
precip_2015_cover_test <- lm(cover~precip_cm_12mo, data=precip_2015_df)
summary(precip_2015_cover_test)$r.squared  # 0.07
precip_2015_biomass_test <- lm(biomass_gm2~precip_cm_12mo, data=precip_2015_df)
summary(precip_2015_biomass_test)$r.squared  # 0.02

cum_chirps_veg_df$year <- factor(cum_chirps_veg_df$year)
p <- ggplot(cum_chirps_veg_df, aes(x=precip_cm_12mo, y=biomass_gm2))
p <- p + geom_point(aes(color=site)) + facet_grid(~year, scales="free")
p <- p + xlab("Precip in previous 12 months (cm)") + ylab("Biomass (g/m2)")
print(p)
pngname <- paste(processed_dir, "biomass_v_precip_by_year.png", sep='/')
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
dung_agg_df <- aggregate(MLU~plotid + year, dung_df, FUN=mean)  # mean MLU per plot across replicates
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

# sfu_per_ha from GLW (2010), area-weighted and dasymetric estimates
sfu_per_ha_GLW_plot <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn summaries_GK/plot_centroid_join_GLW_sfu_per_ha.csv")
glw_sfu_per_ha_Aw <- aggregate(sfu_ha_Aw~site, data=sfu_per_ha_GLW_plot, FUN=mean)
glw_sfu_per_ha_Da <- aggregate(sfu_ha_Da~site, data=sfu_per_ha_GLW_plot, FUN=mean)
sfu_per_ha_GLW <- merge(glw_sfu_per_ha_Aw, glw_sfu_per_ha_Da)

dung_1500_site_agg_df <- read.csv(dung_1500_site_df_path)
# Livestock household data from Oggie, winter 2018
sfu_per_ha_soum <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn summaries_GK/site_centroid_join_soum_sfu_per_ha.csv")
sfu_per_ha_soum <- sfu_per_ha_soum[, c('site', 'sfu_per_ha')]
sfu_per_ha_soum[sfu_per_ha_soum$sfu_per_ha == 0, 'sfu_per_ha'] <- NA
colnames(sfu_per_ha_soum) <- c('site', 'sfu_per_ha_household_data')
# Livestock census data from National Statistics Office, 2014-2015-2018
sfu_per_ha_nso <- read.csv(paste(processed_dir, "sfu_per_ha_by_site_centroid_2014_2015_2018.csv", sep='/'))
sfu_per_ha_nso <- sfu_per_ha_nso[, c('Site', 'year', 'sfu_per_ha')]
colnames(sfu_per_ha_nso) <- c('site', 'year', 'sfu_per_ha_nso')
sfu_per_ha_nso_wide <- reshape(sfu_per_ha_nso, timevar='year', v.names='sfu_per_ha_nso',
                               idvar='site', direction='wide')
animals_per_ha_df <- merge(sfu_per_ha_GLW, dung_1500_site_agg_df)
animals_per_ha_df <- merge(animals_per_ha_df, sfu_per_ha_soum)
animals_per_ha_df <- merge(animals_per_ha_df, sfu_per_ha_nso_wide)
colnames(animals_per_ha_df) <- c('site', 'sfu_per_ha_GLW_Aw', 'sfu_per_ha_GLW_Da',
                                 'mMLU_per_ha_Julian_1500m', 'sfu_per_ha_household_data',
                                 "sfu_per_ha_nso.2014", "sfu_per_ha_nso.2015", "sfu_per_ha_nso.2018")
animals_per_ha_path <- paste(processed_dir, "sfu_per_ha_in_soums_containing_sites.csv", sep='/')
write.csv(animals_per_ha_df, animals_per_ha_path, row.names=FALSE)

# Use empirical livestock numbers from NSO to convert dung to animal density
animals_per_ha_path <- paste(processed_dir, "sfu_per_ha_in_soums_containing_sites.csv", sep='/')
animals_per_ha_df <- read.csv(animals_per_ha_path)
nso_2015_df <- animals_per_ha_df[, c('site', 'sfu_per_ha_nso.2015')]
dung_1500_site_df_path <- paste(processed_dir, 'mMLU_per_ha_1500_location_by_site.csv', sep='/')
dung_1500_site_agg_df <- read.csv(dung_1500_site_df_path)
conversion_df <- merge(dung_1500_site_agg_df, nso_2015_df)
mean_ratio <- mean(conversion_df$sfu_per_ha_nso.2015 / conversion_df$mMLU_per_ha_1500)
adj_ratio <- mean_ratio * 1.5
conversion_df$est_animals_per_ha <- conversion_df$mMLU_per_ha_1500 * adj_ratio
conversion_path <- paste(processed_dir, "est_sfu_per_ha.csv", sep='/')
write.csv(conversion_df, conversion_path, row.names=FALSE)

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

# Century results at Julian's sites
biomass_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/Century_outputs_Aug2020/biomass_summary.csv")
biomass_df$year <- floor(biomass_df$time)
biomass_df$month <- round((biomass_df$time - biomass_df$year) * 12, digits=0)
biomass_df[biomass_df$month == 0, 'month'] <- 12
biomass_df$total_biomass_kgha <- biomass_df$total_biomass * 10
biomass_df$live_biomass_kgha <- biomass_df$live_biomass * 10
peak_biomass_df <- biomass_df[biomass_df$month == 8,
                              c('live_biomass', 'total_biomass',
                                'total_biomass_kgha', 'live_biomass_kgha', 'site_id', 'month')]
peak_biomass_path <- paste(processed_dir, 'peak_biomass_from_Century.csv', sep='/')
write.csv(peak_biomass_df, peak_biomass_path, row.names=FALSE)

# average empirical biomass per site (not restricted to grass)
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
veg_2014 <- veg_df[veg_df$year == 2014, ]
site_mean_biomass <- aggregate(biomass_gm2~site, data=veg_2014, FUN=mean)
colnames(site_mean_biomass) <- c('site_id', 'emp_biomass_gm2')
site_mean_biomass$emp_biomass_kgha <- site_mean_biomass$emp_biomass_gm2 * 10

## Testing: empirical biomass with outlier plots removed
# outliers_removed_path <- paste(processed_dir, "mean_biomass_outliers_removed.csv", sep='/')
# site_mean_biomass_both <- read.csv(outliers_removed_path)
# site_mean_biomass <- site_mean_biomass_both[site_mean_biomass_both$year == 2014, c('site_id', 'emp_biomass_kgha')]
merged_df <- merge(peak_biomass_df, site_mean_biomass)
# mean bias
bias <- mean(merged_df$total_biomass_kgha - merged_df$emp_biomass_kgha)
peak_cor <- cor.test(merged_df$total_biomass_kgha, merged_df$emp_biomass_kgha)
diff <- merged_df$total_biomass_kgha - merged_df$emp_biomass_kgha
diff_sq <- diff * diff
peak_rmse <- sqrt(mean(diff_sq))

library(ggplot2)
p <- ggplot(merged_df, aes(x=total_biomass_kgha, y=emp_biomass_kgha))
p <- p + geom_point() + xlab("Peak biomass (Century)") + ylab("Empirical biomass, 2014")
p <- p + geom_abline(slope=1, intercept=0, linetype='dashed') + print_theme
p <- p + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.5)
print(p)
pngname <- paste(fig_dir, "peak_biomass_Century_v_2014biomass_outliers_removed.png", sep='/')  # TODO check name
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

# biomass from Century in month of sampling
century_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/Century_outputs_Aug2020/biomass_summary.csv")
century_df$year <- floor(century_df$time)
century_df$month <- round((century_df$time - century_df$year) * 12, digits=0)
century_df[century_df$month == 0, 'month'] <- 12
century_df$total_biomass_kgha <- century_df$total_biomass * 10
century_df <- century_df[, c('site_id', 'year', 'month', 'total_biomass_kgha')]
colnames(century_df)[1] <- 'site'
century_2015 <- century_df
century_2015$year <- 2015
century_bothyears <- rbind(century_df, century_2015)
century_sampling_month_df <- merge(century_bothyears, mean_veg_df)  # to get only the months sampled
century_df <- century_sampling_month_df[, colnames(century_df)]
century_mean_veg <- merge(century_df, mean_veg_df)
p <- ggplot(century_mean_veg, aes(x=total_biomass_kgha, y=emp_biomass_kgha))
p <- p + geom_point() + facet_grid(~year)
p <- p + xlab("Simulated biomass, Century (kg/ha)") + ylab("Empirical biomass (kg/ha)")
p <- p + geom_abline(slope=1, intercept=0, linetype='dashed') + print_theme
p <- p + geom_text(aes(label=site), hjust=0, vjust=0, nudge_x=0.5)
print(p)
pngname <- paste(fig_dir, "biomass_Century_both_years_v_empirical_biomass.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

cor_2014 <- cor.test(century_mean_veg[century_mean_veg$year == 2014, 'total_biomass_kgha'],
                     century_mean_veg[century_mean_veg$year == 2014, 'emp_biomass_kgha'])
cor_2015 <- cor.test(century_mean_veg[century_mean_veg$year == 2015, 'total_biomass_kgha'],
                     century_mean_veg[century_mean_veg$year == 2015, 'emp_biomass_kgha'])

# biomass per site simulated with CHIRPS and uniform empirical density, Nov 2020
rpm_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/uniform_density_sfu_per_site/RPM_biomass_site_centroids_uniform_density_sfu_per_site.csv")
rpm_df <- merge(rpm_df, step_key_df)
rpm_df <- rpm_df[, c(
  'year', 'month', 'site', 'standing_biomass')]

# compare to NDVI time series
ndvi_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/biomass_vs_NDVI/ndvi_time_series_ahlborn_sites.csv")
ndvi_df$date <- as.Date(paste(ndvi_df$year, ndvi_df$month, '15', sep='-'))
ndvi_plot_subs <- ndvi_df[ndvi_df$site %in% c(1, 3, 5, 9, 11), ]
p <- ggplot(ndvi_plot_subs, aes(x=date, y=ndvi)) + xlab("") + ylab("NDVI")
p <- p + geom_line() + facet_wrap(~site, nrow=15, scales='free_y') + print_theme
p <- p + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)
pngname <- paste(fig_dir, "NDVI_plot_subset_time_series.png", sep='/')
png(file=pngname, units="in", res=300, width=2, height=6)
print(p)
dev.off()

rpm_subs <- rpm_df[rpm_df$year >= 2014, ]
rpm_subs$date <- as.Date(paste(rpm_subs$year, rpm_subs$month, '15', sep='-'))

plot_subs <- rpm_subs[rpm_subs$site %in% c(1, 3, 5, 9, 11), ]
p <- ggplot(plot_subs, aes(x=date, y=standing_biomass)) + xlab("Date") + ylab("Standing biomass from RPM (kg/ha)")
p <- p + geom_line() + facet_wrap(~site, nrow=5, scales='free_y') + print_theme
p <- p + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)
pngname <- paste(fig_dir, "rpm_uniform_density_sfu_per_site_plot_subset_time_series.png", sep='/')
png(file=pngname, units="in", res=300, width=2, height=6)
print(p)
dev.off()

# compare to grass biomass per site
veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
veg_date_df <- veg_date_df[!duplicated(veg_date_df[, c('site', 'year')]),
                           c('site', 'year', 'month')]
colnames(grass_by_site) <- c('site', 'year', 'grass_kgha')
mean_grass_df <- merge(grass_by_site, veg_date_df)
rpm_mean_grass <- merge(rpm_df, mean_grass_df)
rpm_df <- rpm_mean_grass[, c(colnames(rpm_df))]
merged_df <- merge(rpm_df, grass_by_site)
p <- ggplot(merged_df, aes(x=standing_biomass, y=grass_kgha))
p <- p + geom_point() + geom_abline(slope=1, intercept=0, linetype='dashed') + facet_wrap(~year)
p <- p + geom_text(aes(label=site), hjust=0, vjust=0, nudge_x=0.5)
p <- p + print_theme + xlab("Simulated biomass, RPM (kg/ha)") + ylab("Empirical grass biomass (kg/ha)")
print(p)
pngname <- paste(fig_dir, "biomass_RPM_CHIRPS_uniform_v_empirical_grass_biomass.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# compare with empirical mean total biomass per site
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
site_mean_biomass <- aggregate(biomass_gm2~site + year, data=veg_df, FUN=mean)
colnames(site_mean_biomass) <- c('site', 'year', 'emp_biomass_gm2')
site_mean_biomass$emp_biomass_kgha <- site_mean_biomass$emp_biomass_gm2 * 10

veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
veg_date_df <- veg_date_df[!duplicated(veg_date_df[, c('site', 'year')]),
                           c('site', 'year', 'month')]
mean_veg_df <- merge(site_mean_biomass, veg_date_df)
rpm_mean_veg <- merge(rpm_df, mean_veg_df)  # to get only the months sampled
rpm_df <- rpm_mean_veg[, colnames(rpm_df)]
merged_df <- merge(rpm_df, site_mean_biomass)

p <- ggplot(merged_df, aes(x=standing_biomass, y=emp_biomass_kgha))
p <- p + geom_point() + geom_abline(slope=1, intercept=0, linetype='dashed') + facet_wrap(~year)
p <- p + geom_text(aes(label=site), hjust=0, vjust=0, nudge_x=0.5)
p <- p + print_theme + xlab("Simulated biomass, RPM (kg/ha)") + ylab("Empirical biomass (kg/ha)")
print(p)
pngname <- paste(fig_dir, "biomass_RPM_CHIRPS_uniform_v_empirical_biomass.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# mean bias
bias_2014 <- mean(merged_df[merged_df$year == 2014, 'standing_biomass'] -
                    merged_df[merged_df$year == 2014, 'emp_biomass_kgha'])
bias_2015 <- mean(merged_df[merged_df$year == 2015, 'standing_biomass'] -
                    merged_df[merged_df$year == 2015, 'emp_biomass_kgha'])

# pearson corr
pearson_2014 <- cor.test(merged_df[merged_df$year == 2014, 'standing_biomass'],
                         merged_df[merged_df$year == 2014, 'emp_biomass_kgha'], method='pearson')
pearson_2015 <- cor.test(merged_df[merged_df$year == 2015, 'standing_biomass'],
                         merged_df[merged_df$year == 2015, 'emp_biomass_kgha'], method='pearson')

# compare reported productivity at each site to average precip from Worldclim
worldclim_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_inputs/Ahlborn_sites/intermediate_data/worldclim_precip.csv")
worldclim_df <- aggregate(prec~site, worldclim_df, FUN=sum)
colnames(worldclim_df) <- c('site_id', 'annual_prec_worldclim')
plot_df <- merge(worldclim_df, merged_df)
plot_df$site_id <- as.factor(plot_df$site_id)

biomass_df <- plot_df[, c('site_id', 'annual_prec_worldclim',
                            'total_biomass_kgha', 'emp_biomass_kgha')]
colnames(biomass_df) <- c('site_id', 'annual_prec_worldclim',
                          'biomass_Century', 'empirical_mean_biomass')
biomass_re <- reshape(biomass_df, varying=c('biomass_Century', 'empirical_mean_biomass'),
                      v.names='biomass_kg_ha', timevar='source',
                      times=c('Century', 'Empirical mean (2014)'),
                      idvar=c('site_id', 'annual_prec_worldclim'),
                      direction='long')
p <- ggplot(biomass_re, aes(x=annual_prec_worldclim, y=biomass_kg_ha))
p <- p + geom_point() + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.3)
p <- p + facet_wrap(~source, nrow=1)  #, scales='free')
p <- p + xlab("Annual average precip: Worldclim (cm)") + ylab("Biomass (kg/ha)")
print(p)
pngname <- paste(fig_dir, "biomass_v_annual_precip_empirical2014_Century.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

# RPM results
# generate table of RPM results, zero_sd and uniform and via_ndvi
# zero density
zero_sd_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd/RPM_biomass_plot_centroids_zero_sd.csv")
zero_sd_df <- merge(zero_sd_df, step_key_df)
zero_sd_df <- zero_sd_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
zero_sd_df$method <- 'zero_sd'
# uniform density
uniform_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/uniform_density_sfu_per_site/RPM_biomass_plot_centroids_uniform_density_sfu_per_site.csv")
uniform_df <- merge(uniform_df, step_key_df)
uniform_df <- uniform_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
uniform_df$method <- 'uniform'
# via NDVI
via_ndvi_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/via_ndvi/RPM_biomass_plot_centroids_via_ndvi.csv")
via_ndvi_df <- merge(via_ndvi_df, step_key_df)
via_ndvi_df <- via_ndvi_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
via_ndvi_df$method <- 'via_ndvi'

rpm_df <- rbind(zero_sd_df, uniform_df, via_ndvi_df)
plot_out <- strsplit(as.character(rpm_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
rpm_df <- cbind(rpm_df, plot_df)
date_fun <- function(year, month) {
  return(year + month/12)
}
rpm_df$date <- 0
for (r in 1:nrow(rpm_df)){
  rpm_df[r, 'date'] <- date_fun(rpm_df[r, 'year'], rpm_df[r, 'month'])
}
rpm_dat_path <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/RPM_results_zero_sd_uniform_via_ndvi.csv"
write.csv(rpm_df, rpm_dat_path, row.names=FALSE)

# RPM results at plot centroid
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
colnames(veg_df) <- c('site', 'plot', 'year', 'empirical_biomass_gm2', 'cover')
veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
veg_df <- merge(veg_df, veg_date_df)
rpm_df <- read.csv(rpm_dat_path)
sim_plot_df <- rpm_df[, c(
  'site', 'plot', 'year', 'month', 'plotid',
  'total_biomass_gm2', 'method')]
colnames(sim_plot_df) <- c(
  'site', 'plot', 'year', 'month', 'plotid',
  'sim_total_biomass_gm2', 'method')
merged_df <- merge(veg_df, sim_plot_df, by=c('site', 'plot', 'year', 'month'))
summary_df <- data.frame("method"=c(), "metric"=c(), "year"=c(), "Rsq"=c())
for(year in c('both', 2014, 2015)) {
  for(method in unique(merged_df$method)) {
    if(year == 'both') {
      subs_df <- merged_df[merged_df$method == method, ]
    } else {
      subs_df <- merged_df[(merged_df$year == year) &
                             (merged_df$method == method), ]
    }
    cover_lm <- lm(cover~sim_total_biomass_gm2, data=subs_df)
    cover_rsq <- summary(cover_lm)$r.squared
    biomass_lm <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=subs_df)
    biomass_rsq <- summary(biomass_lm)$r.squared
    one_df <- data.frame("method"=c(method, method), "metric"=c('cover', 'biomass'),
                         "year"=c(year, year), "Rsq"=c(cover_rsq, biomass_rsq))
    summary_df <- rbind(summary_df, one_df)
  }
}
summary_table_path <- paste(fig_dir, "Simulated_vs_empirical_R_sq_summary.csv", sep='/')
write.csv(summary_df, summary_table_path, row.names=FALSE)

# compare biomass estimated by RPM: zero_sd vs uniform vs via_ndvi
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"

rpm_dat_path <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/RPM_results_zero_sd_uniform_via_ndvi.csv"
rpm_df <- read.csv(rpm_dat_path)
date_subs <- rpm_df[rpm_df$year >= 2014, ]
date_subs$site <- factor(date_subs$site, levels=1:15)

# for all sites
date_subs$method <- factor(date_subs$method,
                           levels=c('zero_sd', 'uniform', 'via_ndvi'),
                           labels=c('Without grazing', 'Uniform density', 'Disaggregated via NDVI'))
date_subs$plot <- factor(date_subs$plot,
                         levels=c('A', 'B', 'C', 'D', 'E'),
                         labels=c('50 m', '150 m', '350 m', '750 m', '1500 m'))
p <- ggplot(date_subs, aes(x=date, y=total_biomass_gm2, group=method))
p <- p + geom_line(aes(color=method)) + facet_grid(site~plot, scales='free')
p <- p + xlab("Date") + ylab("Standing biomass (g/m2)")
p <- p + scale_x_continuous(breaks=c(2014, 2015))
p <- p + theme(legend.position='bottom',
               legend.direction='horizontal')
pngname <- paste(fig_dir, "RPM_biomass_zerosd_uniform_ndvi_all_sites.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=10)
print(p)
dev.off()
# for sites 12 and 13 only (site with highest animal density)
subs_13 <- date_subs[(date_subs$site==13) | (date_subs$site==12), ]
p <- ggplot(subs_13, aes(x=date, y=total_biomass_gm2, group=method))
p <- p + geom_line(aes(color=method)) + facet_grid(site~plot, scales='free')
p <- p + xlab("Date") + ylab("Standing biomass (g/m2)")
p <- p + scale_x_continuous(breaks=c(2014, 2015))
p <- p + theme(legend.position='bottom',
               legend.direction='horizontal')
pngname <- paste(fig_dir, "RPM_biomass_zerosd_uniform_ndvi_sites12_13.png", sep='/')
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

# analyze difference between biomass: via NDVI vs uniform animal density
diff_fig_dir <- paste(fig_dir, 'biomass_ndvi_minus_uniform', sep='/')
dir.create(diff_fig_dir)
method_comp_df <- date_subs[(date_subs$method == 'Uniform density') |
                              (date_subs$method == 'Disaggregated via NDVI'), ]
method_df <- reshape(method_comp_df, v.names='total_biomass_gm2',
                     timevar='method',
                     idvar=c('year', 'month', 'site', 'plot', 'date'),
                     direction="wide")
method_df$diff_ndvi_minus_uniform <- method_df$`total_biomass_gm2.Disaggregated via NDVI` - method_df$`total_biomass_gm2.Uniform density`
method_df$perc_diff <- (method_df$diff_ndvi_minus_uniform / method_df$`total_biomass_gm2.Uniform density`) * 100
method_df <- merge(method_df, site_df)
# monthly diff between the two methods
p <- ggplot(method_df, aes(x=plot, y=perc_diff))  # y=diff_ndvi_minus_uniform))
p <- p + geom_boxplot()
p <- p + facet_wrap(~year, scales='free')
print(p)
p <- ggplot(method_df, aes(x=plot, y=perc_diff))  # y=diff_ndvi_minus_uniform))
p <- p + geom_boxplot()
p <- p + xlab("Distance from grazing hotspot") + ylab("% Difference:\nBiomass[via NDVI] - Biomass[uniform]")
p <- p + facet_wrap(~hotspot_type, scales='free')
print(p)
pngname <- paste(diff_fig_dir, "monthly_difference_by_hotspot_type_x_plot.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=3)
print(p)
dev.off()

# yearly mean diff between the two methods
mean_yearly_diff <- aggregate(perc_diff~year + plotid + site + plot, method_df, FUN=mean)
colnames(mean_yearly_diff) <- c('year', 'plotid', 'site', 'plot', 'yearly_perc_diff_mean')
mean_yearly_diff$year <- factor(mean_yearly_diff$year)
mean_yearly_diff$site <- factor(mean_yearly_diff$site, levels=1:15)
mean_yearly_diff <- merge(mean_yearly_diff, site_df)
p <- ggplot(mean_yearly_diff, aes(x=site, y=yearly_perc_diff_mean))
p <- p + geom_boxplot()
p <- p + facet_wrap(~year, scales='free')
pngname <- paste(diff_fig_dir, "mean_yearly_difference_by_site.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()
p <- ggplot(mean_yearly_diff, aes(x=site, y=yearly_perc_diff_mean))
p <- p + geom_boxplot()
p <- p + facet_wrap(~hotspot_type, scales='free')
print(p)
p <- ggplot(mean_yearly_diff, aes(x=plot, y=yearly_perc_diff_mean))
p <- p + geom_boxplot()
p <- p + facet_wrap(~site, nrow=5, scales='free')
p <- p + xlab("Distance from grazing hotspot (m)") + ylab("Yearly mean % difference:\n Biomass[via NDVI] - Biomass[uniform]")
pngname <- paste(diff_fig_dir, "mean_yearly_difference_by_plot_x_site.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=8)
print(p)
dev.off()
p <- ggplot(mean_yearly_diff, aes(x=plot, y=yearly_perc_diff_mean))
p <- p + geom_boxplot()
p <- p + facet_wrap(~hotspot_type, scales='free')
p <- p + xlab("Distance from grazing hotspot") + ylab("Yearly mean % difference:\n Biomass[via NDVI] - Biomass[uniform]")
print(p)
p <- ggplot(mean_yearly_diff, aes(x=plot, y=yearly_perc_diff_mean))
p <- p + geom_boxplot()
p <- p + xlab("Distance from grazing hotspot") + ylab("Yearly mean % difference:\n Biomass[via NDVI] - Biomass[uniform]")
pngname <- paste(diff_fig_dir, "mean_yearly_difference_by_plot_both_years.png", sep='/')
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

# ANOVA: analysis of the difference in biomass between uniform and via NDVI
# on yearly basis (each observation = average yearly difference at 1 plot in 1 year)
mean_yearly_diff$plot <- factor(mean_yearly_diff$plot)
mean_yearly_diff$site <- factor(mean_yearly_diff$site)
mean_yearly_diff$year <- factor(mean_yearly_diff$year)
anova_test <- aov(yearly_perc_diff_mean ~ plot + site + year, mean_yearly_diff)
summary(anova_test)
anova_test2 <- aov(yearly_perc_diff_mean ~ plot*site + year, mean_yearly_diff)
summary(anova_test2)
plot(anova_test2)
# post-hoc test: sig differences between groups
TukeyHSD(anova_test2)  # look for differences between plots

# Kruskal-Wallis, a nonparametric alternative to a one-way anova
kruskal.test(yearly_perc_diff_mean ~ plot, mean_yearly_diff)
pairwise.wilcox.test(mean_yearly_diff$yearly_perc_diff_mean, method_df$plot)

# compare animal density estimated via NDVI to empirical
via_ndvi_dir <- "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/via_ndvi"
sim_density_df <- read.csv(
  paste(via_ndvi_dir, "RPM_density_plot_centroids_via_ndvi.csv", sep='/'))
sim_density_df <- merge(sim_density_df, step_key_df)
plot_out <- strsplit(as.character(sim_density_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
sim_density_df <- cbind(sim_density_df, plot_df)
# empirical dung data
dung_df_path <- paste(processed_dir, 'dung_data.csv', sep='/')
dung_df <- read.csv(dung_df_path)
code_out <- strsplit(as.character(dung_df$code), '[[:alpha:]]')
code_df <- data.frame(do.call(rbind, code_out))
colnames(code_df) <- c('site', 'replicate')
dung_df <- cbind(dung_df, code_df)

# restrict plot to steps in 2014-2015
subs_density <- sim_density_df[sim_density_df$year >= 2014, ]
subs_density$date <- 0
for (r in 1:nrow(subs_density)){
  subs_density[r, 'date'] <- date_fun(subs_density[r, 'year'], subs_density[r, 'month'])
}
subs_density$site <- factor(subs_density$site, levels=c(1:15))
# include only the extreme distance treatments, distances A and E only
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
emp_subs$date <- 0
emp_subs[emp_subs$year == 2014, 'date'] <- subs_density[
  (subs_density$year == 2014) & (subs_density$month == 8), 'date']
emp_subs[emp_subs$year == 2015, 'date'] <- subs_density[
  (subs_density$year == 2015) & (subs_density$month == 8), 'date']
# correct MLU by the same factor I used above to calculate site-level total animals
adj_ratio <- 0.002055487  # calculated above
emp_subs$MLU_per_ha <- emp_subs$MLU * 100
emp_subs$MLU_cor <- emp_subs$MLU_per_ha * adj_ratio
emp_subs$distance <- factor(emp_subs$distance, levels=c('50 m', '1500 m'))
emp_subs$site <- factor(emp_subs$site, levels=c(1:15))

fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"
p <- ggplot(dist_subs, aes(x=date, y=animal_density, group=distance))
p <- p + geom_line(aes(color=distance)) + facet_wrap(~site, nrow=5, scales='free')
p <- p + xlab("Date") + ylab("Animal density (animals/ha)")
p <- p + scale_x_continuous(breaks=c(2014, 2015), labels=c('Jan-2014', 'Jan-2015'))
p <- p + theme(legend.position='bottom',
               legend.direction='horizontal')
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50m_1500m.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=7)
print(p)
dev.off()

p <- p + geom_jitter(data=emp_subs, aes(x=date, y=MLU_cor, color=distance), width=0.04)
pngname <- paste(fig_dir, "RPM_density_comparison_to_ndvi_50m_1500m_with_empirical.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=7)
print(p)
dev.off()

# is animal density usually higher at 50 m than at 1500 m?
dist_comp_df <- reshape(dist_subs, v.names='animal_density',
  timevar='plot', idvar=c('site', 'year', 'month', 'date'),
  direction='wide')
dist_comp_df$density_1500m_minus_50m <- dist_comp_df$animal_density.E - dist_comp_df$animal_density.A
summary(dist_comp_df$density_1500m_minus_50m)
dist_comp_df$density_50m_gt_1500m <- dist_comp_df$density_1500m_minus_50m < 0
count_gt <- aggregate(density_50m_gt_1500m ~ site, dist_comp_df, FUN=sum)
colnames(count_gt) <- c('site', 'num_months_50m_gt_1500m')
count_gt$proportion_months_50m_gt_1500m <- (
  count_gt$num_months_50m_gt_1500m / length(unique(dist_comp_df$date)))
p <- ggplot(count_gt, aes(x=site, y=proportion_months_50m_gt_1500m))
p <- p + geom_bar(stat='identity') + coord_flip()
p <- p + ylab("Proportion of months where\n density[50 m] > density[1500 m]") + xlab("Site")
pngname <- paste(fig_dir, "proportion_months_density50m_gt_density1500m.png", sep='/')
png(file=pngname, units="in", res=300, width=4, height=5)
print(p)
dev.off()

# taking all months across the simulation, not just 2014 and 2015
full_comp_df <- reshape(sim_density_df, v.names='animal_density',
  timevar='plot', idvar=c('site', 'year', 'month'), direction='wide')
full_comp_df$date <- 0
for (r in 1:nrow(full_comp_df)){
  full_comp_df[r, 'date'] <- date_fun(full_comp_df[r, 'year'], full_comp_df[r, 'month'])
}
full_comp_df$density_1500m_minus_50m <- full_comp_df$animal_density.E - full_comp_df$animal_density.A
full_comp_df$density_50m_gt_1500m <- full_comp_df$density_1500m_minus_50m < 0
full_count_gt <- aggregate(density_50m_gt_1500m ~ site, full_comp_df, FUN=sum)
colnames(full_count_gt) <- c('site', 'num_months_50m_gt_1500m')
full_count_gt$proportion_months_50m_gt_1500m <- (
  full_count_gt$num_months_50m_gt_1500m / length(unique(full_comp_df$date)))

# compare simulated biomass to range of empirical biomass across replicates
# generate this above
# rpm_dat_path <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/RPM_results_zero_sd_uniform_via_ndvi.csv"
rpm_df <- read.csv(rpm_dat_path)
mean_veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
mean_veg_df <- merge(mean_veg_df, veg_date_df)
rpm_mean_veg <- merge(rpm_df, mean_veg_df)  # to get only the months sampled
rpm_df <- rpm_mean_veg[, colnames(rpm_df)]
# process raw biomass dat to get range per plot
biomass_df <- read.csv(paste(processed_dir, 'veg_biomass_data.csv', sep='/'))
biomass_min <- aggregate(biomass_gm2 ~ site + plot + year, data=biomass_df, FUN=min)
colnames(biomass_min) <- c('site', 'plot', 'year', 'min_emp_biomass')
biomass_max <- aggregate(biomass_gm2 ~ site + plot + year, data=biomass_df, FUN=max)
colnames(biomass_max) <- c('site', 'plot', 'year', 'max_emp_biomass')
emp_df <- merge(biomass_min, biomass_max)
bio_sum_df <- merge(emp_df, rpm_df)
bio_sum_df$rpm_in_range <- 0
bio_sum_df[(bio_sum_df$total_biomass_gm2 >= bio_sum_df$min_emp_biomass) &
             (bio_sum_df$total_biomass_gm2 <= bio_sum_df$max_emp_biomass),
           'rpm_in_range'] <- 1
bio_sum_df$rpm_lt_min <- 0
bio_sum_df[(bio_sum_df$total_biomass_gm2 < bio_sum_df$min_emp_biomass),
           'rpm_lt_min'] <- 1
bio_sum_df$rpm_gt_max <- 0
bio_sum_df[(bio_sum_df$total_biomass_gm2 > bio_sum_df$max_emp_biomass),
           'rpm_gt_max'] <- 1
in_range_agg <- aggregate(rpm_in_range ~ method, bio_sum_df, FUN=mean)
lt_min_agg <- aggregate(rpm_lt_min ~ method, bio_sum_df, FUN=mean)
gt_max_agg <- aggregate(rpm_gt_max ~ method, bio_sum_df, FUN=mean)
range_sum_df <- merge(in_range_agg, lt_min_agg)
range_sum_df <- merge(range_sum_df, gt_max_agg)

# Generate site df
veg_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dat.txt', sep='/'),
                      header=TRUE)
site_df <- veg_dat[, c('site', 'hotspot_type')]
site_df <- site_df[!duplicated(site_df$site), ]

# Cover data
# create processed cover data for comparison with RPM results
veg_dat <- read.table(paste(data_dir, 'raw_data', 'Mongol_dat.txt', sep='/'),
                      header=TRUE)
cover_df <- veg_dat[, c('site', 'code', 'plot', 'replicate', 'date_2014', 'date_2015', 'cover_perc_2014', 'cover_perc_2015')]
cover_long_df <- reshape(cover_df, idvar=c('site', 'code', 'plot', 'replicate'),
                         varying=c('date_2014', 'date_2015'),
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
veg_df <- merged_df[, c('site', 'plot', 'replicate', 'year', 'month', 'day', 'cover')]
veg_df_path <- paste(processed_dir, 'veg_cover_data.csv', sep='/')
write.csv(veg_df, veg_df_path, row.names=FALSE)

# create processed biomass data
biomass_dat <- read.csv(paste(data_dir, 'raw_data', 'biomass_Mong_shared.csv', sep='/'))
colnames(biomass_dat) <- c('site', 'plot', 'replicate', 'year', 'biomass_gm2')
biomass_path <- paste(processed_dir, 'veg_biomass_data.csv', sep='/')
write.csv(biomass_dat, biomass_path, row.names=FALSE)

# vegetation sampling dates reclassified to match Century monthly time step
veg_df_path <- paste(processed_dir, 'veg_cover_data.csv', sep='/')
veg_df <- read.csv(veg_df_path)
veg_df$month_reclass <- NA
veg_df[veg_df$day < 15, 'month_reclass'] <- veg_df[veg_df$day < 15, 'month'] - 1
veg_df[veg_df$day >= 15, 'month_reclass'] <- veg_df[veg_df$day >= 15, 'month']
veg_date_df <- veg_df[, c('site', 'plot', 'year', 'month_reclass')]
colnames(veg_date_df)[4] <- 'month'
veg_date_df <- veg_date_df[!duplicated(veg_date_df), ]
write.csv(veg_date_df, paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'),
          row.names=FALSE)

# single data frame containing biomass and % cover, mean at plot level
biomass_df <- read.csv(paste(processed_dir, 'veg_biomass_data.csv', sep='/'))
biomass_agg <- aggregate(biomass_gm2 ~ site + plot + year, data=biomass_df, FUN=mean)
cover_df <- read.csv(paste(processed_dir, 'veg_cover_data.csv', sep='/'))
cover_df <- cover_df[, c('site', 'plot', 'replicate', 'year', 'cover')]
cover_agg <- aggregate(cover ~ site + plot + year, data=cover_df, FUN=mean)
veg_df <- merge(biomass_agg, cover_agg, all=TRUE)
write.csv(veg_df, paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'),
          row.names=FALSE)

# process livestock data from National Statistics Office, downloaded from 1212.mn
nso_by_category <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/National_Statistics_Office/livestock_by_type_2014_2015_2018.csv")
# calculate SFU
df_list <- list()
for(year in c(2014, 2015, 2018)) {
  nso_1year <- nso_by_category[, c("Aimag_nso_dat", "Soum_nso_dat", 'type', year)]
  colnames(nso_1year) <- c("Aimag_nso_dat", "Soum_nso_dat", 'type', 'year')
  nso_wide <- reshape(nso_1year, timevar='type', v.names='year',
                      idvar=c('Aimag_nso_dat', 'Soum_nso_dat'), direction='wide')
  colnames(nso_wide) <- c('Aimag_nso_dat', 'Soum_nso_dat', unique(nso_by_category$type))
  nso_wide$SFU <- ((nso_wide$Horse * 7) + (nso_wide$Cattle * 6) + (nso_wide$Camel * 5) +
                     nso_wide$Sheep + (nso_wide$Goat * 0.9))
  nso_wide <- nso_wide[, c('Aimag_nso_dat', 'Soum_nso_dat', 'SFU')]
  nso_wide$year <- year
  df_list[[year]] <- nso_wide
}
sfu_df <- do.call(rbind, df_list)  # Multiply SFU by 1000, these data are in units of 1000
area_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn summaries_GK/Site_centroid_Aimag_soum_table.csv")
area_df <- area_df[, c('Site', "area_ha", "Aimag_nso_dat", "Soum_nso_dat")]
sfu_df <- merge(sfu_df, area_df)
sfu_df$sfu_per_ha <- sfu_df$SFU / sfu_df$area_ha
write.csv(sfu_df, paste(processed_dir, "sfu_per_ha_by_site_centroid_2014_2015_2018.csv", sep='/'),
          row.names=FALSE)

# calculate linear regression to get cover from biomass
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
veg_df$biomass_kgha <- veg_df$biomass_gm2 * 10
cover_lm <- lm(cover~biomass_kgha, data=veg_df)
summary(cover_lm)

# productivity and precip vs empirical animal density
est_sfu_per_ha_df <- read.csv(paste(processed_dir, "est_sfu_per_ha.csv", sep='/'))
peak_biomass_df <- read.csv(paste(processed_dir, 'peak_biomass_from_Century.csv', sep='/'))
biomass_sfu_df <- merge(est_sfu_per_ha_df, peak_biomass_df, by.x='site', by.y='site_id')
biomass_sfu_df$ratio <- biomass_sfu_df$mMLU_per_ha_1500 / biomass_sfu_df$sfu_per_ha_nso.2015
biomass_sfu_df$site <- factor(biomass_sfu_df$site)
worldclim_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_inputs/Ahlborn_sites/intermediate_data/worldclim_precip.csv")
worldclim_df <- aggregate(prec~site, worldclim_df, FUN=sum)
colnames(worldclim_df) <- c('site', 'annual_prec_worldclim')
biomass_sfu_df <- merge(biomass_sfu_df, worldclim_df)

subs_df <- biomass_sfu_df[biomass_sfu_df$site != 5, ]
precip_model <- lm(mMLU_per_ha_1500~annual_prec_worldclim, data=subs_df)
summary(precip_model)

# calculate the residual of each site from best-fit line excluding site 5, an outlier
residual_df <- data.frame(
  site=biomass_sfu_df$site,
  predicted=predict.lm(precip_model, newdata=data.frame(annual_prec_worldclim=biomass_sfu_df$annual_prec_worldclim)),
  obs=biomass_sfu_df$mMLU_per_ha_1500)
residual_df$residual <- residual_df$obs - residual_df$predicted
save_df <- residual_df[, c('site', 'residual')]
write.csv(save_df, paste(processed_dir, 'lm_dung_from_precip_residuals.csv', sep='/'),
          row.names=FALSE)

# plot dung counts at 1500 site, and est sfu per site
dung_resh <- reshape(biomass_sfu_df, idvar=c('site', 'annual_prec_worldclim'),
                     varying=c('mMLU_per_ha_1500', 'est_animals_per_ha'),
                     v.names='density', timevar='source', times=c('Dung (count/ha)', 'Sheep forage unit (animal/ha)'), direction='long',
                     drop=c('sfu_per_ha_nso.2015', 'live_biomass', 'total_biomass', 'total_biomass_kgha',
                            'live_biomass_kgha', 'month', 'ratio'))
dung_resh$source <- factor(dung_resh$source, levels=c('Sheep forage unit (animal/ha)', 'Dung (count/ha)'))
p <- ggplot(dung_resh, aes(x=annual_prec_worldclim, y=density))
p <- p + geom_point() + facet_wrap(~source, scales='free')
p <- p + xlab('Average annual precipitation (cm)') + ylab('Animal density') + print_theme
print(p)
pngname <- paste(processed_dir, "precip_vs_dung_density_est_sfu_density.png", sep='/')
png(file=pngname, units="in", res=300, width=7, height=3.5)
print(p)
dev.off()

p <- ggplot(biomass_sfu_df, aes(x=total_biomass_kgha, y=est_animals_per_ha))
p <- p + geom_point() + xlab('Simulated peak biomass (kg/ha)') + ylab('Dung density (count/ha)')
p <- p + print_theme + geom_text(aes(label=site), hjust=0, vjust=0, nudge_x=8)
print(p)
pngname <- paste(processed_dir, "productivity_vs_dung_density.png", sep='/')
pngname <- paste(processed_dir, "productivity_vs_dung_density_nolabels.png", sep='/')
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

# leftover exploratory analysis
# compare Century biomass to grass biomass only
peak_biomass_df <- read.csv(peak_biomass_path)
grassbysite_2014 <- grass_by_site[grass_by_site$year == 2014, ]
herb_merged <- merge(peak_biomass_df, grassbysite_2014)
# mean bias
herb_bias <- mean(herb_merged$total_biomass_kgha - herb_merged$grass_kgha)
herb_peak_cor <- cor.test(herb_merged$total_biomass_kgha, herb_merged$grass_kgha)
diff <- herb_merged$total_biomass_kgha - herb_merged$grass_kgha
diff_sq <- diff * diff
herb_peak_rmse <- sqrt(mean(diff_sq))
p <- ggplot(herb_merged, aes(x=total_biomass_kgha, y=grass_kgha))
p <- p + geom_point() + xlab("Peak biomass (Century)") + ylab("Empirical grass biomass, 2014")
# p <- p + geom_abline(slope=1, intercept=0, linetype='dashed')
p <- p + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.5) + print_theme
print(p)
pngname <- paste(fig_dir, "peak_biomass_Century_v_2014grassbiomass.png", sep='/')
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()