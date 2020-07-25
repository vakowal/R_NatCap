# Process field data from Julian Ahlborn
data_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn"
processed_dir <- paste(data_dir, 'summaries_GK')
dir.create(processed_dir)

# CHIRPS precip data at sampling plots
chirps_raw_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn/CHIRPS_precip_plot_centroids.csv")
step_key_df <- read.csv(
  "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd/step_key.csv")
colnames(step_key_df) <- c('year', 'month', 'step')
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
p <- ggplot(cum_chirps_veg_df, aes(x=precip_cm_12mo, y=mean_perc_cover))
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

# Century results at Julian's sites
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
colnames(j_table1) <- c('site_id', 'Site_table1', 'latitude', 'longitude', 'altitude', 'hotspot_type',
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
biomass_df <- plot_df[, c('site_id', 'annual_prec_worldclim',
                            'total_biomass_kgha', 'biomass_kg_ha')]
colnames(biomass_df) <- c('site_id', 'annual_prec_worldclim',
                          'biomass_Century', 'biomass_table1')
biomass_re <- reshape(biomass_df, varying=c('biomass_Century', 'biomass_table1'),
                      v.names='biomass_kg_ha', timevar='source',
                      times=c('Century', 'Table 1'),
                      idvar=c('site_id', 'annual_prec_worldclim'),
                      direction='long')
p <- ggplot(biomass_re, aes(x=annual_prec_worldclim, y=biomass_kg_ha))
p <- p + geom_point() + geom_text(aes(label=site_id), hjust=0, vjust=0, nudge_x=0.3)
p <- p + facet_wrap(~source, nrow=1, scales='free')
p <- p + xlab("Annual average precip: Worldclim (cm)") + ylab("Biomass (kg/ha)")
print(p)
pngname <- paste(fig_dir, "biomass_v_annual_precip_Julian_table1_Century.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

# RPM results
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"

# zero density
zero_sd_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd"
step_key_df <- read.csv(
  paste(zero_sd_dir, "step_key.csv", sep='/'))
colnames(step_key_df) <- c('year', 'month', 'step')

# site centroid results: compare to Century and Julian's table 1
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

# RPM results at plot centroid
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
colnames(veg_df) <- c('site', 'plot', 'year', 'empirical_biomass_gm2', 'cover')
veg_date_df <- read.csv(paste(processed_dir, 'veg_sampling_dates_reclass.csv', sep='/'))
veg_df <- merge(veg_df, veg_date_df)
sim_plot_df <- read.csv(
  paste(zero_sd_dir, "RPM_biomass_plot_centroids_zero_sd.csv", sep='/'))
plot_out <- strsplit(as.character(sim_plot_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
sim_plot_df <- cbind(sim_plot_df, plot_df)
sim_plot_df <- merge(step_key_df, sim_plot_df)
sim_plot_df <- sim_plot_df[, c(
  'site', 'plot', 'year', 'month', 'plotid',
  'aboveground_live_biomass_gm2', 'total_biomass_gm2')]
colnames(sim_plot_df) <- c(
  'site', 'plot', 'year', 'month', 'plotid',
  'sim_aboveground_live_biomass_gm2', 'sim_total_biomass_gm2')
merged_df <- merge(veg_df, sim_plot_df, by=c('site', 'plot', 'year', 'month'))

combined_cover_lm <- lm(cover~sim_total_biomass_gm2, data=merged_df)
summary(combined_cover_lm)$r.squared  # 0.64
combined_biomass_lm <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=merged_df)
summary(combined_biomass_lm)$r.squared  # 0.48

# separate comparison by year
zero_sd_2014 <- merged_df[merged_df$year == 2014, ]
cover_lm_2014 <- lm(cover~sim_total_biomass_gm2, data=zero_sd_2014)
summary(cover_lm_2014)$r.squared  # 0.69
biomass_lm_2014 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=zero_sd_2014)
summary(biomass_lm_2014)$r.squared  # 0.5

zero_sd_2015 <- merged_df[merged_df$year == 2015, ]
cover_lm_2015 <- lm(cover~sim_total_biomass_gm2, data=zero_sd_2015)
summary(cover_lm_2015)$r.squared  # 0.49
biomass_lm_2015 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=zero_sd_2015)
summary(biomass_lm_2015)$r.squared  # 0.39

# RPM results with uniform (corrected) mean MLU per ha average across 2014 and 2015
uniform_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/uniform_density_mMLU_per_site"
uniform_df <- read.csv(
  paste(uniform_dir,
    "RPM_biomass_plot_centroids_uniform_density_mMLU_per_site.csv", sep='/'))
colnames(uniform_df)[6] <- 'sim_total_biomass_gm2'
plot_out <- strsplit(as.character(uniform_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
uniform_df <- cbind(uniform_df, plot_df)
uniform_df <- merge(step_key_df, uniform_df)
merged_df <- merge(veg_df, uniform_df, by=c('site', 'plot', 'year', 'month'))

combined_cover_lm <- lm(cover~sim_total_biomass_gm2, data=merged_df)
summary(combined_cover_lm)$r.squared  # 0.61
combined_biomass_lm <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=merged_df)
summary(combined_biomass_lm)$r.squared  # 0.48

# separate comparison by year
uniform_2014 <- merged_df[merged_df$year == 2014, ]
cover_lm_2014 <- lm(cover~sim_total_biomass_gm2, data=uniform_2014)
summary(cover_lm_2014)$r.squared  # 0.67
biomass_lm_2014 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=uniform_2014)
summary(biomass_lm_2014)$r.squared  # 0.5

uniform_2015 <- merged_df[merged_df$year == 2015, ]
cover_lm_2015 <- lm(cover~sim_total_biomass_gm2, data=uniform_2015)
summary(cover_lm_2015)$r.squared  # 0.47
biomass_lm_2015 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=uniform_2015)
summary(biomass_lm_2015)$r.squared  # 0.42

p <- ggplot(merged_df, aes(x=sim_total_biomass_gm2, y=empirical_biomass_gm2))
p <- p + geom_point() + facet_grid(~year, scales="free")
p <- p + xlab("Total simulated biomass (g/m2)") + ylab("Empirical biomass (g/m2)")
print(p)

# RPM results using NDVI to disaggregate animals
via_ndvi_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/compare_to_ndvi"
ndvi_df = read.csv(
  paste(via_ndvi_dir, "RPM_biomass_plot_centroids_compare_to_ndvi.csv", sep='/'))
ndvi_step_key_df = read.csv(paste(via_ndvi_dir, 'step_key.csv', sep='/'))
colnames(ndvi_step_key_df) <- c('year', 'month', 'step')
colnames(ndvi_df)[8] <- 'sim_total_biomass_gm2'
plot_out <- strsplit(as.character(ndvi_df$plotid), '[ ]')
plot_df <- data.frame(do.call(rbind, plot_out))
colnames(plot_df) <- c('site', 'plot')
ndvi_df <- cbind(ndvi_df, plot_df)
ndvi_df <- merge(ndvi_step_key_df, ndvi_df)
merged_df <- merge(veg_df, ndvi_df, by=c('site', 'plot', 'year', 'month'))

combined_cover_lm <- lm(cover~sim_total_biomass_gm2, data=merged_df)
summary(combined_cover_lm)$r.squared  # 0.62
combined_biomass_lm <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=merged_df)
summary(combined_biomass_lm)$r.squared  # 0.48

# separate comparison by year
ndvi_2014 <- merged_df[merged_df$year == 2014, ]
cover_lm_2014 <- lm(cover~sim_total_biomass_gm2, data=ndvi_2014)
summary(cover_lm_2014)$r.squared  # 0.71
biomass_lm_2014 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=ndvi_2014)
summary(biomass_lm_2014)$r.squared  # 0.51

ndvi_2015 <- merged_df[merged_df$year == 2015, ]
cover_lm_2015 <- lm(cover~sim_total_biomass_gm2, data=ndvi_2015)
summary(cover_lm_2015)$r.squared  # 0.47
biomass_lm_2015 <- lm(empirical_biomass_gm2~sim_total_biomass_gm2, data=ndvi_2015)
summary(biomass_lm_2015)$r.squared  # 0.41

p <- ggplot(merged_df, aes(x=sim_total_biomass_gm2, y=empirical_biomass_gm2))
p <- p + geom_point() + facet_grid(~year, scales="free")
p <- p + xlab("Total simulated biomass (g/m2)") + ylab("Empirical biomass (g/m2)")
print(p)

# compare biomass estimated by RPM: zero_sd vs uniform vs via_ndvi
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_sites/summary_figs"

# zero density
zero_sd_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/zero_sd"
step_key_df <- read.csv(
  paste(zero_sd_dir, "step_key.csv", sep='/'))
colnames(step_key_df) <- c('year', 'month', 'step')
zero_sd_df <- read.csv(
  paste(zero_sd_dir, "RPM_biomass_plot_centroids_zero_sd.csv", sep='/'))
zero_sd_df <- merge(step_key_df, zero_sd_df)
zero_sd_df <- zero_sd_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
zero_sd_df$method <- 'zero_sd'

# uniform density
uniform_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/uniform_density_mMLU_per_site"
uniform_df <- read.csv(
  paste(uniform_dir,
    "RPM_biomass_plot_centroids_uniform_density_mMLU_per_site.csv", sep='/'))
uniform_df <- merge(step_key_df, uniform_df)
uniform_df <- uniform_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
uniform_df$method <- 'uniform'

# via NDVI
via_ndvi_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_sites_RPM_outputs/compare_to_ndvi"
ndvi_df = read.csv(
  paste(via_ndvi_dir, "RPM_biomass_plot_centroids_compare_to_ndvi.csv", sep='/'))
ndvi_step_key_df = read.csv(paste(via_ndvi_dir, 'step_key.csv', sep='/'))
colnames(ndvi_step_key_df) <- c('year', 'month', 'step')
ndvi_df <- merge(ndvi_df, ndvi_step_key_df)
ndvi_df <- ndvi_df[, c(
  'year', 'month', 'plotid', 'total_biomass_gm2')]
ndvi_df$method <- 'via_ndvi'

rpm_df <- rbind(zero_sd_df, uniform_df, ndvi_df)
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
method_comp_df <- date_subs[(date_subs$method == 'uniform') |
                              (date_subs$method == 'via_ndvi'), ]
method_df <- reshape(method_comp_df, v.names='total_biomass_gm2',
                     timevar='method',
                     idvar=c('year', 'month', 'site', 'plot', 'date'),
                     direction="wide")
method_df$diff_ndvi_minus_uniform <- method_df$total_biomass_gm2.via_ndvi - method_df$total_biomass_gm2.uniform
method_df$perc_diff <- (method_df$diff_ndvi_minus_uniform / method_df$total_biomass_gm2.uniform) * 100
method_df <- merge(method_df, site_df)
method_df$plot <- factor(method_df$plot,
                                levels=c('A', 'B', 'C', 'D', 'E'),
                                labels=c('50', '150', '350', '750', '1500'))
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
anova_test3 <- aov(yearly_perc_diff_mean ~ plot*hotspot_type + year, mean_yearly_diff)
summary(anova_test3)

# Kruskal-Wallis, a nonparametric alternative to a one-way anova
kruskal.test(yearly_perc_diff_mean ~ plot, mean_yearly_diff)
pairwise.wilcox.test(mean_yearly_diff$yearly_perc_diff_mean, method_df$plot)

# compare animal density estimated via NDVI to empirical
sim_density_df <- read.csv(
  paste(via_ndvi_dir, "RPM_density_plot_centroids_compare_to_ndvi.csv", sep='/'))
sim_density_df <- merge(sim_density_df, ndvi_step_key_df)
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
# this correction accounts for conversion to MLU/ha (* 100) and correction (/ 758)
emp_subs$MLU_cor <- emp_subs$MLU / 7.58
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
