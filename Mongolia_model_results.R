# examine Mongolia simulations
library(ggplot2)

# results of back-calculate to match 2016 sites
back_calc_match_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec_back_calc/match_summary.csv"
back_calc_match_df <- read.csv(back_calc_match_csv)
back_calc_match_df$diff <- back_calc_match_df$Simulated_biomass - back_calc_match_df$Empirical_biomass
back_calc_match_df$schedule <- 'calibrated'
back_calc_match_df[back_calc_match_df$Iteration == 0, 'schedule'] <- 'default'
back_calc_reshape <- reshape(back_calc_match_df, idvar='site', timevar='schedule',
                             v.names=c('diff', 'Empirical_biomass', 'Simulated_biomass'),
                             direction='wide')
back_calc_reshape <- back_calc_reshape[, c('site', 'Empirical_biomass.default', 'Simulated_biomass.default',
                                           'Simulated_biomass.calibrated', 'diff.default', 'diff.calibrated')]
for (r in 1:NROW(back_calc_reshape)) {
  if (is.na(back_calc_reshape[r, 'Simulated_biomass.calibrated'])) {
    back_calc_reshape[r, 'Simulated_biomass.calibrated'] <- back_calc_reshape[r, 'Simulated_biomass.default']
    back_calc_reshape[r, 'diff.calibrated'] <- back_calc_reshape[r, 'diff.default']
  }
}
colnames(back_calc_reshape)[2] <- 'Empirical_biomass'
write.csv(back_calc_reshape, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec_back_calc/match_diff_summary.csv",
          row.names=FALSE)

# distribution of ending mismatch, after calibration:
hist(back_calc_reshape$diff.calibrated, breaks=100)
summary(back_calc_reshape$diff.calibrated)
# number of sites that were successfully matched
dim(back_calc_reshape[abs(back_calc_reshape$diff.calibrated) < 5, ])  # 19
# number of sites where we couldn't impose enough grazing
dim(back_calc_reshape[back_calc_reshape$diff.calibrated > 5, ])  # 79
# number of sites where we couldn't remove enough grazing
dim(back_calc_reshape[back_calc_reshape$diff.calibrated < -5, ])  # 24

# model outputs vs empirical biomass, potential (no grazing) vs standing (after grazing)
sum_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/SCP_sites/CBM_SCP_sites_2016_herb_biomass_potential_standing_run_7.30.19.csv"
sum_df <- read.csv(sum_csv)
sum_df[sum_df == -9999] <- NA
sum_df$Herb_biomass_prior <- sum_df$Herb_biomass
sum_df$Herb_biomass <- sum_df$Herb_biomass_prior * 10

cor_df <- data.frame('modeled_biomass_source'=character(),
                     'spearman_rho'=numeric(), 'spearman_pval'=numeric(), 
                     'pearson_cor'=numeric(), 'pearson_pval'=numeric(),
                     stringsAsFactors=FALSE)
p <- ggplot(sum_df, aes(x=Herb_biomass, y=potential))
p <- p + geom_point()
print(p)
cor_potential_p <- cor.test(sum_df$Herb_biomass, sum_df$potential,
                                  method='pearson')
cor_potential_s <- cor.test(sum_df$Herb_biomass, sum_df$potential,
                                   method='spearman')
i <- 1
cor_df[i, 'modeled_biomass_source'] <- 'potential'
cor_df[i, 'spearman_rho'] <- cor_potential_s[[4]]
cor_df[i, 'spearman_pval'] <- cor_potential_s[[3]]
cor_df[i, 'pearson_cor'] <- cor_potential_p[[4]]
cor_df[i, 'pearson_pval'] <- cor_potential_p[[3]]

p <- ggplot(sum_df, aes(x=Herb_biomass, y=standing))
p <- p + geom_point()
print(p)
cor_standing_p <- cor.test(sum_df$Herb_biomass, sum_df$standing,
                                  method='pearson')
cor_standing_s <- cor.test(sum_df$Herb_biomass, sum_df$standing,
                                   method='spearman')
i <- 2
cor_df[i, 'modeled_biomass_source'] <- 'standing'
cor_df[i, 'spearman_rho'] <- cor_standing_s[[4]]
cor_df[i, 'spearman_pval'] <- cor_standing_s[[3]]
cor_df[i, 'pearson_cor'] <- cor_standing_p[[4]]
cor_df[i, 'pearson_pval'] <- cor_standing_p[[3]]
write.csv(cor_df, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/SCP_sites/correlation_summary_empirical_vs_modeled.csv")

# reshape to plot potential and standing on same plot
pot_df <- sum_df[, c('Herb_biomass', 'potential')]
pot_df$modeled_biomass <- pot_df$potential
pot_df$source <- 'potential'
pot_df <- pot_df[, c('Herb_biomass', 'modeled_biomass', 'source')]
std_df <- sum_df[, c('Herb_biomass', 'standing')]
std_df$modeled_biomass <- std_df$standing
std_df$source <- 'standing'
std_df <- std_df[, c('Herb_biomass', 'modeled_biomass', 'source')]
long_df <- rbind(pot_df, std_df)
p <- ggplot(long_df, aes(x=Herb_biomass, y=modeled_biomass))
p <- p + geom_point(aes(color=source))
p <- p + xlab("Empirical biomass (kg/ha)") + ylab("Modeled biomass (kg/ha)")
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/SCP_sites/empirical_vs_modeled_potential_standing.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# plot 10 sites at a time:
# biomass (observed from NDVI vs potential without grazing vs standing after grazing)
output_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/biomass_WCS_sampling_points/line_plots_emp_vs_zero_sd"
site_id_list <- unique(sum_df$site_id)
plot_idx <- 1
for (i in seq(1, 153, by=10)) {
  site_id_subs <- site_id_list[c(i:(i+9))]
  rand_df <- sum_df[(sum_df$site_id %in% site_id_subs), ]
  p <- ggplot(rand_df, aes(x=step, y=biomass))
  p <- p + geom_line(aes(color=biomass_source))
  p <- p + facet_grid(source~site_id)
  p <- p + ggtitle(sprintf("plot #%s", plot_idx))
  pngname <- paste(output_dir, sprintf('biomass_plot_%d.png', plot_idx), sep='/')
  png(file=pngname, units="in", res=300, width=10, height=3)
  print(p)
  dev.off()
  plot_idx <- plot_idx + 1
}

# animal diet sufficiency
anim_df <- sum_df[sum_df$source == 'empirical_sd', ]
output_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/biomass_WCS_sampling_points/line_plots_emp_vs_zero_sd"
site_id_list <- unique(sum_df$site_id)
plot_idx <- 1
for (i in seq(1, 150, by=15)) {
  site_id_subs <- site_id_list[c(i:(i+14))]
  rand_df <- anim_df[(anim_df$site_id %in% site_id_subs), ]
  p <- ggplot(rand_df, aes(x=step, y=diet_sufficiency))
  p <- p + geom_line()
  p <- p + facet_wrap(~site_id, nrow=3)
  p <- p + ggtitle(sprintf("plot #%s", plot_idx))
  pngname <- paste(output_dir, sprintf('diet_sufficiency_plot_%d.png', plot_idx), sep='/')
  png(file=pngname, units="in", res=300, width=8, height=6)
  print(p)
  dev.off()
  plot_idx <- plot_idx + 1
}

# simulated results at sampling points with zero stocking density:
# beta model vs new model
sum_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/biomass_WCS_sampling_points/modeled_biomass_summary.csv"
sum_df <- read.csv(sum_csv, stringsAsFactors=FALSE)

# line plots comparing beta to new model, in groups of 15 sites
output_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/biomass_WCS_sampling_points/line_plots"
site_id_list <- unique(sum_df$site_id)
plot_idx <- 1
for (i in seq(1, 150, by=15)) {
  site_id_subs <- site_id_list[c(i:(i+14))]
  rand_df <- sum_df[(sum_df$site_id %in% site_id_subs) &
                      (sum_df$green_biomass_gm2 > -10), ]
  p <- ggplot(rand_df, aes(x=step, y=green_biomass_gm2))
  p <- p + geom_line(aes(colour=source))
  p <- p + facet_wrap(~site_id, nrow=3)
  p <- p + ggtitle(sprintf("plot #%s", plot_idx))
  pngname <- paste(output_dir, sprintf('green_biomass_%d.png', plot_idx), sep='/')
  png(file=pngname, units="in", res=300, width=8, height=6)
  print(p)
  dev.off()
  plot_idx <- plot_idx + 1
}
plot_idx <- 1
for (i in seq(1, 150, by=15)) {
  site_id_subs <- site_id_list[c(i:(i+14))]
  rand_df <- sum_df[(sum_df$site_id %in% site_id_subs) &
                      (sum_df$dead_biomass_gm2 > -10), ]
  p <- ggplot(rand_df, aes(x=step, y=dead_biomass_gm2))
  p <- p + geom_line(aes(colour=source))
  p <- p + facet_wrap(~site_id, nrow=3)
  p <- p + ggtitle(sprintf("plot #%s", plot_idx))
  pngname <- paste(output_dir, sprintf('dead_biomass_%d.png', plot_idx), sep='/')
  png(file=pngname, units="in", res=300, width=8, height=6)
  print(p)
  dev.off()
  plot_idx <- plot_idx + 1
}
plot_idx <- 1
for (i in seq(1, 150, by=15)) {
  site_id_subs <- site_id_list[c(i:(i+14))]
  rand_df <- sum_df[(sum_df$site_id %in% site_id_subs) &
                      (sum_df$dead_biomass_gm2 > -10), ]
  p <- ggplot(rand_df, aes(x=step, y=total_biomass_gm2))
  p <- p + geom_line(aes(colour=source))
  p <- p + facet_wrap(~site_id, nrow=3)
  p <- p + ggtitle(sprintf("plot #%s", plot_idx))
  pngname <- paste(output_dir, sprintf('total_biomass_%d.png', plot_idx), sep='/')
  png(file=pngname, units="in", res=300, width=8, height=6)
  print(p)
  dev.off()
  plot_idx <- plot_idx + 1
}

###

match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/soum_ctr_match_table.csv")

# find most and least variable CHIRPS pixels in each soum
match_pixel_soum <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/CHIRPS/CHIRPS_pixel_soum_ctr_20km_match.csv")
ch_min_max <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/min_max_biomass.csv")
ch_min_max <- merge(ch_min_max, match_pixel_soum, by='site_id')
ch_min_max$range <- ch_min_max$max_biomass - ch_min_max$min_biomass
nrows <- length(unique(match_pixel_soum$site_id_soum))
range_df <- data.frame('soum_site_id'=numeric(nrows), 'min_range_pixel_id'=numeric(nrows),
                       'max_range_pixel_id'=numeric(nrows), 'min_peak_pixel_id'=numeric(nrows),
                       'max_peak_pixel_id'=numeric(nrows))
for(i in 1:length(unique(match_pixel_soum$site_id_soum))){
  soum <- unique(match_pixel_soum$site_id_soum)[i]
  subs <- ch_min_max[ch_min_max$site_id_soum == soum, ]
  min_range <- subs[subs$range == min(subs$range), 'site_id']
  max_range <- subs[subs$range == max(subs$range), 'site_id']
  min_peak <- subs[subs$max_biomass == min(subs$max_biomass), 'site_id']
  max_peak <- subs[subs$max_biomass == max(subs$max_biomass), 'site_id']
  range_df[i, 'soum_site_id'] <- soum
  range_df[i, 'min_range_pixel_id'] <- min_range
  range_df[i, 'max_range_pixel_id'] <- max_range
  range_df[i, 'min_peak_pixel_id'] <- min_peak
  range_df[i, 'max_peak_pixel_id'] <- max_peak
}

ch_res_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/chirps_prec/zero_sd"
nm_res_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/namem_clim/zero_sd"
df_list <- list()
i <- 1
for(soum in unique(range_df$soum_site_id)){
  nm_df <- read.csv(paste(nm_res_dir, soum, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", soum, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", soum, sep=""), 'green_kgha', sep='_'))
  nm_df <- nm_df[, cols]
  colnames(nm_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  nm_df$total_biomass_kgha <- nm_df$dead_kgha + nm_df$green_kgha
  nm_df$source <- 'NAMEM'
  nm_df$soum <- soum
  df_list[[i]] <- nm_df
  i <- i + 1
  
  pixel_id <- range_df[range_df$soum_site_id == soum, 'min_range_pixel_id']
  ch_df <- read.csv(paste(ch_res_dir, pixel_id, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", pixel_id, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", pixel_id, sep=""), 'green_kgha', sep='_'))
  ch_df <- ch_df[, cols]
  colnames(ch_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  ch_df$total_biomass_kgha <- ch_df$dead_kgha + ch_df$green_kgha
  ch_df$source <- 'CHIRPS_min_range'
  ch_df$soum <- soum
  df_list[[i]] <- ch_df
  i <- i + 1
  
  pixel_id <- range_df[range_df$soum_site_id == soum, 'max_range_pixel_id']
  ch_df <- read.csv(paste(ch_res_dir, pixel_id, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", pixel_id, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", pixel_id, sep=""), 'green_kgha', sep='_'))
  ch_df <- ch_df[, cols]
  colnames(ch_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  ch_df$total_biomass_kgha <- ch_df$dead_kgha + ch_df$green_kgha
  ch_df$source <- 'CHIRPS_max_range'
  ch_df$soum <- soum
  df_list[[i]] <- ch_df
  i <- i + 1
  
  pixel_id <- range_df[range_df$soum_site_id == soum, 'min_peak_pixel_id']
  ch_df <- read.csv(paste(ch_res_dir, pixel_id, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", pixel_id, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", pixel_id, sep=""), 'green_kgha', sep='_'))
  ch_df <- ch_df[, cols]
  colnames(ch_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  ch_df$total_biomass_kgha <- ch_df$dead_kgha + ch_df$green_kgha
  ch_df$source <- 'CHIRPS_min_peak'
  ch_df$soum <- soum
  df_list[[i]] <- ch_df
  i <- i + 1
  
  pixel_id <- range_df[range_df$soum_site_id == soum, 'max_peak_pixel_id']
  ch_df <- read.csv(paste(ch_res_dir, pixel_id, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", pixel_id, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", pixel_id, sep=""), 'green_kgha', sep='_'))
  ch_df <- ch_df[, cols]
  colnames(ch_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  ch_df$total_biomass_kgha <- ch_df$dead_kgha + ch_df$green_kgha
  ch_df$source <- 'CHIRPS_max_peak'
  ch_df$soum <- soum
  df_list[[i]] <- ch_df
  i <- i + 1
}
variability_df <- do.call(rbind, df_list)
variability_df$date <- as.Date(paste(variability_df$year, variability_df$month, '01', sep="-"),
                               format="%Y-%m-%d")
variability_df <- merge(variability_df, match_df, by.x='soum', by.y='site_id')
variability_df$total_biomass_gm2 <- variability_df$total_biomass_kgha / 10
minmax_range_df <- variability_df[variability_df$source %in%
                             c('CHIRPS_min_range', 'CHIRPS_max_range', 'NAMEM'), ]
minmax_range_df$source <- factor(minmax_range_df$source,
                                 levels=c('CHIRPS_min_range', 'CHIRPS_max_range', 'NAMEM'),
                                 labels=c('CHIRPS: min range', 'CHIRPS: max range', 'NAMEM'))
lines <- c('dashed', 'dotted', 'solid')
p <- ggplot(minmax_range_df, aes(x=date, y=total_biomass_gm2))
p <- p + geom_line(aes(linetype=source))
p <- p + scale_linetype_manual(values=lines)
p <- p + facet_wrap(~name_en, scales='free')
p <- p + scale_x_date(date_breaks = "6 months", date_labels=("%m/%y"))
p <- p + ylab("Total biomass (g/m2)") + xlab("Date")
p <- p + theme(legend.position="bottom", legend.title = element_blank())
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/AGU_Dec_2017/variability_NAMEM_vs_CHIRPS_range_20km_buf_dash_dot.png"
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

# add points for empirical biomass
emp_match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/nearest_SCP_site_soum_centers.csv")
emp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/SCP_herb_biomass.csv")
emp_df <- merge(emp_match_df, emp_df, by.x='site_id_SCP', by.y='site_id')
emp_df$source <- 'Empirical monitoring'
emp_df <- merge(emp_df, match_df, by.x='site_id_soum', by.y='site_id')
emp_df <- emp_df[, c("Date", 'biomass_g_m2', 'source', 'name_en')]
colnames(emp_df) <- c('date', 'total_biomass_gm2', 'source', 'name_en')
emp_df$date <- as.Date(emp_df$date, format='%Y-%m-%d')

minmax_peak_df <- variability_df[variability_df$source %in%
                                    c('CHIRPS_min_peak', 'CHIRPS_max_peak', 'NAMEM'),
                                 c('date', 'total_biomass_gm2', 'source', 'name_en')]
# minmax_peak_df <- rbind(minmax_peak_df, emp_df)
minmax_peak_df$source <- factor(minmax_peak_df$source,
                                 levels=c('CHIRPS_min_peak', 'CHIRPS_max_peak', 'NAMEM'),
                                 labels=c('CHIRPS: min peak biomass', 'CHIRPS: max peak biomass', 'NAMEM'))
lines <- c('dashed', 'dotted', 'solid')  # c('dashed', 'longdash', 'solid')
p <- ggplot(minmax_peak_df, aes(x=date, y=total_biomass_gm2))
p <- p + geom_line(aes(linetype=source))
p <- p + scale_linetype_manual(values=lines)
p <- p + geom_point(data=emp_df, aes(x=date, y=total_biomass_gm2))
p <- p + facet_wrap(~name_en, scales='free')
p <- p + scale_x_date(date_breaks = "6 months", date_labels=("%m/%y"))
p <- p + ylab("Total biomass (g/m2)") + xlab("Date")
p <- p + theme(legend.position="bottom", legend.title = element_blank())
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/AGU_Dec_2017/variability_NAMEM_vs_CHIRPS_peak_20km_buf_dash_dot.png"
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

# test: predicted forage of NAMEM and nearest CHIRPS pixel
closest_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/CHIRPS/CHIRPS_pixel_soum_ctr_match.csv")
ch_res_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/chirps_prec/zero_sd"
nm_res_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/namem_clim_wc_temp/zero_sd"
df_list <- list()
i <- 1
for(soum in unique(closest_df$soum_site_id)){
  nm_df <- read.csv(paste(nm_res_dir, soum, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", soum, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", soum, sep=""), 'green_kgha', sep='_'))
  nm_df <- nm_df[, cols]
  colnames(nm_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  nm_df$total_biomass_kgha <- nm_df$dead_kgha + nm_df$green_kgha
  nm_df$source <- 'NAMEM'
  nm_df$soum <- soum
  df_list[[i]] <- nm_df
  i <- i + 1
  
  pixel_id <- closest_df[closest_df$soum_site_id == soum, 'pixel_site_id']
  ch_df <- read.csv(paste(ch_res_dir, pixel_id, 'summary_results.csv', sep='/'))
  cols <- c('month', 'year', paste(paste("X", pixel_id, sep=""), 'dead_kgha', sep='_'),
            paste(paste("X", pixel_id, sep=""), 'green_kgha', sep='_'))
  ch_df <- ch_df[, cols]
  colnames(ch_df) <- c('month', 'year', 'dead_kgha', 'green_kgha')
  ch_df$total_biomass_kgha <- ch_df$dead_kgha + ch_df$green_kgha
  ch_df$source <- 'CHIRPS'
  ch_df$soum <- soum
  df_list[[i]] <- ch_df
  i <- i + 1
}
matching_df <- do.call(rbind, df_list)
matching_df$date <- as.Date(paste(matching_df$year, matching_df$month, '01', sep="-"),
                               format="%Y-%m-%d")
match_df <- match_df[, c('site_id', 'name_en')]
matching_df <- merge(matching_df, match_df, by.x='soum', by.y='site_id')
p <- ggplot(matching_df, aes(x=date, y=total_biomass_kgha))
p <- p + geom_line(aes(colour=source))
p <- p + facet_wrap(~name_en, scales='free')
p <- p + scale_x_date(date_breaks = "6 months", date_labels=("%m/%y"))
p <- p + ylab("Total biomass (kg/ha)") + xlab("Date")
p <- p + theme(legend.position="bottom")
print(p)
pngname <- "C:/Users/Ginger/Desktop/Namem_wc_temp_vs_CHIRPS_nearest_pixel.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

# make map of CHIRPS pixel-based model results
ch_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/biomass_summary_zero_sd_chirps_GCD_G.csv")
aug_df <- ch_df[ch_df$month == 9, colnames(ch_df)[c(2:5, 9)]]
aug_wide <- reshape(aug_df, idvar='site_id', timevar='year', direction="wide")
aug_wide$total_forage_kgha_2016 <- aug_wide$total_biomass_gm2.2016 * 10
aug_wide$SFU_ha_2016 <- aug_wide$total_forage_kgha_2016 / 730
aug_wide$total_forage_kgha_2017 <- aug_wide$total_biomass_gm2.2017 * 10
aug_wide$SFU_ha_2017 <- aug_wide$total_forage_kgha_2017 / 730
aug_wide <- aug_wide[, colnames(aug_wide)[c(1:7, 9, 11)]]
write.csv(aug_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/biomass_August_2016_2017_zero_sd.csv",
          row.names=FALSE)

# compare results from NAMEM and CHIRPS
comp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
comp_df <- comp_df[comp_df$month == 9, ]  # August only
match_df <- match_df[, c('name_en', 'site_id')]
comp_df <- merge(comp_df, match_df)
p <- ggplot(comp_df, aes(x=name_en, y=total_biomass_gm2))
p <- p + geom_point(aes(shape=climate_source))
p <- p + facet_wrap(~year)
p <- p + xlab("Soum") + ylab("August biomass (g/m2)")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/figs/soum_ctr_August_biomass_2016_2017.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

# make csv to join to soum centers, to make map
match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/soum_ctr_match_table.csv")
comp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
comp_nm <- comp_df[comp_df$month == 9 &
                       comp_df$climate_source == 'namem_clim',
                     colnames(comp_df)[c(2:5, 9)]]
comp_wide <- reshape(comp_nm, idvar='site_id',
                     timevar='year',
                     direction="wide")
comp_wide <- merge(comp_wide, match_df, by='site_id')
comp_wide$total_forage_kgha_2016 <- comp_wide$total_biomass_gm2.2016 * 10
comp_wide$SFU_ha_2016 <- comp_wide$total_forage_kgha_2016 / 730
comp_wide$total_forage_kgha_2017 <- comp_wide$total_biomass_gm2.2017 * 10
comp_wide$SFU_ha_2017 <- comp_wide$total_forage_kgha_2017 / 730
comp_wide <- comp_wide[colnames(comp_wide)[c(1:7, 18, 20)]]
write.csv(comp_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_August_2016_2017_NAMEM.csv",
          row.names=FALSE)

comp_ch <- comp_df[comp_df$month == 9 &
                     comp_df$climate_source == 'chirps_prec',
                   colnames(comp_df)[c(2:5, 9)]]
comp_wide <- reshape(comp_ch, idvar='site_id',
                     timevar='year',
                     direction="wide")
comp_wide <- merge(comp_wide, match_df, by='site_id')
comp_wide$total_forage_kgha_2016 <- comp_wide$total_biomass_gm2.2016 * 10
comp_wide$SFU_ha_2016 <- comp_wide$total_forage_kgha_2016 / 730
comp_wide$total_forage_kgha_2017 <- comp_wide$total_biomass_gm2.2017 * 10
comp_wide$SFU_ha_2017 <- comp_wide$total_forage_kgha_2017 / 730
comp_wide <- comp_wide[colnames(comp_wide)[c(1:7, 18, 20)]]
write.csv(comp_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_August_2016_2017_CHIRPS.csv",
          row.names=FALSE)

# compare NAMEM and CHIRPS climate data at soum centers
match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/soum_ctr_match_table.csv")
namem_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_inputs/soum_centers/namem_clim"
chirps_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_inputs/soum_centers/chirps_prec"
figdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/source_comparisons"

widths <- c(6, 4, rep(7, 12))
df_list <- list()
for(r in 1:NROW(match_df)){
  chirps_wth <- paste(chirps_dir, "/", match_df[r, 'site_id'], ".wth", sep="")
  ch_df <- read.fwf(chirps_wth, widths=widths)
  ch_df <- ch_df[which(ch_df$V2 >= 2008), ]
  ch_df$source <- 'CHIRPS'
  nm_wth <- paste(namem_dir, "/", match_df[r, 'name_en'], ".wth", sep="")
  nm_df <- read.fwf(nm_wth, widths=widths)
  nm_df$source <- 'NAMEM'
  site_df <- rbind(ch_df, nm_df)
  site_df$site_id <- match_df[r, 'name_en']
  colnames(site_df) <- c('variable', 'Year', seq(1, 12), 'source', 'site_id')
  df_list[[r]] <- site_df
}
clim_df <- do.call(rbind, df_list)
clim_res <- reshape(clim_df, idvar=c('variable', 'Year', 'source', "site_id"),
                    varying=colnames(clim_df)[3:14], v.names='amount',
                    direction="long", timevar='month')
clim_res$date <- as.Date(paste(clim_res$Year, clim_res$month, '01', sep="-"),
                         format='%Y-%m-%d')
p <- ggplot(clim_res, aes(x=date, y=amount))
p <- p + geom_line(aes(linetype=source))
p <- p + facet_grid(variable~site_id, scales="free")
print(p)
pngname <- paste(figdir, "CHIRPS_vs_NAMEM.png", sep="/")
png(file=pngname, units="in", res=300, width=15, height=10)
print(p)
dev.off()

clim_sub <- clim_res[clim_res$variable == "prec  ", ]
clim_sub <- clim_sub[clim_sub$Year > 2015, ]
p <- ggplot(clim_sub, aes(x=date, y=amount))
p <- p + geom_line(aes(colour=source))
p <- p + facet_wrap(~site_id, scales="free",
                    nrow=2)
p <- p + ylab("Precipitation (cm)") + xlab("Month")
p <- p + theme(legend.position="bottom")
print(p)
pngname <- paste(figdir, "CHIRPS_vs_NAMEM_prec.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

ch_subs <- clim_res[clim_res$source == 'CHIRPS', ]
nm_subs <- clim_res[clim_res$source == 'NAMEM', ]
sub_res <- merge(ch_subs, nm_subs, by=c('date', "Year", 'month', 'variable', 'site_id'))
sub_res$CHIRPS_minus_NAMEM <- sub_res$amount.x - sub_res$amount.y
sub_prec <- sub_res[sub_res$variable == unique(sub_res$variable)[1], ]

p <- ggplot(sub_prec, aes(x=date, y=CHIRPS_minus_NAMEM))
p <- p + geom_line()
p <- p + facet_wrap(~site_id, nrow=1)  #, scales="free")
p <- p + ylab("CHIRPS - NAMEM (cm)")
print(p)
pngname <- paste(figdir, "CHIRPS_minus_NAMEM_prec.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# summarize temp data from NAMEM and CHIRPS/Worldclim
clim_res <- reshape(clim_df, idvar=c('variable', 'Year', 'source', "site_id"),
                    varying=colnames(clim_df)[3:14], v.names='amount',
                    direction="long", timevar='month')
tmin <- clim_res[clim_res$variable == unique(clim_res$variable)[2], ]
avg_tmin <- aggregate(amount~site_id+month+source, data=tmin, FUN=mean)
p <- ggplot(avg_tmin, aes(x=month, y=amount))
p <- p + geom_point(aes(shape=source))
p <- p + facet_wrap(~site_id) + ylab ("Average min temperature")
pngname <- paste(figdir, "Worldclim_vs_NAMEM_avg_tmin.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

tmax <- clim_res[clim_res$variable == unique(clim_res$variable)[3], ]
avg_tmax <- aggregate(amount~site_id+month+source, data=tmax, FUN=mean)
p <- ggplot(avg_tmax, aes(x=month, y=amount))
p <- p + geom_point(aes(shape=source))
p <- p + facet_wrap(~site_id) + ylab ("Average max temperature")
pngname <- paste(figdir, "Worldclim_vs_NAMEM_avg_tmax.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# empirical summary of biomass: SCP and CBM
emp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/biomass_SCP_CBM.csv")
source_id <- emp_df[, c('site_id', 'source')]
colnames(source_id) <- c('site_id', 'emp_source')

sim_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
# sim_df <- merge(sim_df, source_id, by='site_id')

wth_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_SCP_points_nearest_soum_ctr.csv")
wth_df <- wth_df[, c("site_id", "NEAR_DIST", "name_en")]
sim_df <- merge(sim_df, wth_df)

# mismatch (sim - empirical) by precip source, for SCP sites only
emp_df$year <- format(as.Date(emp_df$Date), '%Y')
emp_df <- emp_df[emp_df$source == 'SCP', c('site_id', 'biomass_g_m2', 'year')]
colnames(emp_df) <- c('site_id', 'empirical_biomass', 'year')
aug_df <- sim_df[sim_df$month == 9, ]
simemp_df <- merge(emp_df, aug_df, by=c('site_id', 'year'), all=FALSE)
simemp_df$sim_minus_emp_gm2 <- simemp_df$total_biomass_gm2 - simemp_df$empirical_biomass
p <- ggplot(simemp_df, aes(x=climate_source, y=sim_minus_emp_gm2))
p <- p + geom_boxplot()
p <- p + facet_wrap(~year)
print(p)
figdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/prec_source_comparison"
pngname <- paste(figdir, "sim-emp_chirps_namem_worldclim.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(simemp_df, aes(x=empirical_biomass, y=total_biomass_gm2))
p <- p + geom_point()
p <- p + facet_wrap(~climate_source, nrow=3, scales="free")
p <- p + xlab('empirical biomass') + ylab('simulated biomass')
print(p)
pngname <- paste(figdir, "sim_vs_emp_chirps_namem_worldclim.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=8)
print(p)
dev.off()

for(precip_source in unique(simemp_df$climate_source)){
  subs <- simemp_df[simemp_df$climate_source == precip_source, ]
  c <- cor.test(subs$total_biomass_gm2, subs$empirical_biomass,
                  method='pearson')
  print(precip_source)
  print(c)
}

# compare empirical livestock densities and predicted SFU capacity from CHIRPS
emp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/spatial_results/combined_with_empirical/Empirical_SU.csv")
SFU_2016 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/spatial_results/combined_with_empirical/CHIRPS_SFU_2016_by_empirical.csv")
SFU_2017 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/spatial_results/combined_with_empirical/CHIRPS_SFU_2017_by_empirical.csv")
SFU_df <- merge(emp_df, SFU_2016, by.x='FID_emp', by.y='FID_EMP')
SFU_df <- merge(SFU_df, SFU_2017, by.x='FID_emp', by.y='FID_EMP')
SFU_df$CHIRPS_SFU_avg_mean <- (SFU_df$CHIRPS_SFU_2016_mean + SFU_df$CHIRPS_SFU_2017_mean) / 2
SFU_df[SFU_df$SFU_per_ha == 0, 'SFU_per_ha'] <- NA

sum_df <- data.frame('CHIRPS_year'=numeric(12), 'quartile'=numeric(12),
                     'total_n_emp_hexagons'=numeric(12), 'n_overlapping_hexagons'=numeric(12))
SU_quants <- quantile(SFU_df[, 'SFU_per_ha'], num=4, na.rm=TRUE)
i <- 1
for(year in c(2016, 2017, 'avg')){
  pred <- paste('CHIRPS_SFU_', year, '_mean', sep="")
  SFU_quants <- quantile(SFU_df[, pred], num=4, na.rm=TRUE)
  for(q in 1:4){
    emp_subs <- SFU_df[(SFU_df$SFU_per_ha >= SU_quants[[q]] &
                        SFU_df$SFU_per_ha < SU_quants[[q+1]]), ]
    emp_subs <- emp_subs[!is.na(emp_subs$SFU_per_ha), ]  # hexagons falling within that percentile for SU
    sim_subs <- SFU_df[SFU_df[, pred] >= SFU_quants[[q]] &
                         SFU_df[, pred] < SFU_quants[[q+1]], ]
    sim_subs <- sim_subs[!is.na(sim_subs$SFU_per_ha), ]
    sim_emp_subs <- sim_subs[(sim_subs$SFU_per_ha >= SU_quants[[q]] &
                                sim_subs$SFU_per_ha < SU_quants[[q+1]]), ]  # hexagons overlapping
    total_polygons <- dim(emp_subs)[1]
    overlapping <- dim(sim_emp_subs)[1]
    sum_df[i, 'CHIRPS_year'] <- year
    sum_df[i, 'quartile'] <- q
    sum_df[i, 'total_n_emp_hexagons'] <- total_polygons
    sum_df[i, 'n_overlapping_hexagons'] <- overlapping
    i <- i + 1
  }
}

# process extended NAMEM climate data
raw_csv = "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/Jul 2017-Aug 2018 Precip and Temp Data.csv"
raw_df <- read.csv(raw_csv, stringsAsFactors=FALSE)
raw_df[raw_df$Station_name == "Bayan Ovoo", "Station_name"] <- "Bayan-Ovoo"

temp_df <- raw_df[, c("Station_name", "Year", "Month", "Day", "temp_C")]
min_temp <- aggregate(temp_df$temp_C~temp_df$Station_name + temp_df$Year + temp_df$Month,
                      FUN=min)
colnames(min_temp) <- c('station_name', 'year', 'month', 'tmin')
max_temp <- aggregate(temp_df$temp_C~temp_df$Station_name + temp_df$Year + temp_df$Month,
                      FUN=max)
colnames(max_temp) <- c('station_name', 'year', 'month', 'tmax')
temp_df <- merge(min_temp, max_temp)
temp_df[temp_df$station_name == "Bayan Ovoo", "station_name"] <- "Bayan-Ovoo"
write.csv(temp_df, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/temp_2017-2018.csv",
          row.names=FALSE)

precip_df <- raw_df[, c("Station_name", "Year", "Month", "Day", "precip_mm")]
sum_precip <- aggregate(precip_df$precip_mm~precip_df$Station_name + precip_df$Year + precip_df$Month,
                        FUN=sum)
colnames(sum_precip) <- c('station_name', 'year', 'month', 'precip_mm')
write.csv(sum_precip, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/precip_2017-2018.csv",
          row.names=FALSE)

## Modeled biomass vs empirical cover at WCS exclosure sites, NAMEM and CHIRPS
norm <- function(vec){
  normalized = (vec - min(vec)) / (max(vec) - min(vec))
  return(normalized)
}

figdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/WCS_exclosures/figs"
sim_summary <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/WCS_exclosures/simulated_biomass_summary.csv")
sim_summary <- sim_summary[sim_summary$month == 9, ]
emp_summary <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Chantsa/RangelandExcloData_WCS2018_V2_20190503OA_exclosure_data.csv")

emp_summary[emp_summary$Inside.Outside == 'Inside ', 'Inside.Outside'] <- 'Inside'
emp_summary$grass_forb_cover <- rowSums(emp_summary[, 12:15])

emp_cols <- c('Year', 'Exclosure.number', 'X100.1000', 'TOTAL.VEG.COVER', 'grass_forb_cover')
emp_summary <- emp_summary[emp_summary$Inside.Outside == 'Inside', emp_cols]
emp_agg <- aggregate(emp_summary$grass_forb_cover~emp_summary$Year + emp_summary$Exclosure.number,
                     FUN=mean)  # mean of 100 and 1000 levels within sampling date / site
colnames(emp_agg) <- c('Year', 'Exclosure.number', 'grass_forb_cover_mean')

p <- ggplot(emp_agg, aes(x=Exclosure.number, y=grass_forb_cover_mean))
p <- p + geom_point() + facet_wrap(~Year)
p <- p + xlab("Exclosure") + ylab("Herbaceous cover (%)")
pngname <- paste(figdir, "emp_mean_herbaceous_cover.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=2.5)
print(p)
dev.off()

p <- ggplot(sim_summary, aes(x=site_id, y=total_biomass_gm2))
p <- p + geom_point()
p <- p + facet_grid(climate_source~year)
p <- p + xlab("Exclosure") + ylab("Herbaceous biomass (g/m2)")
pngname <- paste(figdir, "sim_biomass_month9.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

# normalized biomass and cover to combine the two datasets
sim_summary$site_id <- gsub("F", "", sim_summary$site_id)
namem_df <- sim_summary[sim_summary$climate_source == 'namem', ]
namem_norm_df <- data.frame('year'=namem_df$year, 'source'=namem_df$climate_source,
                            'site_id'=namem_df$site_id, 'norm_value'=norm(namem_df$total_biomass_gm2))
chirps_df <- sim_summary[sim_summary$climate_source == 'chirps', ]
chirps_norm_df <- data.frame('year'=chirps_df$year, 'source'=chirps_df$climate_source,
                            'site_id'=chirps_df$site_id, 'norm_value'=norm(chirps_df$total_biomass_gm2))
emp_norm_df <- data.frame('year'=emp_agg$Year, 'source'=rep("empirical", length(emp_agg$Year)),
                          'site_id'=emp_agg$Exclosure.number, 'norm_value'=norm(emp_agg$grass_forb_cover_mean))
comb_norm_df <- do.call(rbind, list(chirps_norm_df, namem_norm_df, emp_norm_df))

# empirical rank calculated outside of R
normalized_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/WCS_exclosures/normalized.csv")
normalized_df$source <- factor(normalized_df$source, levels=c("empirical", "chirps", "namem"))

p <- ggplot(normalized_df, aes(x=site_id, y=norm_value))
p <- p + geom_point(aes(shape=source), size=3) +
                    scale_shape_manual(values=c(17, 24, 16))
# p <- p + facet_grid(source~year)
p <- p + facet_wrap(~year)
print(p)  # whoa that doesn't look good

p <- ggplot(normalized_df, aes(x=emp_rank, y=norm_value))
p <- p + geom_point()
p <- p + facet_grid(source~year)
p <- p + xlab("Exclosure rank") + ylab("Normalized biomass or cover")
print(p)
pngname <- paste(figdir, "norm_sim_emp_ranked.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

## Simulated biomass at all WCS monitoring sites, chirps prec, zero sd
sim_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec/zero_sd/biomass_summary_zero_sd_chirps_GCD_G.csv")
sim_df <- sim_df[sim_df$year > 2015, ]
max_green_biomass_per_year <- aggregate(sim_df$green_biomass_gm2~sim_df$year + sim_df$site_id,
                                  FUN=max)
colnames(max_green_biomass_per_year) <- c('year', 'site_id', 'green_biomass_gm2')
max_green_biomass_per_year$year <- as.factor(max_green_biomass_per_year$year)
max_total_biomass_per_year <- aggregate(sim_df$total_biomass_gm2~sim_df$year + sim_df$site_id,
                                  FUN=max)
colnames(max_total_biomass_per_year) <- c('year', 'site_id', 'total_biomass_gm2')
max_total_biomass_per_year$year <- as.factor(max_total_biomass_per_year$year)
p <- ggplot(max_total_biomass_per_year, aes(x=site_id, y=total_biomass_gm2))
p <- p + geom_point(aes(color=year))
print(p)
p <- ggplot(max_green_biomass_per_year, aes(x=site_id, y=green_biomass_gm2))
p <- p + geom_point(aes(color=year))
print(p)

figdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec/zero_sd"
p <- ggplot(max_green_biomass_per_year, aes(x=year, y=green_biomass_gm2))
p <- p + geom_boxplot()
p <- p + ylab("Maximum green biomass (gC m-2)")
print(p)
pngname <- paste(figdir, "Max_green_biomass_across_sites_per_year.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

p <- ggplot(max_total_biomass_per_year, aes(x=year, y=total_biomass_gm2))
p <- p + geom_boxplot()
p <- p + ylab("Maximum total biomass (gC m-2)")
print(p)
pngname <- paste(figdir, "Max_total_biomass_across_sites_per_year.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

summary(max_green_biomass_per_year[max_green_biomass_per_year$year == 2016, 'green_biomass_gm2'])
summary(max_green_biomass_per_year[max_green_biomass_per_year$year == 2017, 'green_biomass_gm2'])

### compare biomass calculated from NDVI via regression to modeled biomass
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

eo_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/CBM_SCP_sites_2016_2017_NDVI_herb_biomass_new.csv")
modeled_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/SCP_sites/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")

eo_df$ndvi_scaled <- eo_df$NDVI / 10000  # match the order of mangnitude of Lingling's regression analysis
eo_df$herb_biomass_from_NDVI <- 206.73 * eo_df$ndvi_scaled - 11.002  # Lingling's regression: linear, herbaceous biomass
eo_df[eo_df$Day < 15, 'match_month'] <- 7
eo_df[eo_df$Day >= 15, 'match_month'] <- 8

modeled_df <- modeled_df[modeled_df$climate_source == 'chirps_prec', ]
biomass_summary_df <- merge(modeled_df, eo_df, by.x=c('site_id', 'year', 'month'),
                            by.y=c('site_ID', 'Year', 'match_month'))
maximum_biomass <- max(c(biomass_summary_df$total_biomass_gm2, biomass_summary_df$herb_biomass_from_NDVI))
p <- ggplot(biomass_summary_df, aes(x=total_biomass_gm2, y=herb_biomass_from_NDVI))
p <- p + geom_point()
p <- p + xlim(0, maximum_biomass + 2)
p <- p + ylim(0, maximum_biomass + 2)
p <- p + geom_abline(slope=1, intercept=0, size=0.05)
p <- p + xlab("Modeled biomass (gC m-2)") + ylab("Observed biomass from NDVI (gC m-2)")
p <- p + print_theme
print(p)
figdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_vs_EO"
pngname <- paste(figdir, "obs_potential_biomass_SCP_sites.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

# calculate index of grazing pressure: difference between potential and observed biomass
biomass_summary_df$grazing_pressure <- biomass_summary_df$total_biomass_gm2 - biomass_summary_df$herb_biomass_from_NDVI
estimated_grazing_pressure <- biomass_summary_df[, c('site_id', 'year', 'Month', 'Day', 'grazing_pressure')]
write.csv(estimated_grazing_pressure, paste(figdir, "est_grazing_pressure_SCP_sites.csv", sep="/"), row.names=FALSE)

write.csv(biomass_summary_df, paste(figdir, "biomass_summary_NDVI_modeled_no_grazing.csv", sep="/"), row.names=FALSE)
