# examine Mongolia simulations

library(ggplot2)

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
ch_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/biomass_summary_zero_sd_chirps_GCD_G.csv")
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
