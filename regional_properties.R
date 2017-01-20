# summarize results of regional property simulations

library(ggplot2)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# summarize productivity of regional properties
zero_dens <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/zero_dens/Worldclim_precip/combined_summary.csv")
# calib <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/preset_dens/gain_summary.csv")
varying_cp <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/preset_dens_uncalibrated/gain_summary.csv")
constant_cp <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/preset_dens_uncalibrated_constant_cp/gain_summary.csv")

# compare primary productivity to individual livestock weight gain
zero_dens_sum <- aggregate(total_kgha~site, data=zero_dens, FUN=mean)
colnames(zero_dens_sum)[2] <- 'kgha_monthly_mean'
varying_cp_m <- varying_cp[, c('density', 'site', 'avg_yearly_gain')]
colnames(varying_cp_m)[3] <- 'perc_gain_varying_cp'
constant_cp_m <- constant_cp[, c('density', 'site', 'avg_yearly_gain')]
colnames(constant_cp_m)[3] <- 'perc_gain_constant_cp'

comb_df <- merge(constant_cp_m, varying_cp_m, by=c('site', 'density'))
comb_df <- merge(comb_df, zero_dens_sum, by='site')

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/summary_figs_analysis"
p <- ggplot(comb_df, aes(x=kgha_monthly_mean, y=perc_gain_constant_cp))
p <- p + geom_point()
p <- p + facet_wrap(~density, scales="free")
p <- p + xlab("Average monthly biomass (kg/ha)")
p <- p + ylab("Average yearly liveweight gain (kg)")
# print(p)
pngname <- paste(imgdir, "npp_vs_gain_constant_cp.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(comb_df, aes(x=kgha_monthly_mean, y=perc_gain_varying_cp))
p <- p + geom_point()
p <- p + facet_wrap(~density, scales="free")
p <- p + xlab("Average monthly biomass (kg/ha)")
p <- p + ylab("Average yearly liveweight gain (kg)")
# print(p)
pngname <- paste(imgdir, "npp_vs_gain_varying_cp.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

t1 <- cor.test(comb_df$perc_gain_calibrated, comb_df$perc_gain_uncalibrated,
               method="spearman")
t2 <- cor.test(comb_df$perc_gain_calibrated, comb_df$kgha_monthly_mean,
               method="spearman")
t3 <- cor.test(comb_df$kgha_monthly_mean, comb_df$perc_gain_uncalibrated,
               method="spearman")

# total herd-level weight gain instead of individual
zero_dens_sum <- aggregate(total_kgha~site, data=zero_dens, FUN=mean)
colnames(zero_dens_sum)[2] <- 'kgha_monthly_mean'
varying_cp_m <- varying_cp[, c('density', 'site', 'total_delta_weight_kg')]
colnames(varying_cp_m)[3] <- 'total_gain_varying_cp'
constant_cp_m <- constant_cp[, c('density', 'site', 'total_delta_weight_kg')]
colnames(constant_cp_m)[3] <- 'total_gain_constant_cp'

comb_df <- merge(constant_cp_m, varying_cp_m, by=c('site', 'density'))
comb_df <- merge(comb_df, zero_dens_sum, by='site')

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/summary_figs_analysis"
p <- ggplot(comb_df, aes(x=kgha_monthly_mean, y=total_gain_constant_cp))
p <- p + geom_point()
p <- p + facet_wrap(~density, scales="free")
p <- p + xlab("Average monthly biomass in absence of grazing (kg/ha)")
p <- p + ylab("Average yearly liveweight gain (kg)")
# print(p)
pngname <- paste(imgdir, "npp_vs_herd_gain_constant_cp.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(comb_df, aes(x=kgha_monthly_mean, y=total_gain_varying_cp))
p <- p + geom_point()
p <- p + facet_wrap(~density, scales="free")
p <- p + xlab("Average monthly biomass in absence of grazing (kg/ha)")
p <- p + ylab("Average yearly liveweight gain (kg)")
# print(p)
pngname <- paste(imgdir, "npp_vs_herd_gain_varying_cp.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# summarize match by back-calc management routine
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/forward_from_2014/back_calc_match_summary_2015.csv")

p <- ggplot(sum_df, aes(x=site, y=g_m2, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + geom_line(aes(group=site))
p <- p + facet_wrap(~year + live_or_total, scales="free")
p <- p + ylab('biomass (grams per square m)')
print(p)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis"
pngname <- paste(img_dir, "back_calc_match_summary_2015_constrained.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# add error bars for min-max empirical measurements
emp_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_PDM_summary.csv")
emp_min <- emp_summary[which(emp_summary$Year == 2015), c("FID", "min_gm2")]
emp_min$min_max <- 'min'
emp_max <- emp_summary[which(emp_summary$Year == 2015), c("FID", "max_gm2")]
emp_max$min_max <- 'max'
colnames(emp_min) <- c('site', 'g_m2', 'min_max')
colnames(emp_max) <- c('site', 'g_m2', 'min_max')
emp_reshape <- rbind(emp_min, emp_max)
emp_reshape$year <- 2015
emp_reshape$X <- NA
emp_reshape$live_or_total <- NA
emp_reshape$sim_vs_emp <- NA
emp_reshape <- emp_reshape[, colnames(sum_df)]
sum_df$min_max <- NA

sum_df <- rbind(sum_df, emp_reshape)

p <- ggplot(sum_df, aes(x=site, y=g_m2, group=sim_vs_emp))
p <- p + geom_line(data=emp_reshape, aes(x=site, y=g_m2, group=site))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + ylab('biomass (grams per square m)')
print(p)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis"
pngname <- paste(img_dir, "back_calc_match_summary_2015_min-max_empirical_modify_24_months.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# compare intensity in shared months: back-calc match 2015, and 2014
summary_2015 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_2015_total/2015_total_schedule_summary.csv")
summary_2014 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_2014_total/2014_total_schedule_summary.csv")
cols_to_keep <- c("date", "site", "total_rem")
summary_2015 <- summary_2015[, cols_to_keep]
colnames(summary_2015)[3] <- 'g_m2_removed_match_2015'
summary_2014 <- summary_2014[, cols_to_keep]
colnames(summary_2014)[3] <- 'g_m2_removed_match_2014'
means_2015 <- aggregate(g_m2_removed_match_2015~site, data=summary_2015, FUN=mean)
means_2014 <- aggregate(g_m2_removed_match_2014~site, data=summary_2014, FUN=mean)
site_means <- merge(means_2014, means_2015, by="site")

p <- ggplot(site_means, aes(x=g_m2_removed_match_2015, y=g_m2_removed_match_2014))
p <- p + geom_point()
p <- p + xlab('average monthly biomass removed (g / m2) matching 2015')
p <- p + ylab('average monthly biomass removed (g / m2) matching 2014')
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
print(p)

pngname <- paste(img_dir, "back_calc_intensity_shared_months_match_2015_vs_match_2014.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

# compare intensity in shared months: back-calc to match 2015,
# running forward from 2014 match (constrained) and not (unconstrained)
summary_2015 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_2015_total/2015_total_schedule_summary.csv")
summary_2015_con <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/forward_from_2014/back_calc_match 2015/2015_total_schedule_summary.csv")
summary_2014 <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_2014_total/2014_total_schedule_summary.csv")

cols_to_keep <- c("date", "site", "total_rem")
summary_2015 <- summary_2015[, cols_to_keep]
colnames(summary_2015)[3] <- 'g_m2_removed_match_2015_unc'
summary_2015_con <- summary_2015_con[, cols_to_keep]
colnames(summary_2015_con)[3] <- 'g_m2_removed_match_2015_con'
# identify dates following 2014 measurement
summary_2014 <- summary_2014[, cols_to_keep]
colnames(summary_2014)[3] <- 'g_m2_removed_match_2014'
id_date <- merge(summary_2014, summary_2015, by=c("site", "date"), all.y=TRUE)
summary_2015_unc <- id_date[which(is.na(id_date$g_m2_removed_match_2014)), colnames(summary_2015)]
combined_2015 <- merge(summary_2015_con, summary_2015_unc, by=c("site", "date"))
# plot each month separately
p <- ggplot(combined_2015, aes(x=g_m2_removed_match_2015_unc,
                               y=g_m2_removed_match_2015_con))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + xlab("g/m2 removed - unconstrained")
p <- p + ylab("g/m2 removed - constrained")
print(p)
pngname <- paste(img_dir, "back_calc_intensity_2015_constrained_vs_unconstrained.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

means_unc <- aggregate(g_m2_removed_match_2015_unc~site, data=combined_2015, FUN=mean)
means_con <- aggregate(g_m2_removed_match_2015_con~site, data=combined_2015, FUN=mean)
means_2015_con_unc <- merge(means_unc, means_con, by="site")
p <- ggplot(means_2015_con_unc, aes(x=g_m2_removed_match_2015_unc,
                                    y=g_m2_removed_match_2015_con))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + xlab("average g/m2 removed - unconstrained")
p <- p + ylab("average g/m2 removed - constrained")
print(p)
pngname <- paste(img_dir, "back_calc_intensity_2015_constrained_vs_unconstrained_average.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

# compare to back-calc intensity for 2015
bc_2015_unc <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_2015_total/2015_total_schedule_summary.csv")
bc_2015_con <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/forward_from_2014/back_calc_match 2015/2015_total_con_schedule_summary.csv")

# summarize back-calc intensity in last 6 and 12 months
nrows <- length(unique(bc_2015_con$site)) * 2
bc_summary_con <- data.frame('bc_source'=rep('constrained', nrows), 'time_period'=numeric(nrows),
                             'site'=numeric(nrows), 'average_gm2_removed'=numeric(nrows))
i <- 1
df <- bc_2015_con
for(per in c(6, 12)){
  for(s in unique(df$site)){
    sub <- df[which(df$site == s), ]
    sub <- sub[order(sub$time), ]
    select <- sub[(NROW(sub) - (per - 1)):NROW(sub), ]
    intensity <- mean(select[, 'total_rem'])
    bc_summary_con[i, 'time_period'] <- per
    bc_summary_con[i, 'site'] <- s
    bc_summary_con[i, 'average_gm2_removed'] <- intensity
    i <- i + 1
  }
}
bc_summary_unc <- data.frame('bc_source'=rep('unconstrained', nrows), 'time_period'=numeric(nrows),
                             'site'=numeric(nrows), 'average_gm2_removed'=numeric(nrows))
i <- 1
df <- bc_2015_unc
for(per in c(6, 12)){
  for(s in unique(df$site)){
    sub <- df[which(df$site == s), ]
    sub <- sub[order(sub$time), ]
    select <- sub[(NROW(sub) - (per - 1)):NROW(sub), ]
    intensity <- mean(select[, 'total_rem'])
    bc_summary_unc[i, 'time_period'] <- per
    bc_summary_unc[i, 'site'] <- s
    bc_summary_unc[i, 'average_gm2_removed'] <- intensity
    i <- i + 1
  }
}
bc_summary <- rbind(bc_summary_con, bc_summary_unc)
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis/match_2015_intensity_summary.csv"
write.csv(bc_summary, save_as, row.names=FALSE)

bc_summary <- read.csv(save_as)

# summarize intensity across 24 months matching both 2014 and 2015
bc_2015_con <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/forward_from_2014/back_calc_match 2015/2015_total_con_schedule_summary.csv")
intensity_df <- aggregate(total_rem~site, data=bc_2015_con, FUN=mean)
colnames(intensity_df)[2] <- 'average_monthly_gm2_removed'

# compare grazing intensity in back-calc history and reported cattle density
results_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis/comparison_with_reported_density"
est_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/Regional_properties_dung_analysis/reg_cattle_estimates_11.30.16.csv")
FID_list <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Property_FID_match.csv")
est_df$property_cattle <- (est_df$PropCattle0 + est_df$PropCattleT.6) / 2
est_df$non_property_cattle <- (est_df$NonPropCattle0 + est_df$NonPropCattleT.6) / 2
est_df$property_non_property_cattle <- est_df$property_cattle + est_df$non_property_cattle
est_df$prop_density <- est_df$property_cattle / est_df$LwfPropSizeHa
est_df$prop_non_density <- est_df$property_non_property_cattle / est_df$LwfPropSizeHa
est_df <- merge(est_df, FID_list, by.x="Property", by.y="NAME", all.x=TRUE)

# test taking out Makurian
est_df <- est_df[which(est_df$Property != "Makurian"), ]
nrows <- 2
cor_record <- data.frame('spearman_rho'=numeric(nrows), 'spearman_p_val'=numeric(nrows),
                         'pearson_cor'=numeric(nrows), 'pearson_p_val'=numeric(nrows),
                         stringsAsFactors=FALSE)
i <- 1
for(cattle_est in c("prop_density", "prop_non_density")){
  est_subs <- est_df[, c("FID", cattle_est)]
  colnames(est_subs)[2] <- 'cattle_density'
  est_merge <- merge(est_subs, intensity_df, by.x="FID", by.y="site")
  sp_test <- cor.test(~ cattle_density + average_monthly_gm2_removed, data=est_merge,
                   method="spearman")
  sp_rho = sp_test[[4]]
  sp_p_val = sp_test[[3]]
  pe_test <- cor.test(~ cattle_density + average_monthly_gm2_removed, data=est_merge,
                      method="pearson")
  pe_cor = pe_test[[4]]
  pe_p_val = pe_test[[3]]
  p <- ggplot(est_merge, aes(x=average_monthly_gm2_removed, y=cattle_density))
  p <- p + geom_point()
  p <- p + xlab("Back-calc intensity") + ylab("Estimated cattle density")
  pngname <- paste(results_dir,
                   paste(paste("back-calc_intensity_2014_2015_v", cattle_est, sep="_"),
                         '.png', sep=""), sep="/")
  # png(file=pngname, units="in", res=300, width=5, height=5)
  # print(p)
  # dev.off()
  cor_record[i, 'cattle_estimation_method'] <- cattle_est
  cor_record[i, 'spearman_rho'] <- sp_rho
  cor_record[i, 'spearman_p_val'] <- sp_p_val
  cor_record[i, 'pearson_cor'] <- pe_cor
  cor_record[i, 'pearson_p_val'] <- pe_p_val
  i <- i + 1
}
save_as <- paste(results_dir, "correlation_summary_NOMAKURIAN.csv", sep="/")
write.csv(cor_record, save_as)

# empirical biomass: regional properties
# PDM by property
count <- function(values){
  return(length(values))
}

pdm_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_PDM_combined.csv"
pdm_df <- read.csv(pdm_csv)
FID_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Property_FID_match.csv"
FID_list <- read.csv(FID_csv)

PDM_count <- aggregate(PDM~Year + Transect + Property, data=pdm_df, FUN=count)
trouble <- PDM_count[which(PDM_count$PDM != 11), ]

PDM_by_transect <- aggregate(PDM~Year + Transect + Property, data=pdm_df, FUN=mean)
PDM_by_transect$biomass_kgha <- PDM_by_transect$PDM * 332.35 + 15.857
PDM_by_transect$biomass_gm2 <- PDM_by_transect$biomass_kgha / 10
biomass_summary <- aggregate(biomass_gm2~Year + Property, data=PDM_by_transect, FUN=count)
colnames(biomass_summary)[3] <- 'n_transect'
mean_biomass <- aggregate(biomass_gm2~Year + Property, data=PDM_by_transect, FUN=mean)
min_biomass <- aggregate(biomass_gm2~Year + Property, data=PDM_by_transect, FUN=min)
max_biomass <- aggregate(biomass_gm2~Year + Property, data=PDM_by_transect, FUN=max)
stdev_biomass <- aggregate(biomass_gm2~Year + Property, data=PDM_by_transect, FUN=sd)
biomass_summary$mean_biomass_gm2 <- mean_biomass$biomass_gm2
biomass_summary$min_gm2 <- min_biomass$biomass_gm2
biomass_summary$max_gm2 <- max_biomass$biomass_gm2
biomass_summary$stdev_gm2 <- stdev_biomass$biomass_gm2
biomass_summary <- merge(biomass_summary, FID_list, by.x="Property",
                         by.y="NAME", all.x=TRUE)

date_range <- function(values){
  min_date <- min(values)
  max_date <- max(values)
  date_r <- max_date - min_date
  return(date_r)
}

pdm_df$Date <- as.Date(pdm_df$Date, format="%d-%b-%y")
# verify that each transect was collected in 1 day
test <- aggregate(Date~Year + Transect + Property, data=pdm_df, FUN=date_range)
date_summary <- aggregate(Date~Year + + Property, data=pdm_df, FUN=mean)
colnames(date_summary)[3] <- "average_date"
num_days <- aggregate(Date~Year + Property, data=pdm_df, FUN=date_range)

property_summary <- merge(biomass_summary, date_summary)
date_list <- seq(as.Date("2014/01/28"), as.Date("2015/12/28"), by="month")
property_summary$sim_date <- property_summary$average_date
for(i in (1:length(property_summary$Property))){
  property_summary[i, 'sim_date'] <- closest_date(property_summary[i, 'average_date'], date_list)
}
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_PDM_summary.csv"
write.csv(property_summary, save_as, row.names=FALSE)

# pin hits summarized by Felicia
hits_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/HitsSummary_Aug_10_2016.csv")
metadata <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/RS_vegetation_2014_2015_metadata.csv")

metadata$Date.sampled <- as.Date(metadata$Date.sampled, format="%m/%d/%Y")
sample_dates <- aggregate(Date.sampled~Property+Year, data=metadata, FUN=mean.Date)

biomass_df <- hits_df[, c("Property", "Year", "GBiomass", "BBiomass", "TotalBiomass")]
biomass_by_date <- merge(sample_dates, biomass_df, by=c("Property", "Year"))
biomass_by_date$kgha <- biomass_by_date$TotalBiomass * 10

p <- ggplot(biomass_by_date, aes(x=Date.sampled, y=kgha))
p <- p + geom_point()
print(p)

# compare empirical and simulated biomass: regional properties
sim_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/zero_dens/Worldclim_precip/combined_summary.csv")
sim_df$date <- paste(sim_df$month, "28", sim_df$year, sep="-")
sim_df$date <- as.Date(sim_df$date, format="%m-%d-%Y")

# find closest date of sampling
closest_date <- function(match_date, date_list){
  diff_list <- abs(sapply(date_list, difftime, time2=match_date))
  closest_indx <- which.min(diff_list)
  closest_date <- date_list[closest_indx]
  return(closest_date)
}

# convert factors to character
i <- sapply(biomass_by_date, is.factor)
biomass_by_date[i] <- lapply(biomass_by_date[i], as.character)
i <- sapply(sim_df, is.factor)
sim_df[i] <- lapply(sim_df[i], as.character)

# prepare df for plotting
plot_df1 <- biomass_by_date[, c("Property", "Date.sampled", "kgha", "Year")]
plot_df1$sim_vs_emp <- 'empirical'
nrows <- NROW(biomass_by_date)
plot_df2 <- plot_df1
plot_df2$sim_vs_emp <- "simulated"
for(r in seq(1, nrows)){
  prop <- plot_df1[r, "Property"]
  date_list <- sim_df[which(sim_df$Property == prop), 'date']
  match_date <- plot_df1[r, "Date.sampled"]
  sim_date <- closest_date(match_date, date_list)
  plot_df2[r, 'Property'] <- prop
  plot_df2[r, 'Date.sampled'] <- sim_date
  plot_df2[r, 'kgha'] <- sim_df[which(sim_df$date == sim_date &
                                          sim_df$Property == prop),
                                  'total_kgha']
}
combined_df <- rbind(plot_df1, plot_df2)
combined_df$id <- paste(combined_df$Property, combined_df$Year, sep="-")

# make df for back-calc management
site_df <- biomass_by_date[, c("Property", "Date.sampled", "GBiomass",
                               "TotalBiomass", "Year")]
site_df$FID <- 'NA'
nrows <- NROW(biomass_by_date)
for(r in seq(1, nrows)){
  prop <- site_df[r, "Property"]
  date_list <- sim_df[which(sim_df$Property == prop), 'date']
  match_date <- site_df[r, "Date.sampled"]
  sim_date <- closest_date(match_date, date_list)
  site_df[r, 'sim_date'] <- as.character.Date(sim_date)
  site_df[r, 'FID'] <- unique(sim_df[which(sim_df$Property == prop), 'site'])
}
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/regional_veg_match_file.csv"
write.csv(site_df, save_as, row.names=FALSE)
 
# plot sim vs emp by date
p <- ggplot(combined_df, aes(x=Date.sampled, y=kgha, group=id))
p <- p + geom_point(aes(colour=sim_vs_emp), size=5)
p <- p + geom_line()
p <- p + facet_wrap(~Year, scales="free")
print(p)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/zero_dens/Worldclim_precip/results_analysis"
pngname <- paste(img_dir, "empirical_vs_sim_by_date-zero_dens.png", sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

p <- ggplot(combined_df, aes(x=sim_vs_emp, y=kgha, group=id))
p <- p + geom_point(aes(colour=sim_vs_emp), size=5)
p <- p + geom_line() + xlab("") + ylab("Biomass (kg/ha)")
p <- p + facet_wrap(~Year, scales="free")
p <- p + theme(legend.position="none")
print(p)

pngname <- paste(img_dir, "empirical_vs_sim-zero_dens.png", sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

sim_cor <- combined_df[which(combined_df$sim_vs_emp == 'simulated'),
                       c('id', 'kgha', 'Year')]
colnames(sim_cor)[2] <- 'simulated_biomass'
emp_cor <- combined_df[which(combined_df$sim_vs_emp == 'empirical'),
                       c('id', 'kgha', 'Year')]
colnames(emp_cor)[2] <- 'empirical_biomass'
cor_df <- merge(sim_cor, emp_cor, by=c('id', 'Year'))

p <- ggplot(cor_df, aes(x=empirical_biomass, y=simulated_biomass))
p <- p + geom_point()
print(p)
pngname <- paste(img_dir, "empirical_vs_sim-zero_dens_scatterplot.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

cor_test <- cor.test(cor_df$simulated_biomass, cor_df$empirical_biomass, method="spearman")

# sim only by date
sub_sim <- sim_df[which((sim_df$date > as.Date("2014-06-27") &
                          sim_df$date < as.Date("2014-08-29")) |
                     (sim_df$date > as.Date("2015-06-27") &
                        sim_df$date < as.Date("2015-07-29"))), ]
sub_sim <- sub_sim[which(sub_sim$Property %in% combined_df$Property), ]
p <- ggplot(sub_sim, aes(x=date, y=total_kgha, group=Property))
p <- p + geom_line() + facet_wrap(~year, scales="free")
p <- p + geom_point()
print(p)
pngname <- paste(img_dir, "sim-zero_dens.png", sep="/")
png(file=pngname, units="in", res=300, width=8.9, height=5)
print(p)
dev.off()

# sim vs empirical by property
p <- ggplot(combined_df, aes(x=Property, y=kgha))
p <- p + geom_point(aes(colour=sim_vs_emp), size=5)
p <- p + geom_line()
p <- p + facet_wrap(~Year, scales="free")
print(p)
pngname <- paste(img_dir, "empirical_vs_sim-zero_dens_by_property.png", sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

# how much does biomass in absence of grazing differ btw 6 weather stations?
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/zero_dens/Worldclim_precip/combined_summary.csv")
img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/zero_dens/Worldclim_precip"

sum_df$site <- as.factor(sum_df$site)

p <- ggplot(sum_df, aes(x=site, y=total_kgha))
p <- p + geom_boxplot()
print(p)

means_by_site <- aggregate(total_kgha~site, data=sum_df, FUN=mean)
save_as <- paste(img_dir, "mean_total_biomass_by_site.csv", sep="/")
write.table(means_by_site, save_as, sep=",", row.names=FALSE)

# how much does precip differ between FEWS RFE, Worldclim, and OPC weather stations?
lines <- c("solid", "longdash", "dotted")
prec_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/prec_source_summary_OPC.csv")

p <- ggplot(prec_summary, aes(x=date, y=prec_cm, group=Source))
p <- p + geom_point(aes(colour=Source))
p <- p + geom_line(aes(linetype=Source))
p <- p + scale_linetype_manual(values=lines)
p <- p + print_theme
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/FEWS_Worldclim_OPC.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()
