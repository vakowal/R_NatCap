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

# summarize match by back-calc management routine
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_match_summary.csv")

p <- ggplot(sum_df, aes(x=site, y=g_m2, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + geom_line(aes(group=site))
p <- p + facet_wrap(~year + live_or_total, scales="free")
p <- p + ylab('biomass (grams per square m)')
print(p)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/back_calc_results_analysis"
pngname <- paste(img_dir, "back_calc_match_summary.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# compare grazing intensity in back-calc history and reported cattle density
back_calc_intensity <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/summary_figs_analysis/back_calc_intensity_summary.csv")
emp_intensity <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Brian/est_cattle_density_10_Jul_2016.csv")
intensity_df <- merge(back_calc_intensity, emp_intensity, by="FID")

p <- ggplot(intensity_df, aes(x=CattleDensity, y=total_rem_2014_total))
p <- p + geom_point() + xlab("Estimated density (cattle / ha)")
p <- p + xlim(c(0, 0.7))
p <- p + ylab("Simulated average monthly biomass removed (g/m2)")
print(p)
pngname <- paste(imgdir, 'back-calc_intensity_vs_empirical_density.png', sep="/")
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

intensity_cor <- cor.test(intensity_df$CattleDensity,
                          intensity_df$total_rem_2014_total, method="spearman")

# empirical biomass: regional properties
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
