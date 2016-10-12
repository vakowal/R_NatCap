# various summary plots: vegetation at Ol Pejeta Conservancy
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# integrated test: empirical stocking density simulations compared to empirical biomass measurements
x10_comp_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison_x10_OPC_veg_9.30.16_by_weather.csv"
x10df <- read.csv(x10_comp_csv)
x10df <- x10df[which(x10df$sim_vs_emp == 'sim'), ]
x10df$multiplier <- '10'

grz_months <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/grazing_months.csv")
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison_OPC_veg_9.30.16_by_weather.csv"
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_by_weather.csv"
comp_df <- read.csv(comparison_csv)
veg_df <- read.csv(veg_csv)
veg_df <- veg_df[which(veg_df$year == 15), ]

p <- ggplot(veg_df, aes(x=month, y=mean_biomass_kgha))
p <- p + geom_ribbon(aes(ymin=min_biomass_kgha, ymax=max_biomass_kgha), alpha=0.2)
p <- p + facet_wrap(~site, scales='free_x')
p <- p + geom_point(aes(x=month, y=biomass_kg_ha, colour=sim_vs_emp),
                    data=comp_df)
p <- p + geom_line(aes(x=month, y=biomass_kg_ha, colour=sim_vs_emp),
                   data=comp_df)
p <- p + geom_point(aes(x=month, y=mean_biomass_kgha), data=grz_months)
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

comp_df = comp_df[which(comp_df$sim_vs_emp == 'sim'), ]
comp_df$multiplier <- '1'
comp_df = rbind(comp_df, x10df)
p <- ggplot(comp_df, aes(x=month, y=biomass_kg_ha))
p <- p + geom_point(aes(x=month, y=biomass_kg_ha, colour=multiplier))
p <- p + geom_line(aes(colour=multiplier))
p <- p + geom_point(aes(x=month, y=mean_biomass_kgha), data=grz_months)
p <- p + facet_wrap(~site, scales='free_x')
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/empirical_density_vs_10x_empirical_density.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# regional veg summary
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/HitsSummary_Aug_10_2016.csv"
veg_df <- read.csv(veg_csv)

veg_df[is.na(veg_df)] <- 0

spp_list <- unique(sapply(colnames(veg_df)[22:29], substr, start=5, stop=6))
prop_g_list <- paste("Prop", spp_list, "G", sep="")
prop_b_list <- paste("Prop", spp_list, "B", sep="")

for(i in c(1:length(spp_list))){
  veg_df[, (paste("prop_total_", spp_list[i], sep=""))] <- 
    veg_df[, prop_g_list[i]] + veg_df[, prop_b_list[i]]
}

fig_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/regional_veg_composition_plots"
for(spp in spp_list){
  col <- paste("prop_total_", spp, sep="")
  p <- ggplot(veg_df, aes_string(x="X", y=col))
  p <- p + geom_point() + ggtitle(spp) + xlab("Property index")
  filename <- paste(fig_dir, paste("prop_", spp, "_by_property.png", sep=""), sep="/")
  png(file=filename, units="in", res=300, width=7, height=5)
  print(p)
  dev.off()
}

# back-calculated management of Jenny's sites (33 sites, 9.14.16)
summary_csv = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/comparison_summary.csv"
sum_df = read.csv(summary_csv)

failed <- c('N4', 'M10', 'W06', 'GO', 'LO3', 'LO4')

p <- ggplot(sum_df, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp)) + geom_line(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=10, scales='free')
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/summary.png"
png(file=pngname, units="in", res=300, width=18, height=8)
print(p)
dev.off()

# linear interpolation
df_list = list()
diff_df_list = list()
i <- 1
for(site in unique(sum_df$site)){
  subs <- sum_df[which(sum_df$site == site), ]
  sim <- subs[which(subs$sim_vs_emp == 'simulated'), ]
  emp <- subs[which(subs$sim_vs_emp == 'empirical'), ]
  empinterp <- approx(x=emp$date, y=emp$biomass,
                      xout=seq(min(sim$date), max(sim$date), length.out=12))
  siminterp <- approx(x=sim$date, y=sim$biomass,
                      xout=seq(min(sim$date), max(sim$date), length.out=12))
  sumdiff <- sum(abs(siminterp$y - empinterp$y))
  interpdf <- rbind(data.frame('date'=siminterp$x, 'biomass'=siminterp$y,
                               'sim_vs_emp'=rep('simulated', 12),
                               'site'=rep(site, 12)),
                    data.frame('date'=empinterp$x, 'biomass'=empinterp$y,
                               'sim_vs_emp'=rep('empirical', 12),
                               'site'=rep(site, 12)))
  diff_df_list[[i]] <- data.frame('site'=site, 'sum_diff'=sumdiff)
  df_list[[i]] <- interpdf
  i <- i + 1
}
interpdf <- do.call(rbind, df_list)
diff_df <- do.call(rbind, diff_df_list)

p <- ggplot(interpdf, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=10, scales='free_x')
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/interpolated_points_all.png"
png(file=pngname, units="in", res=300, width=18, height=8)
print(p)
dev.off()

succeeded <- setdiff(interpdf$site, failed)
succ_interp <- interpdf[which(interpdf$site %in% succeeded), ]
p <- ggplot(succ_interp, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + geom_line(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=4, scales='free_x')
p <- p + ylab("Biomass (g/m2)")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/interpolated_points_succeeded.png"
png(file=pngname, units="in", res=300, width=10, height=12)
print(p)
dev.off()

diff_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/sum_diff.csv"
write.csv(diff_df, diff_csv)

# joined sum_diff to site summary csv manually
site_summary_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/jenny_site_summary_open.csv"
site_summary_df = read.csv(site_summary_csv)

p <- ggplot(site_summary_df, aes(x=weather_distance_m, y=sum_weekly_diff))
p <- p + geom_point() + xlab("distance to nearest weather station (m)")
p <- p + ylab("sum(abs(simulated - empirical))")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/distance_to_weather_vs_diff.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

succeeded <- setdiff(site_summary_df$site, failed)
succ_subs <- site_summary_df[which(site_summary_df$site %in% succeeded), ]
p <- ggplot(succ_subs, aes(x=closest_weather, y=sum_weekly_diff))
p <- p + geom_point()
p <- p + xlab("Weather station")
p <- p + ylab("sum(abs(simulated - empirical))")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/weather_stn_vs_diff.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

## OPC veg analysis
coeff_var <- function(values){
  cv <- sd(values) / mean(values) * 100
  return(cv)
}

count <- function(values){
  return(length(values))
}

# analysis of veg data 9.30.16
metadata_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_metadata.csv"
PDM_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_PDM.csv"

meta_df <- read.csv(metadata_csv)
veg_df <- read.csv(PDM_csv)

# look for issues matching up PDM records with site lat/long
veg_ids <- unique(veg_df[, c("Date", "Site")])
veg_ids <- paste(veg_ids$Date, veg_ids$Site, sep="-")
meta_ids <- unique(meta_df[ , c("Date", "Site")])
meta_ids <- paste(meta_ids$Date, meta_ids$Site, sep="-")

mismatch <- c()
for (id in veg_ids){
  if (!is.element(id, meta_ids)){
    mismatch <- c(mismatch, id)
  }
}
in_veg_not_meta <- mismatch

mismatch <- c()
for (id in meta_ids){
  if (!is.element(id, veg_ids)){
    mismatch <- c(mismatch, id)
  }
}
in_meta_not_veg <- mismatch

PDM_count <- aggregate(PDM~Date + Site, data=veg_df, FUN=count)
trouble <- PDM_count[which(PDM_count$PDM != 11), ]

# calculate average PDM and sum of tree and shrub counts
# within transects
PDM_avg <- aggregate(PDM~Date + Site, data=veg_df, FUN=mean)
tree_sum <- aggregate(Tree~Date + Site, data=veg_df, FUN=sum)
shrub_sum <- aggregate(Shrub~Date + Site, data=veg_df, FUN=sum)
veg_summarized <- PDM_avg
veg_summarized$biomass_kgha <- veg_summarized$PDM * 332.35 + 15.857
veg_summarized$tree_sum <- tree_sum$Tree
veg_summarized$shrub_sum <- shrub_sum$Shrub
veg_summarized$month = rep("NA", NROW(veg_summarized))
for (r in (1:NROW(veg_summarized))){
  date_list <- unlist(strsplit(as.character(
    veg_summarized[r, "Date"]), split="-"))
  veg_summarized[r, 'month_year'] <- paste(date_list[2], date_list[3], sep="_")
}

meta_df$id <- paste(meta_df$Date, meta_df$Site, sep="_")
veg_summarized$id <- paste(veg_summarized$Date, veg_summarized$Site, sep="_")

# summarize biomass by month
site_list <- c('Loirugurugu', 'Loidien', 'Research', 'Kamok', 'Rongai', 'Serat')
nrows <- length(site_list)
# diagnostic_df <- data.frame('total_measurements'=numeric(nrows),
  #                          'restricted_by_shrubs_trees'=numeric(nrows),
   #                         'site'=character(nrows), stringsAsFactors=FALSE)
df_list <- list()
i <- 1
for(site in site_list){
  meta_sub <- meta_df[which(meta_df$weather_2km == site), ]
  veg_sub <- veg_summarized[which(veg_summarized$id %in% meta_sub$id), ]
  total <- NROW(veg_sub)
  veg_sub <- veg_sub[which(veg_sub$tree_sum <= 12), ]
  veg_sub <- veg_sub[which(veg_sub$shrub_sum <= 14), ]
  # restr <- NROW(veg_sub)
  # diagnostic_df[i, 'total_measurements'] <- total
  # diagnostic_df[i, 'restricted_by_shrubs_trees'] <- restr
  # diagnostic_df[i, 'site'] <- as.character(site)
  mean_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=mean)
  min_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=min)
  max_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=max)
  n_meas <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=count)
  cv <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=coeff_var)
  sum_df <- cbind(mean_biomass, min_biomass$biomass_kgha, max_biomass$biomass_kgha,
                  n_meas$biomass_kgha, cv$biomass_kgha)
  colnames(sum_df) <- c('month_year', 'mean_biomass_kgha', 'min_biomass_kgha',
                        'max_biomass_kgha', 'num_measurements', 'cv')
  sum_df$site <- rep(site, NROW(sum_df))
  df_list[[site]] <- sum_df
  i <- i + 1
}
summary_df <- do.call(rbind, df_list)

diagnostic_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_sample_size.csv"
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_by_weather.csv"

write.csv(diagnostic_df, file=diagnostic_csv, row.names=FALSE)
write.csv(summary_df, file=veg_csv, row.names=FALSE)

# analysis of veg data 11.25.15
outdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger"
file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/OPC_veg_data_11.25.15.csv"

data <- read.csv(file)
# restrict empirical biomass measurements to sites with few trees and shrubs
hist(data$Tree, breaks=50)
hist(data$Shrub, breaks=50)
treeperc <- quantile(data$Tree, probs=seq(0, 1, by=0.1))
shrubperc <- quantile(data$Shrub, probs=seq(0, 1, by=0.1))

df_list <- list()
data$date <- paste(data$Year, "-", data$Month, sep="")
for(site in c('Loirugurugu', 'Loidien', 'Research', 'Kamok', 'Rongai', 'Serat')){
  sub <- data[which(data$X2km_weat == site), ]
  sub <- sub[which(sub$Tree <=6), ]
  sub <- sub[which(sub$Shrub <= 8), ]
  summary <- aggregate(sub$Biomass, by=list(sub$date), FUN='mean')
  summary_2 <- aggregate(sub$Biomass, by=list(sub$date), FUN=length)
  summary_3 <- aggregate(sub$Biomass, by=list(sub$date), FUN=coeff_var)
  summary <- merge(summary, summary_2, by='Group.1')
  summary <- merge(summary, summary_3, by='Group.1')
  colnames(summary) <- c("date", "biomass (kg/ha)", "num_obs", "cv")
  summary$site <- rep(site, NROW(summary))
  df_list[[site]] <- summary
}
sum_df <- do.call(rbind, df_list)
outfile <- paste(outdir, 'summary_9.29.16.csv', sep="/")
write.csv(sum_df, outfile, row.names=FALSE)

locations <- unique(data[c("Lat", "Long")])
areas <- unique(data[c('Area')])
months <- unique(data["date"])

############## older analyses
Jenny_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Jenny_biomass_reshaped.txt"
Sharon_file = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Vegetation data_July-August_2014_biomass.txt"

cp_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/CP_combined.txt"

## Jenny's crude protein data
cp_dat <- read.table(cp_file, header = TRUE, sep = "\t")
cp_dat$Year <- factor(cp_dat$Year)
cp_dat$group <- interaction(cp_dat$Year, cp_dat$site)
cp_dat <- cp_dat[order(cp_dat$group), ]
cv_df <- aggregate(cp_dat$mj_per_kg_dm, by = list(cp_dat$Year), FUN = coeff_var)

## Jenny's data
veg_data <- read.table(Jenny_file, header = TRUE, sep = "\t")
veg_data$Transect <- as.factor(veg_data$site)
veg_data$caged <- as.factor(veg_data$caged)
veg_data$habitat <- as.factor(veg_data$habitat)

cattle_categorized <- veg_data[which(!is.na(veg_data$cattle)), ]
cattle_categorized <- cattle_categorized[cattle_categorized$caged == 'caged', ]
cattle_categorized$cattle <- factor(cattle_categorized$cattle, levels = c('none', 'low', 'medium', 'medium/high', 'high'))
cattle_categorized$site <- factor(cattle_categorized$site, levels = unique(cattle_categorized[order(cattle_categorized$cattle), 'site']))

p <- ggplot(cattle_categorized, aes(x = site, y = DM_g_per_sq_m, colour = cattle, order = cattle)) + geom_boxplot()
print(p)

sites <- c('GT', 'M05', 'MO', 'N4', 'W3', 'W06')
sites_veg <- veg_data[veg_data$site %in% sites, ]

p <- ggplot(sites_veg, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot()
p <- p + facet_wrap(~ caged, nrow = 2, ncol = 1)
p <- p + xlab('Site') + ylab('Dry matter (g per sq m)')
print(p)

uncaged_data <- sites_veg[sites_veg$caged == 'uncaged', ]

# means by site
aggdata <-aggregate(uncaged_data$DM_g_per_sq_m, by = list(uncaged_data$site), FUN=mean, na.rm=TRUE)

p <- ggplot(caged_data, aes(x = week, y = DM_g_per_sq_m)) + geom_line()
p <- p + facet_wrap(~ Transect, nrow = length(sites), ncol = 1) + print_theme
p <- p + scale_y_continuous(limits=c(50, 500))
p <- p + xlab('Sampling week') + ylab('Dry matter (g per sq m)')
p <- p + scale_x_continuous(breaks=c(0, 3, 6, 9)) + ggtitle("Empirical biomass: Jenny's caged sites")
print(p)

bplot <- ggplot(caged_data, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot() + print_theme
bplot <- bplot + scale_y_continuous(limits=c(50, 500))
bplot <- bplot + xlab('Site') + ylab('Dry matter (g per sq m)')
bplot <- bplot + ggtitle("Empirical biomass: Jenny's caged sites")
print(bplot)

pngname <- paste(outdir, "Jenny_caged_biomass.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(p)
dev.off()

pngname <- paste(outdir, "Jenny_caged_biomass_boxplot.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 4)
print(bplot)
dev.off()

## Sharon's data
veg_data$Transect <- as.factor(veg_data$Transect)

model <- aov(DM_g_per_sq_m ~ Transect, data = veg_data)
tHSD <- TukeyHSD(model)

veg_data$label[veg_data$Transect == '1'] <- 'a,b'
veg_data$label[veg_data$Transect == '2'] <- 'a,b'
veg_data$label[veg_data$Transect == '3'] <- 'a,b'
veg_data$label[veg_data$Transect == '4'] <- 'a'
veg_data$label[veg_data$Transect == '5'] <- 'a,b'
veg_data$label[veg_data$Transect == '6'] <- 'b'

p <- ggplot(veg_data, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot()
p <- p + xlab('Transect') + ylab('Dry matter (g per sq m)')
print(p)
