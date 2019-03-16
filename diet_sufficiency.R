# plot regional productivity energy balance for Kenya MS
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# multiple densities, multiple conception steps
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/energy_balance_summary.csv")
# ensure that management threshold works as I expect it to
min_biomass <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/min_biomass_summary.csv")
p <- ggplot(min_biomass, aes(x=site, y=min_biomass))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
print(p)

# aggregated metric: # months during the simulation where diet was insufficient
# to meet needs of the herd
sum_df <- sum_df[sum_df$step >= 0,
                 c('conception_step', 'density', 'site', 'herd_avg_diet_sufficiency')]
sufficiency_threshold <- 0  # threshold under which we consider the animals' dietary needs not met
fun <- function(diet_sufficiency_values, sufficiency_threshold){
  count_insufficient <- length(diet_sufficiency_values[diet_sufficiency_values < sufficiency_threshold])
  return(count_insufficient)
}
num_months_insufficient <- aggregate(herd_avg_diet_sufficiency ~ conception_step + density + site,
                                     data=sum_df, FUN=fun, sufficiency_threshold=sufficiency_threshold)
colnames(num_months_insufficient)[4] <- 'num_months_insufficient'
p <- ggplot(num_months_insufficient, aes(x=site, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
p <- p + ylab("# months with insufficient diet")
print(p)

# how sensitive is # months insufficient to timing of conception month?
subs <- num_months_insufficient[num_months_insufficient$density > 0.4, ]
p <- ggplot(subs, aes(x=site, y=num_months_insufficient))
p <- p + geom_jitter(width=0.2, height=0)
p <- p + facet_wrap(~density)
p <- p + ylab("# months with insufficient diet")
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_density.png"
png(file=pngname, units="in", res=300, width=8, height=3)
print(p)
dev.off()

# diet insufficiency vs rainfall
precip_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/regional_average_annual_precip_centroid.csv")
precip_suff <- merge(subs, precip_df, by.x="site", by.y="FID")
precip_suff$rainfall_cm <- precip_suff$avg_annual_rainfall_mm / 10
precip_suff <- precip_suff[precip_suff$conception_step == -4, ]
p <- ggplot(precip_suff, aes(x=rainfall_cm, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_wrap(~density)
p <- p + ylab("Number months with insufficient diet")
p <- p + xlab("Annual precipitation (cm)")
p <- p + print_theme
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_rainfall.png"
png(file=pngname, units="in", res=300, width=6, height=2.5)
print(p)
dev.off()

precip_suff <- merge(subs, precip_df, by.x="site", by.y="FID")
precip_suff$rainfall_cm <- precip_suff$avg_annual_rainfall_mm / 10
p <- ggplot(precip_suff, aes(x=rainfall_cm, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
p <- p + ylab("Number months with insufficient diet")
p <- p + xlab("Annual precipitation (cm)")
p <- p + print_theme
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_rainfall_conception_month.png"
png(file=pngname, units="in", res=300, width=6, height=8)
print(p)
dev.off()

# throwaway: remove botched rows from summary df
combined_no_string <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_summary_2018-10-01--13_22_24.csv")
combined_no_string <- rbind(sum_df, sum_df2, sum_df3, sum_df4, sum_df5)
combined_no_string$density_cor <- as.character(combined_no_string$density)
combined_no_string$density_valid <- as.numeric(combined_no_string$density_cor)
combined_valid <- combined_no_string[!is.na(combined_no_string$density_valid), 
                                     colnames(sum_df)]
nodups <- combined_valid[!duplicated(combined_valid), ]
write.csv(nodups, "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_summary_valid.csv")

## max viable density with precip perturb
library(ggplot2)
# sum_df <- remove invalid rows, as above
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_summary_2018-10-02--23_19_12.csv")
# how many site / precip scenario combinations should there be? 360
change_perc_series <- c(-0.8, -0.4, 0, 0.4, 0.8, 1.2)
site_list <- c(0, 1, 2, 6, 10, 12, 13, 27, 21, 24)
sum_df <- sum_df[sum_df$site %in% site_list,
                 colnames(sum_df)[c(1, 3:6)]]
total_sample_size <- length(change_perc_series) * length(change_perc_series) * length(site_list)
failed_list <- list()
for (mean_change_perc in change_perc_series){
  for (pci_change_perc in change_perc_series){
    for (site in site_list){
      subs <- sum_df[(sum_df$PCI_perc_change == pci_change_perc) &
                       (sum_df$total_precip_perc_change == mean_change_perc) &
                       (sum_df$site == site), ]
      if (!(1 %in% subs$num_months_insufficient) &
          !(2 %in% subs$num_months_insufficient)){
        id <- paste(site, mean_change_perc, pci_change_perc, sep="-")
        failed_list[[id]] <- subs
      }
    }
  }
}
failed_df <- do.call(rbind, failed_list)
failed_df <- failed_df[order(failed_df$site, failed_df$total_precip_perc_change,
                             failed_df$PCI_perc_change, failed_df$num_months_insufficient,
                             decreasing=TRUE), ]
failed_scenarios <- failed_df[, c('site', 'total_precip_perc_change', 'PCI_perc_change')]
failed_scenarios <- failed_scenarios[!duplicated(failed_scenarios), ]
hist(failed_scenarios$total_precip_perc_change, breaks=50)
num_failed <- dim(failed_scenarios)[1]

succeeded1mo <- sum_df[sum_df$num_months_insufficient == 1,
                                 c('site', 'PCI_perc_change', 'total_precip_perc_change')]
succeeded1mo <- succeeded1mo[!duplicated(succeeded1mo), ]
num_successful_scenarios_1mo <- dim(succeeded1mo)[1]

succeeded1or2mos <- sum_df[(sum_df$num_months_insufficient == 2) |
                            (sum_df$num_months_insufficient == 1),
                              c('site', 'PCI_perc_change', 'total_precip_perc_change')]
succeeded1or2mos <- succeeded1or2mos[!duplicated(succeeded1or2mos), ]
num_successful_scenarios_1or2mo <- dim(succeeded1or2mos)[1]

# regression analysis with 1 month of diet insufficiency
subs_1 <- sum_df[(sum_df$num_months_insufficient == 1), ]
subs_1 <- aggregate(density~site + total_precip_perc_change + PCI_perc_change,
                    data=subs_1, FUN=max)
perturb_df <- read.csv("C:/Users/ginge/Documents/NatCap/model_inputs_Kenya/regional_precip_perturbations/precip_perturbations.csv")
perturb_df <- perturb_df[, colnames(perturb_df)[2:6]]
colnames(perturb_df) <- c('PCI', 'PCI_perc_change', 'site', 'tot_precip', 'total_precip_perc_change')

sum_perturb_df <- merge(subs_1, perturb_df)
sum_perturb_df$PCI <- sum_perturb_df$PCI *100
sum_perturb_df$PCI_quartiles <- cut(sum_perturb_df$PCI,
                                    breaks=c(quantile(sum_perturb_df$PCI)),
                                    labels=c("First quartile PCI", "Second quartile PCI",
                                             "Third quartile PCI", "Fourth quartile PCI"),
                                    include.lowest=TRUE)

p <- ggplot(sum_perturb_df, aes(x=tot_precip, y=density))
p <- p + geom_point()
p <- p + facet_wrap(~PCI_quartiles)
# p <- p + facet_grid(PCI_perc_change~site)
p <- p + xlab("Annual precipitation (cm)")
p <- p + ylab("Maximum viable density (animals/ha)")
p <- p + print_theme
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/max_viable_density_v_precip.png"
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

p <- ggplot(sum_perturb_df, aes(x=PCI, y=density))
p <- p + geom_point()
p <- p + facet_wrap(~precip_bin)
p <- p + xlab("Precipitation Concentration Index")
p <- p + ylab("Maximum viable density (animals/ha)")
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/max_viable_density_v_PCI.png"
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

model <- lm(density~PCI*tot_precip + site, data=sum_perturb_df)
library(lm.beta)
lm.final.beta <- lm.beta(model)
print(lm.final.beta)
summary(lm.final.beta)
coef(lm.final.beta)

model_resid <- resid(model)
plot(sum_perturb_df$tot_precip, model_resid, ylab="Residuals",
     xlab="Precip")
plot(sum_perturb_df$PCI, model_resid, ylab="Residuals",
     xlab="PCI")

# combine maximum viable densities, from precip perturbation dataset, with
# reported densities. look at mismatch in max viable densities and reported densities
# by management / ecological class.
####### generate property df: property attributes like reported stocking density, ecolclass, etc
property_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/property_df.csv"
FID_list <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Property_FID_match.csv")
est_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/reg_cattle_estimates_11.30.16.csv"
est_df <- read.csv(est_csv)
est_df$property_cattle <- (est_df$PropCattle0 + est_df$PropCattleT.6) / 2
est_df$non_property_cattle <- (est_df$NonPropCattle0 + est_df$NonPropCattleT.6) / 2
est_df$property_non_property_cattle <- est_df$property_cattle + est_df$non_property_cattle
est_df[is.na(est_df$property_non_property_cattle), "property_non_property_cattle"] <- 
  est_df[is.na(est_df$property_non_property_cattle), "EstCattle"]
est_df$prop_non_density <- est_df$property_non_property_cattle / est_df$LwfPropSizeHa
est_df <- est_df[, c("Property", "prop_non_density", "Confirmed")]
est_df <- merge(est_df, FID_list, by.x="Property", by.y="NAME", all.x=TRUE) # [, 2:25]
est_df[which(est_df$Confirmed == 'yes'), 'Confirmed'] <- 'Yes'
est_df[which(est_df$Confirmed == 'no'), 'Confirmed'] <- 'No'
science_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/PropertyMasterFile_10July2016_Final.csv")
science_df <- science_df[, colnames(science_df)[c(1, 2, 17, 18, 32)]]
property_df <- merge(science_df, est_df, all=TRUE)
write.csv(property_df, property_csv, row.names=FALSE)
#######
climate_levels <- c(0, -0.6, 0.6)
property_df <- read.csv(property_csv, stringsAsFactors=FALSE)
property_df[property_df$Property == "Makurian", 'prop_non_density'] <- NA  # remove outlier
property_df[property_df$Property == "Lombala", "EcolClass"] <- "Livestock"  # use EconClass if EcolClass is missing
sum_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_summary_2018-10-18--18_14_10.csv"
sum_df <- read.csv(sum_csv)
sum_df <- sum_df[, colnames(sum_df)[c(1, 3:6)]]
viable_df <- sum_df[(sum_df$num_months_insufficient == 1), ]
viable_df <- aggregate(density~site + total_precip_perc_change + PCI_perc_change,
                       data=viable_df, FUN=max)
viable_df <- viable_df[viable_df$PCI_perc_change == 0, ]
viable_df <- viable_df[viable_df$total_precip_perc_change %in% climate_levels, ]
viable_df <- viable_df[, c("site", "total_precip_perc_change", "density")]
colnames(viable_df)[3] <- "mvdensity"

# merge calculated max viable density with other property attributes
viable_classified <- merge(property_df, viable_df, by.x="FID", by.y="site")
viable_classified$space_wl <- viable_classified$mvdensity - viable_classified$prop_non_density  # space for wildlife

# boxplot: current room for wildlife (proportion) by ecol class
cur_climate <- viable_classified[viable_classified$total_precip_perc_change == 0, ]
cur_climate$perc_room_for_wildlife <- (1 - (cur_climate$prop_non_density / cur_climate$mvdensity))
plot_df <- cur_climate
plot_df$EcolClass <- factor(plot_df$EcolClass, levels=c("Livestock", "Integrated", "Wildlife"))
plot_df[is.na(plot_df$EcolClass), "EcolClass"] <- plot_df[is.na(plot_df$EcolClass), "EconClass"]  # use EconClass if EcolClass is missing
plot_df <- plot_df[!is.na(plot_df$EcolClass), ]
p <- ggplot(plot_df, aes(x=EcolClass, y=perc_room_for_wildlife))
p <- p + geom_boxplot()
p <- p + print_theme + xlab("") + ylab("Proportion forage available for wildlife")
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/reported_v_mvdensity_by_EcolClass.png"
png(file=pngname, units="in", res=300, width=2.9, height=3.3)
print(p)
dev.off()

# plausible climate extremes: +/- 60%
# reshape: climate scenarios to columns
viable_scenarios <- reshape(viable_classified, idvar=colnames(viable_classified)[c(1:6)],
                      timevar='total_precip_perc_change', v.names=colnames(viable_classified)[c(10,11)],
                      direction='wide')
# management extremes: min and max observed stocking densities
min_rep_density <- min(property_df$prop_non_density, na.rm=TRUE)
max_rep_density <- max(property_df$prop_non_density, na.rm=TRUE)
viable_scenarios$space_wl_maxrepdensity <- viable_scenarios$mvdensity.0 - max_rep_density  # current max viable density minus max reported density
viable_scenarios$space_wl_minrepdensity <- viable_scenarios$mvdensity.0 - min_rep_density  # current max viable density minus min reported density

# formatted table for manuscript
ms_table <- viable_scenarios
ms_table <- ms_table[, c("Felicia_id", "EcolClass", "prop_non_density", "mvdensity.0",
                         "space_wl.0", "space_wl.-0.6", "space_wl.0.6",
                         "space_wl_maxrepdensity", "space_wl_minrepdensity")]
colnames(ms_table) <- c("Property", "Ecological classification", "Stocking rate", "Maximum viable density",
                        "Space for wildlife current", "Space for wildlife low rainfall", "Space for wildlife high rainfall",
                        "Space for wildlife high stocking", "Space for wildlife low stocking")
ms_table <- ms_table[order(ms_table$Property), ]
write.csv(ms_table,
          "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/space_wildlife_climate_mgmt_scenarios.csv",
          row.names=FALSE)

# percent change space_wl = change in space for wildlife in climate/mgmt scenarios
# (future - current) / current
viable_scenarios$pch_spwl_climate_up <- (viable_scenarios$space_wl.0.6 - viable_scenarios$space_wl.0)/viable_scenarios$space_wl.0
viable_scenarios$pch_spwl_climate_down <- (viable_scenarios$`space_wl.-0.6` - viable_scenarios$space_wl.0)/viable_scenarios$space_wl.0
viable_scenarios$pch_spwl_stockdens_up <- (viable_scenarios$space_wl_maxrepdensity - viable_scenarios$space_wl.0)/viable_scenarios$space_wl.0
viable_scenarios$pch_spwl_stockdens_down <- (viable_scenarios$space_wl_minrepdensity - viable_scenarios$space_wl.0)/viable_scenarios$space_wl.0

# histogram illustration of scenarios
library(ggplot2)
library("RColorBrewer")
pch_wide <- viable_scenarios[, c("Property", "pch_spwl_climate_up", "pch_spwl_climate_down",
                                 "pch_spwl_stockdens_up", "pch_spwl_stockdens_down")]
pch_wide <- pch_wide[!is.na(pch_wide$pch_spwl_climate_up), ]
pch_long <- reshape(pch_wide, idvar="Property", varying=colnames(pch_wide)[2:5],
                    v.names="percent_change", times=c("climate_up", "climate_down", "stockdens_up", "stockdens_down"),
                    timevar="scenario", direction="long")
pch_long$scenario <- factor(pch_long$scenario,
                            levels=c("climate_down", "stockdens_up", "stockdens_down", "climate_up"),
                            labels=c("Low rainfall", "High stocking density", "Low stocking density", "High rainfall"))
pch_long$perc_change_100 <- pch_long$percent_change * 100
rdbu <- brewer.pal(n=4, name = "RdBu")
rdbupal <- c(rdbu[2], rdbu[1], rdbu[3], rdbu[4]) 
brblpal <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")
photocopy_pal <- c('#fef0d9','#fdcc8a','#fc8d59','#d7301f')

# density plot
p <- ggplot(pch_long, aes(perc_change_100, fill=scenario))
p <- p + geom_density(alpha=0.7)
p <- p + ylab("Density")
p <- p + xlab("Space for wildlife: percent change from current")
p <- p + theme(axis.title=element_text(size=10),
               axis.text=element_text(size=8.5),
               panel.background=element_rect(fill="white", colour="grey50"),
              legend.title=element_blank(),
              legend.background=element_blank(),
              legend.key.width=unit(0.5, "cm"),
              legend.key.height=unit(0.5, "cm"),
              legend.text=element_text(margin=margin(r=5)),
              legend.position=c(0.73, 0.8))
p <- p + scale_fill_manual(values=photocopy_pal, guide=guide_legend(label.position="left"))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/scenarios_histogram.png"
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

# bar chart
p <- ggplot(pch_long, aes(x=scenario, y=perc_change_100))
p <- p + geom_bar(stat="identity", aes(fill=scenario))
p <- p + geom_hline(yintercept=0, colour="grey50")
p <- p + scale_fill_manual(values=brblpal)
p <- p + facet_wrap(~Property, ncol=4)
p <- p + ylab("Space for wildlife: percent change from current")
p <- p + theme(strip.background = element_blank(),
               strip.text.x = element_blank(),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               panel.background=element_rect(fill="white", colour="grey50"),
               axis.title.y=element_text(size=10),
               axis.text=element_text(size=8.5),
               legend.title=element_blank(),
               legend.key.size=unit(0.3, "cm"),
               legend.position=c(0.75, 0.08))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/scenarios_bar_chart.png"
png(file=pngname, units="in", res=300, width=3.5, height=4)
print(p)
dev.off()

# what % of livestock are shoats, on livestock properties?
property_df <- read.csv(property_csv)
dung_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_dung_2015_property_means_grouped.csv")
comp_df <- merge(property_df, dung_df, by.x="FID", by.y="Property")
comp_df$livestock <- comp_df$bovid + comp_df$shoat
livestock_properties <- comp_df[comp_df$EcolClass == "Livestock", ]
perc_shoat = livestock_properties$shoat / livestock_properties$livestock
summary(perc_shoat)
bodyweights <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Wildlife_body_weights.csv")
shoat_weights <- bodyweights[bodyweights$common.name == 'Shoat', 'weight..kg.']
avg_shoat_weight <- mean(shoat_weights)
avg_cattle_weight <- mean(bodyweights[bodyweights$common.name == 'Cattle', 'weight..kg.'])
shoat_aue <- (avg_shoat_weight^0.75)/(453.6^0.75)
cattle_aue <- (avg_cattle_weight^0.75)/(453.6^0.75)
