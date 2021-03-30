# Summarize results of RPM scenario runs
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()
relative_to_baseline <- function(df) {
  df$relative_value <- 0
  for (y in unique(df$year)) {
    for (id in unique(df$run_id)) {
      id_val <- df[(df$run_id == id) & (df$year == y), 'pixel_mean']
      baseline_val <- df[(df$run_id == 'A') & (df$year == y), 'pixel_mean']
      df[(df$run_id == id) & (df$year == y), 'relative_value'] <- (id_val - baseline_val) / baseline_val * 100
    }
  }
  return(df)
}

countif <- function(x) {
  return (length(x[(x >= 1) & !(is.na(x))]))
}

dietsuf_count <- function(df) {
  # count number of months in 2017 when mean diet sufficiency was >=1
  subs2017 <- df[df$year == 2017, c('run_id', 'pixel_mean')]
  count2017 <- aggregate(pixel_mean~run_id, data=subs2017, FUN=countif)
  colnames(count2017) <- c('run_id', 'n_months_gte_1')
  return (count2017)
}

# Julian Ahlborn's sites
outer_dir = "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_scenarios"
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/Ahlborn_scenarios/summary_figs"

# summarize number of months where diet sufficiency >= 1
diet_suff_list <- list()
for (aoi_idx in c(1:15)) {
  dietsuf_path <- paste(outer_dir, paste('aoi_', aoi_idx, sep=''), 'monthly_diet_suff_summary.csv', sep='/')
  dietsuf <- read.csv(dietsuf_path)
  gte1_df <- dietsuf_count(dietsuf)
  gte1_df$site <- aoi_idx
  diet_suff_list[[aoi_idx]] <- gte1_df
}
dietsuf_df <- do.call(rbind, diet_suff_list)
baseline_dietsuf <- dietsuf_df[dietsuf_df$run_id == 'A', ]
baseline_dietsuf <- merge(baseline_dietsuf, worldclim_df)

p <- ggplot(baseline_dietsuf, aes(x=site, y=n_months_gte_1))
p <- p + geom_col() + ylab("Number of months (out of 12) \n diet sufficiency >= 1")
print(p)
pngname <- paste(fig_dir, "diet_sufficiency_gte1_baseline.png", sep='/')
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()


# pixel mean values
df_list = list()
for(aoi_idx in c(1:15)) {
  summary_df_path <- paste(outer_dir, paste('aoi_', aoi_idx, sep=''), 'average_value_summary.csv', sep='/')
  summary_df <- read.csv(summary_df_path)
  summary_df$site <- aoi_idx
  df_list[[aoi_idx]] <- summary_df
}
ave_val_df <- do.call(rbind, df_list)

# percent change relative to baseline
df_list = list()
for(aoi_idx in c(1:15)) {
  change_df_path <- paste(outer_dir, paste('aoi_', aoi_idx, sep=''), 'perc_change_summary.csv', sep='/')
  change_df <- read.csv(change_df_path)
  change_df$site <- aoi_idx
  df_list[[aoi_idx]] <- change_df
}
perc_change_df <- do.call(rbind, df_list)

# precipitation from Worldclim at Julian's sites
worldclim_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_inputs/Ahlborn_sites/intermediate_data/worldclim_precip.csv")
worldclim_df <- aggregate(prec~site, worldclim_df, FUN=sum)
colnames(worldclim_df) <- c('site', 'annual_prec_worldclim')
breaks <- c(15, 20, 25, 30)
tags <- c('10 - 15', '15 - 20', '20 - 25', '25 - 30')
# classify precip into bins used in fig 1
worldclim_df$precip_tags <- cut(worldclim_df$annual_prec_worldclim, breaks=breaks,
                   include.lowest=TRUE, right=FALSE, labels=tags)
worldclim_df$precip_tags <- factor(worldclim_df$precip_tags, levels=tags, ordered=TRUE)
# colors for precip levels used in fig 1
prec_colors <- c("#8fc2de", "#5ba3d0", "#3182be", "#08306b")  # 25-30, 35+
names(prec_colors) <- tags
precip_scale <- scale_colour_manual(name="worldclim", values=prec_colors)

# summarize mean, maximum change in biomass & diet sufficiency
pc_df_2017 <- perc_change_df[(perc_change_df$year == 2017), ]
mean_pc_df <- aggregate(mean_perc_change~run_id + output, pc_df_2017, FUN=mean)
colnames(mean_pc_df) <- c('run_id', 'output', 'mean_perc_change_across_sites')
max_pc_df <- aggregate(mean_perc_change~run_id + output, pc_df_2017, FUN=max)
colnames(max_pc_df) <- c('run_id', 'output', 'max_perc_change_across_sites')
summarized <- merge(mean_pc_df, max_pc_df)
write.csv(summarized, paste(fig_dir, 'percent_change_summary_across_sites.csv', sep='/'), row.names=FALSE)

# summary of diet sufficiency values across pixels within site, baseline scenario
ave_val_df <- merge(ave_val_df, worldclim_df)
baseline_ds_df <- ave_val_df[(ave_val_df$year == 2017) &
                                (ave_val_df$run_id == 'A') &
                                (ave_val_df$output == 'diet_sufficiency'), ]
p <- ggplot(baseline_ds_df, aes(x=annual_prec_worldclim, y=pixel_mean))
p <- p + geom_text(aes(label=site)) # geom_point() + geom_errorbar(aes(ymin=pixel_min, ymax=pixel_max), width=0.2)
p <- p + print_theme + xlab("Average annual precipitation (cm)") + ylab("Diet sufficiency")
print(p)
pngname <- paste(fig_dir, "diet_sufficiency_by_site_baseline_sitelabels.png", sep='/') # "diet_sufficiency_by_site_baseline.png", sep='/')
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

perc_change_df <- merge(perc_change_df, worldclim_df)
perc_change_df$output <- factor(perc_change_df$output,
                                levels=c('standing_biomass', 'diet_sufficiency'),
                                labels=c('Biomass', 'Diet sufficiency'))

# density plot contrasting changes in grazing: with and without changes in climate
gr_sc_df <- perc_change_df[(perc_change_df$year == 2017) & (perc_change_df$run_id %in% c('E', 'K', 'H', 'I')), ]
gr_sc_df$run_id <- factor(gr_sc_df$run_id, levels=c('H', 'I', 'E', 'K'),
                          labels=c('Doubled animal density alone',
                                   'No grazing alone',
                                   'Doubled animal density, \nelevated temperature \nand precipitation',
                                   'No grazing, \nelevated temperature \nand precipitation'))
p <- ggplot(gr_sc_df, aes(mean_perc_change, fill=run_id))
p <- p + geom_density(alpha=0.7) + facet_wrap(~output, nrow=2) + scale_color_brewer(palette="Accent")  # Set2
p <- p + facet_wrap(~output, nrow=2, scales='free_y') + theme(legend.title = element_blank())
p <- p + print_theme + xlab("Percent change from baseline") + ylab("Density")
print(p)
pngname <- paste(fig_dir, "grazing_scenarios_oneway_threeway.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=6)
print(p)
dev.off()

# one-way drivers colored by site precip
oneway_df <- perc_change_df[(perc_change_df$year == 2017) &
                              (perc_change_df$run_id %in% c('B', 'F', 'H', 'I')), ]
oneway_df$run_id <- factor(oneway_df$run_id, levels=c('B', 'F', 'H', 'I'),
                           labels=c('Elevated \ntemperature', 'Elevated \nprecipitation', 'Doubled \nanimal density',
                                    'No grazing'))
p <- ggplot(oneway_df, aes(x=run_id, y=mean_perc_change)) + precip_scale
p <- p + geom_jitter(width=0.05, aes(color=precip_tags)) + facet_wrap(~output, nrow=2, scales='free_y')
p <- p + geom_abline(intercept=0, slope=0, linetype='dashed')
p <- p + print_theme + theme(axis.text.x=element_text(angle=90)) + theme(legend.position="none")
p <- p + xlab("") + ylab("Percent change from baseline")
print(p)
pngname <- paste(fig_dir, "perc_change_colored_by_precip.png", sep='/')
png(file=pngname, units="in", res=300, width=3, height=5)
print(p)
dev.off()

# one-way changes only
oneway_df <- perc_change_df[(perc_change_df$year == 2017) &
                              (perc_change_df$run_id %in% c('B', 'F', 'H', 'I')), ]
p <- ggplot(oneway_df, aes(x=annual_prec_worldclim, y=mean_perc_change))
p <- p + geom_smooth(method="lm", color='black', size=0.1, alpha=0.2, formula=y~x)
p <- p + geom_text(aes(label=site))
p <- p + facet_grid(run_id~output, scales='free_y') + print_theme
p <- p + xlab("Average annual precipitation (cm)") + ylab("Percent change from baseline")
print(p)
pngname <- paste(fig_dir, "oneway_scenarios_sitelabels.png", sep='/')
png(file=pngname, units="in", res=300, width=4, height=6)
print(p)
dev.off()

# two-way scenarios only
twoway_df <- perc_change_df[(perc_change_df$year == 2017) &
                              (perc_change_df$run_id %in% c('C', 'D', 'G', 'J', 'L')), ]
p <- ggplot(twoway_df, aes(x=annual_prec_worldclim, y=mean_perc_change))
p <- p + geom_smooth(method="lm", color='black', size=0.1, alpha=0.2, formula=y~x)  # + geom_point()
p <- p + geom_text(aes(label=site))
# p <- p + geom_errorbar(aes(ymin=min_perc_change, ymax=max_perc_change))
p <- p + facet_grid(run_id~output, scales='free') + print_theme
p <- p + xlab("Average annual precipitation (cm)") + ylab("Percent change from baseline")
print(p)
pngname <- paste(fig_dir, "twoway_scenarios_sitelabels.png", sep='/')  # "twoway_scenarios.png", sep='/')
png(file=pngname, units="in", res=300, width=4, height=7.5)
print(p)
dev.off()

# three-way scenarios only
threeway_df <- perc_change_df[(perc_change_df$year == 2017) &
                                  (perc_change_df$run_id %in% c('E', 'K')), ]
p <- ggplot(threeway_df, aes(x=annual_prec_worldclim, y=mean_perc_change))
p <- p + geom_smooth(method="lm", color='black', size=0.1, alpha=0.2, formula=y~x) # + geom_point()
p <- p + geom_text(aes(label=site))
# p <- p + geom_errorbar(aes(ymin=min_perc_change, ymax=max_perc_change))
p <- p + facet_grid(run_id~output, scales='free') + print_theme
p <- p + xlab("Average annual precipitation (cm)") + ylab("Percent change from baseline")
print(p)
pngname <- paste(fig_dir, "threeway_scenarios_sitelabels.png", sep='/')  # "threeway_scenarios.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=5)
print(p)
dev.off()

# change in biomass vs change in diet sufficiency
restr_df <- perc_change_df[(perc_change_df$run_id %in% c('B', 'C', 'D', 'E', 'F', 'G', 'H')), ]
comb_resh <- reshape(restr_df, idvar=c('run_id', 'site'), timevar='output', direction='wide',
                     drop=c('year', 'min_perc_change', 'max_perc_change', 'annual_prec_worldclim'))
colnames(comb_resh) <- c('run_id', 'site', 'perc_change_biomass', 'perc_change_dietsuff')
comb_resh$site <- factor(comb_resh$site)
# comb_resh <- merge(comb_resh, residual_df)
comb_resh <- merge(comb_resh, worldclim_df)
p <- ggplot(comb_resh, aes(x=perc_change_biomass, y=perc_change_dietsuff, color=site)) # , color=residual))
p <- p + geom_point() + facet_wrap(~run_id, nrow=4, scales='free') + theme(legend.position='none')
p <- p + xlab("Percent change: biomass") + ylab("Percent change: diet sufficiency")
print(p)
pngname <- paste(fig_dir, "perc_change_dietsuff_vs_biomass_by_scenario.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=8)
print(p)
dev.off()

p <- ggplot(comb_resh, aes(x=perc_change_biomass, y=perc_change_dietsuff, color=run_id)) # , color=residual))
p <- p + geom_point()
p <- p + xlab("Percent change: biomass") + ylab("Percent change: diet sufficiency")
p <- p + geom_abline(intercept=0, slope=1, linetype='dotted')
print(p)
pngname <- paste(fig_dir, "perc_change_dietsuff_vs_biomass_by_scenario.png", sep='/')
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(comb_resh, aes(x=perc_change_biomass, y=perc_change_dietsuff, color=precip_tags))
p <- p + geom_point() + precip_scale
p <- p + xlab("Percent change: biomass") + ylab("Percent change: diet sufficiency")
p <- p + geom_abline(intercept=0, slope=1, linetype='dotted')
p <- p + print_theme + theme(legend.position='none')
print(p)
pngname <- paste(fig_dir, "perc_change_dietsuff_vs_biomass_by_precip.png", sep='/')
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

# relationship btw magnitude of change in biomass vs change in biomass
comb_resh$change_comp <- comb_resh$perc_change_dietsuff / comb_resh$perc_change_biomass
subs <- comb_resh[comb_resh$change_comp > -5, ]  # leave out one outlier
p <- ggplot(subs, aes(x=annual_prec_worldclim, y=change_comp, color=site))
p <- p + geom_point() + facet_wrap(~run_id)
print(p)
change_comp <- change_comp[change_comp > -5]
hist(change_comp, breaks=50)
summary(change_comp)


# residuals of each site from linear model relating empirical dung density to annual precip
# (an indicator of grazing intensity at each site relative to productivity)
residual_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn summaries_GK/lm_dung_from_precip_residuals.csv")

# WCS monitoring area in the Gobi
summary_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/RPM_scenarios/revised_9.8.20/average_value_summary.csv")
fig_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/RPM_scenarios/revised_9.8.20/summary_figs"
dir.create(fig_dir)

# biomass summaries across pixels
biomass_df <- relative_to_baseline(summary_df[(summary_df$output == 'standing_biomass') &
                                                (summary_df$aggregation_method == 'yearly_average_across_pixels_across_months'), ])
p <- ggplot(biomass_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab("Average biomass, all months \n Relative to baseline")
pngname <- paste(fig_dir, "biomass_avg_all_months.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

biomass_df <- relative_to_baseline(summary_df[(summary_df$output == 'standing_biomass') &
                           (summary_df$aggregation_method == 'growing_season_sum_average_across_pixels'), ])
p <- ggplot(biomass_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab("Average biomass, growing season \n Relative to baseline")
pngname <- paste(fig_dir, "biomass_avg_growing_season.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

biomass_df <- relative_to_baseline(summary_df[(summary_df$output == 'standing_biomass') &
                           (summary_df$aggregation_method == 'growing_season_sum_stdev_across_pixels'), ])
p <- ggplot(biomass_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab('Std deviation biomass, growing season \n Relative to baseline')
pngname <- paste(fig_dir, "biomass_std_dev_growing_season.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

# diet sufficiency summaries across pixels
ds_df <- relative_to_baseline(summary_df[(summary_df$output == 'diet_sufficiency') &
                                         (summary_df$run_id != 'F') &
                                         (summary_df$aggregation_method == 'yearly_average_across_pixels_across_months'), ])
p <- ggplot(ds_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab("Average diet sufficiency, all months \n Relative to baseline")
pngname <- paste(fig_dir, "diet_sufficiency_avg_all_months.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

ds_df <- relative_to_baseline(summary_df[(summary_df$output == 'diet_sufficiency') &
                                           (summary_df$run_id != 'F') &
                                                (summary_df$aggregation_method == 'growing_season_sum_average_across_pixels'), ])
p <- ggplot(ds_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab("Average diet sufficiency, growing season \n Relative to baseline")
pngname <- paste(fig_dir, "diet_sufficiency_avg_growing_season.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

ds_df <- relative_to_baseline(summary_df[(summary_df$output == 'diet_sufficiency') &
                                           (summary_df$run_id != 'F') &
                                                (summary_df$aggregation_method == 'growing_season_sum_stdev_across_pixels'), ])
p <- ggplot(ds_df, aes(x=run_id, y=relative_value))
p <- p + geom_point() + xlab("Scenario")
p <- p + facet_wrap(~year) + ylab('Std deviation diet sufficiency, growing season \n Relative to baseline')
pngname <- paste(fig_dir, "diet_sufficiency_std_dev_growing_season.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

# Laikipia Kenya
outer_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/Laikipia_RPM/RPM_scenarios"
fig_dir <- paste(outer_dir, "summary_figs", sep='/')
dir.create(fig_dir)
ave_val_df <- read.csv(paste(outer_dir, "average_value_summary.csv", sep='/'))
summary_df <- relative_to_baseline(ave_val_df)
summary_df$year_of_scenario_conditions <- NA
summary_df[summary_df$year == 2016, 'year_of_scenario_conditions'] <- 'first year'
summary_df[summary_df$year == 2017, 'year_of_scenario_conditions'] <- 'second year'
summary_df$run_id <- factor(summary_df$run_id,
                            levels=c('A', 'B', 'C', 'D', 'F', 'G', 'I', 'J', 'L'),
                            labels=c('baseline', 'increased precip', 
                                     'decreased precip', 'increased density', 'no grazing',
                                     'increased precip/ \nincreased density', 'increased precip/\nno grazing',
                                     'decreased precip/\nincreased density', 'decreased precip/\nno grazing'))

biomass_yearly_df <- summary_df[(summary_df$output == 'standing_biomass') &
                                  (summary_df$aggregation_method == 'yearly_average_across_pixels_across_months'), ]
print(min(biomass_yearly_df[biomass_yearly_df$run_id == 'D', 'relative_value']))
print(max(biomass_yearly_df[biomass_yearly_df$run_id == 'F', 'relative_value']))
p <- ggplot(biomass_yearly_df, aes(x=run_id, y=relative_value, color=year_of_scenario_conditions))
p <- p + geom_point() + xlab("Scenario")
p <- p + theme(axis.text.x = element_text(angle=-90)) + theme(legend.title = element_blank())
p <- p + ylab("Average biomass, all months \n Percent change from baseline") # + facet_wrap(~site) 
print(p)
pngname <- paste(fig_dir, "biomass_avg_all_months.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=4.5)
print(p)
dev.off()

ds_df <- summary_df[(summary_df$output == 'diet_sufficiency') &
                      (summary_df$run_id != 'no grazing') &
                      (summary_df$run_id != 'increased precip/\nno grazing') &
                      (summary_df$run_id != 'decreased precip/\nno grazing') &
                      (summary_df$aggregation_method == 'yearly_average_across_pixels_across_months'), ]
print(min(ds_df[ds_df$run_id == 'D', 'relative_value']))
p <- ggplot(ds_df, aes(x=run_id, y=relative_value, color=year_of_scenario_conditions))
p <- p + geom_point() + xlab("Scenario")
p <- p + theme(axis.text.x = element_text(angle=-90)) + theme(legend.title = element_blank())
p <- p + ylab("Average diet sufficiency, all months \n Relative to baseline")
pngname <- paste(fig_dir, "diet_sufficiency_avg_all_months.png", sep='/')
png(file=pngname, units="in", res=300, width=6, height=4.5)
print(p)
dev.off()

# compare biomass without grazing from RPM vs Century
compare_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/Ahlborn_scenarios/Nov62020/biomass_comparison_with_century.csv")
compare_df$biomass_Century <- compare_df$total_biomass * 10
compare_df$diff <- compare_df$standing_biomass_RPM - compare_df$biomass_Century
p <- ggplot(compare_df, aes(x=month, y=diff))
p <- p + geom_point()
p <- p + facet_wrap(~site_id)
print(p)
