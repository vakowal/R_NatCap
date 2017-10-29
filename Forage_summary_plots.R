# Plot output from livestock forage model #

library(ggplot2)
library(grid)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

summary_plots <- function(model_results_folder, imgpath){
  dir.create(file.path(imgpath))
  lines <- c("solid", "longdash", "dotted", "twodash")
  
  summary_file <- read.csv(paste(model_results_folder,
                                 "summary_results.csv", sep = "/"), header = TRUE)
  summary_file$date <- summary_file$year + (1/12) * summary_file$month
  grass_type_cols <- grep("kgha", colnames(summary_file), value=TRUE)
  grass_types <- gsub("_kgha", "", grass_type_cols)
  summary_file$total_grass <- rowSums(summary_file[, grass_type_cols])
  animal_type_cols <- grep("gain_kg", colnames(summary_file), value=TRUE)
  animal_types <- gsub("_gain_kg", "", animal_type_cols)
  animal_types <- animal_types[animal_types != "total"]
  summary_list = list()
  for(grass in grass_types){
    one_df <- data.frame('step'=summary_file$step,
                         'year'=summary_file$year,
                         'month'=summary_file$month,
                         'biomass'=summary_file[, paste(grass, '_kgha', sep="")],
                         'offtake'=rep(NA, length(summary_file$step)),
                         'label'=rep(grass, length(summary_file$step)),
                         'type'=rep('grass', length(summary_file$step)))
    summary_list[[grass]] <- one_df
  }
  one_df <- data.frame('step'=summary_file$step,
                       'year'=summary_file$year,
                       'month'=summary_file$month,
                       'biomass'=summary_file[, 'total_grass'],
                       'offtake'=rep(NA, length(summary_file$step)),
                       'label'=rep('total_grass', length(summary_file$step)),
                       'type'=rep('grass', length(summary_file$step)))
  summary_list[['total']] <- one_df
  for(animal in animal_types){
      one_df <- data.frame('step'=summary_file$step,
                           'year'=summary_file$year,
                           'month'=summary_file$month,
                           'biomass'=summary_file[, paste(animal, '_kg', sep="")],
                           'offtake'=summary_file[, paste(animal, '_intake_forage_per_indiv_kg', sep="")],
                           'label'=rep(animal, length(summary_file$step)),
                           'type'=rep('animal', length(summary_file$step)))
      summary_list[[animal]] <- one_df  
  }
  summary_df <- do.call(rbind, summary_list)
  summary_df$date <- summary_df$year + (1/12) * summary_df$month
  
  biomass_subset <- summary_df[(summary_df$label %in% c('total_grass', animal_types)), ]
  p <- ggplot(biomass_subset, aes(x=date, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("biomass (kg)") + ggtitle("Total biomass")
  pngname <- paste(imgpath, "Biomass_summary.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  grass_biomass_subset <- summary_df[(summary_df$label %in% c('total_grass', grass_types)), ]
  p <- ggplot(grass_biomass_subset, aes(x=date, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("Biomass (kg/ha)") + ggtitle("Forage biomass")
  p <- p + geom_point(aes(x=2016.708, y=916))
  pngname <- paste(imgpath, "Grass_biomass.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  animal_biomass_subset <- summary_df[(summary_df$label %in% animal_types), ]
  p <- ggplot(animal_biomass_subset, aes(x=date, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("Liveweight (kg)") + ggtitle("Goat body weight")
  p <- p + scale_linetype_manual(values = lines, name = "")
  pngname <- paste(imgpath, "Herbivore_biomass.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  p <- ggplot(animal_biomass_subset, aes(x=date, y=offtake, group=label, linetype=label))
  p <- p + geom_line() + ylab("offtake (kg)") + ggtitle("Herbivore offtake")
  p <- p + scale_linetype_manual(values = lines, name = "")
  pngname <- paste(imgpath, "Herbivore_offtake.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  # proportions of each grass type
  prop_list <- list()
  grass_labels <- gsub("_green", "", grep("_green", grass_types, value=TRUE))
  for(label in grass_labels){
    prop <- (summary_file[, paste(label, '_green_kgha', sep="")] + 
               summary_file[, paste(label, '_dead_kgha', sep="")]) / summary_file$total_grass
    prop_df <- data.frame('step'=summary_file$step,
                          'year'=summary_file$year,
                          'month'=summary_file$month,
                          'date'=summary_file$date,
                          'label'=rep(label, length(summary_file$step)),
                          'proportion_biomass'=prop)
    prop_list[[label]] <- prop_df
  }
  prop_df <- do.call(rbind, prop_list)
  
  p <- ggplot(prop_df, aes(x=date, y=proportion_biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("percentage of total biomass") + 
    ggtitle("Proportion of total biomass by grass type")
  p <- p + scale_linetype_manual(values = lines, name = "")
  pngname <- paste(imgpath, "Grass_proportion.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
}

summary_plots_rot <- function(model_results_folder, imgpath){
  dir.create(file.path(imgpath))
  lines <- c("solid", "longdash", "dotted", "dotdash", "twodash")
  summary_file <- read.csv(paste(model_results_folder,
                                 "pasture_summary.csv", sep = "/"),
                                 header = TRUE)
  summary_file$date <- summary_file$year + (1/12) * summary_file$month
  grass_type_cols_d <- grep("dead_kgha", colnames(summary_file), value=TRUE)
  grass_type_cols_g <- grep("green_kgha", colnames(summary_file), value=TRUE)
  grass_type_cols <- c(grass_type_cols_g, grass_type_cols_d)
  grass_types <- gsub("_kgha", "", c(grass_type_cols_d, grass_type_cols_g))
  
  # average grass biomass across pastures
  pasture_ave <- aggregate(summary_file[, grass_type_cols],
                           by=list(summary_file$date), FUN=mean)
  colnames(pasture_ave)[1] <- 'date'
  pasture_ave$total_grass <- rowSums(pasture_ave[, grass_type_cols])
  
  summary_list <- list()
  for(grass in grass_types){
    one_df <- data.frame('date'=pasture_ave$date,
                         'biomass'=pasture_ave[, paste(grass, '_kgha', sep="")],
                         'label'=rep(grass, length(pasture_ave$date)),
                         'type'=rep('grass', length(pasture_ave$date)))
    summary_list[[grass]] <- one_df
  }
  grass_biomass_df <- do.call(rbind, summary_list)
  p <- ggplot(grass_biomass_df, aes(x=date, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("biomass (kg/ha)") + ggtitle("Grass biomass")
  p <- p + scale_linetype_manual(values=lines, name=grass_biomass_df$label)
  pngname <- paste(imgpath, "Grass_biomass.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  # proportions of each grass type
  prop_list <- list()
  grass_labels <- gsub("_green", "", grep("_green", grass_types, value=TRUE))
  for(label in grass_labels){
    prop <- (pasture_ave[, paste(label, '_green_kgha', sep="")] + 
               pasture_ave[, paste(label, '_dead_kgha', sep="")]) / pasture_ave$total_grass
    prop_df <- data.frame('date'=pasture_ave$date,
                          'label'=rep(label, length(pasture_ave$date)),
                          'proportion_biomass'=prop)
    prop_list[[label]] <- prop_df
  }
  prop_df <- do.call(rbind, prop_list)
  
  p <- ggplot(prop_df, aes(x=date, y=proportion_biomass, group=label,
              linetype=label))
  p <- p + geom_line() + ylab("percentage of total biomass") + 
    ggtitle("Proportion of total biomass by grass type")
  p <- p + scale_linetype_manual(values=lines, name="")
  pngname <- paste(imgpath, "Grass_proportion.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
}

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/empirical_sd/st17"
imgpath <- paste(model_results_folder, 'figs', sep='/')
summary_plots(model_results_folder, imgpath)

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/sample_input_grass_parameters/rot_2_pastures_0.2_v_0.8_perc_knz_high_cp"
imgpath <- paste(model_results_folder, 'figs', sep='/')
summary_plots_rot(model_results_folder, imgpath)

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/sample_input_grass_parameters/control_0.2_v_0.8_perc_knz_high_cp"
imgpath <- paste(model_results_folder, 'figs', sep='/')
summary_plots(model_results_folder, imgpath)

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/rot_0.206_v_0.103_cp_0.2_v_0.8_perc"
imgpath <- paste(model_results_folder, 'figs', sep='/')
summary_plots_rot(model_results_folder, imgpath)

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/continuous"
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/continuous_figs"
summary_plots(model_results_folder, imgpath)

model_results_folder <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs/cattle_zebra"
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs/cattle_zebra"
summary_plots(model_results_folder, imgpath)

for(suf in levels(df$suf)){
  folder_name <- suf
  model_results_folder <- paste("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs/",
                                folder_name, sep="")
  imgpath <- paste("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs/",
                   folder_name, sep="")
  summary_plots(model_results_folder, imgpath)
}

## grass composition plots
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"
diff_sum <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/asymmetric_weight_effects_grass_proportions.csv"
df <- read.csv(diff_sum)
df$weight <- as.factor(df$weight)
p <- ggplot(df, aes(x=step, y=diff_abs, group=weight))
p <- p + geom_line(aes(colour=weight))
pngname <- paste(imgpath, "Grass_abundance_digest_abun_same.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()
p <- ggplot(df, aes(x=step, y=diff_proportion, group=weight))
p <- p + geom_line(aes(colour=weight))
p <- p + xlab('month') + ylab('grass1 / total - grass2 / total') + ggtitle("Grass proportions")
pngname <- paste(imgpath, "Grass_proportions_digest_abun_same.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

comp_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/asymmetric_weight_effects_grass_proportions_6.21.16.csv"
comp_df <- read.csv(comp_file)
comp_df <- comp_df[which(comp_df$step < 11), ]
comp_df$grass_type <- factor(comp_df$grass_type, levels=c('grass_1', 'grass_2'),
                             labels=c('grass 1', 'grass 2'))
p <- ggplot(comp_df, aes(x=step, y=prop, group=grass_type))
p <- p + geom_line(aes(linetype=grass_type))
p <- p + scale_linetype_manual(values=lines)
p <- p + xlab("Step") + ylab("Proportion of total biomass")
p <- p + print_theme + theme(legend.title=element_blank()) + theme(legend.key = element_blank())
p <- p + scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10))
pngname <- paste(imgpath, "Grass_proportions_from_50.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

## facilitation plots
df_list <- list()
for(suf in c('herb1_0.2', 'herb1_2_0.3', 'herb1_2_0.2')){
  summary_df <- data.frame('month'=c(1:12), 'weight_kg'=numeric(12), 'gain'=numeric(12),
                           'suf'=character(12))
  model_results_folder <- paste("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs",
                                paste(suf, "_grass1_grass2", sep=""), sep="/")
  # imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"  # ", suf, sep="/")
  # summary_plots(model_results_folder, imgpath)
  summary <- read.csv(paste(model_results_folder, 'summary_results.csv', sep="/"),
                      stringsAsFactors=FALSE)
  summary[, 'herb_1_kg'] = as.numeric(summary[, 'herb_1_kg'])
  summary_df$weight_kg <- summary$herb_1_kg
  summary[, 'herb_1_gain_kg'] = as.numeric(summary[, 'herb_1_gain_kg'])
  summary_df$gain <- summary$herb_1_gain_kg
  summary_df$suf <- suf
  df_list[[suf]] <- summary_df
  start_wt <- summary[1, 'herb_1_kg'] - summary[1, 'herb_1_gain_kg']
  end_wt <- summary[dim(summary)[1], 'herb_1_kg']
  delta_wt <- end_wt - start_wt
  # delta_wt_list[[suf]] <- delta_wt
}
summary_df <- do.call(rbind, df_list)
summary_df$label <- factor(summary_df$suf, levels=c('herb1_0.2', 'herb1_2_0.3', 'herb1_2_0.2'),
                              labels=c('Alone', 'Mixed increased density', 'Mixed equal density'))
p <- ggplot(summary_df, aes(x=month, y=weight_kg, group=label))
p <- p + geom_line(aes(linetype=label))
p <- p + scale_linetype_manual(values=lines)
p <- p + xlab('month') + ylab('weight (kg)')
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"
pngname <- paste(imgpath, "Direct_contrast_cumulative_gain.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

# boxplot: monthly individual gain by treatment
p <- ggplot(summary_df, aes(factor(label), gain))
p <- p + geom_boxplot()
p <- p + ylab("Monthly individual gain (kg)") + xlab("")
print(p)
pngname <- paste(imgpath, "Direct_contrast_indiv_gain.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

summary_df$herd_weight <- 0
summary_df[which(summary_df$suf == 'herb1_0.2'), 'herd_weight'] <- summary_df[which(summary_df$suf == 'herb1_0.2'), 'weight_kg'] * 0.2
summary_df[which(summary_df$suf == 'herb1_2_0.3'), 'herd_weight'] <- summary_df[which(summary_df$suf == 'herb1_2_0.3'), 'weight_kg'] * 0.2
summary_df[which(summary_df$suf == 'herb1_2_0.2'), 'herd_weight'] <- summary_df[which(summary_df$suf == 'herb1_2_0.2'), 'weight_kg'] * 0.1
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/direct_contrast_summary.csv"
write.csv(summary_df, file=save_as, row.names=FALSE)

p <- ggplot(summary_df, aes(x=month, y=herd_weight, group=label))
p <- p + geom_line(aes(linetype=label))
p <- p + scale_linetype_manual(values=lines)
p <- p + ylab("Herb1 herd weight (kg)")
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"
pngname <- paste(imgpath, "Direct_contrast_cumulative_gain_herd.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

p <- ggplot(summary_df, aes(x=month, y=weight_kg, group=label))
p <- p + geom_line(aes(linetype=label))
p <- p + scale_linetype_manual(values=lines)
p <- p + xlab('month') + ylab('weight (kg)')
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"
pngname <- paste(imgpath, "Cattle_weight_gain_generic_grasses.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

diff_weight <- summary_df[summary_df$suf == 'cattle_zebra', 'weight_kg'] - 
  summary_df[summary_df$suf == 'cattle', 'weight_kg']
diff_gain <- summary_df[summary_df$suf == 'cattle_zebra', 'gain'] - 
  summary_df[summary_df$suf == 'cattle', 'gain']

##### crude protein between treatments
setup_l <- c("_equal_sd_opposite_grass_varying_CP",
             "_equal_sd_opposite_grass",
             "_unequal_sd_opposite_grass_GH_varying_CP",
             "_unequal_sd_opposite_grass_GH")
treatment_l <- c("herb1", "herb2")
df_list <- list()
for(setup in setup_l){
  for(treatment in treatment_l){
    summary_df <- data.frame('month'=c(1:12), 'CPI_f'=numeric(12), 'DMD_f'=numeric(12),
                             'treatment'=rep(treatment, 12), 'setup'=rep(setup, 12))
    model_results_folder <- paste("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs",
                                  paste(treatment, setup, sep=""), sep="/")
    diet_csv <- paste(model_results_folder, "herb_1diet.csv", sep="/")
    diet_df <- read.csv(diet_csv)
    summary_df$CPI_f <- diet_df$CPIf
    summary_df$DMD_f <- diet_df$DMDf
    df_list[[paste(setup, treatment, sep="-")]] <- summary_df
  }
}
summary_df <- do.call(rbind, df_list)

diff_df_list <- list()
for(s in setup_l){
  diff_df <- data.frame('month'=c(1:12), 'setup'=rep(s, 12), 'diff_CPI'=numeric(12),
                        'diff_DMD'=numeric(12))
  dat <- subset(summary_df, setup == s)
  dif_CP <- dat[which(dat$treatment == 'herb2'), 'CPI_f'] - dat[which(dat$treatment == 'herb1'), 'CPI_f']
  dif_DMD <- dat[which(dat$treatment == 'herb2'), 'DMD_f'] - dat[which(dat$treatment == 'herb1'), 'DMD_f']
  diff_df$diff_CPI <- dif_CP
  diff_df$diff_DMD <- dif_DMD
  diff_df_list[[s]] <- diff_df
}
diff_df <- do.call(rbind, diff_df_list)
write.csv(diff_df, "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_inputs/facilitation_exploration/diet_diff_summary.csv",
            row.names=FALSE)

# new growth: calculated in script
sim_months <- c("2014_11", "2014_12", "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06",
                "2015_07", "2015_08", "2015_09", "2015_10", "2015_11", "2015_12")
lines <- c("solid", "dotted", "longdash")
growth_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/n_mult_start_2013/growth_summary.csv"
gr_df <- read.csv(growth_file)
gr_df$date <- paste('01', gr_df$month, gr_df$year, sep="-")
gr_df$date <- as.Date(gr_df$date, format="%d-%m-%Y")
gr_df$year_month <- format(gr_df$date, "%Y_%m")
gr_df <- gr_df[which(gr_df$year_month %in% sim_months), ]
gr_df$stocking_density <- as.factor(gr_df$stocking_density)
gr_df[which(gr_df$label == 'n_content_live'), 'biomass'] <- gr_df[which(gr_df$label == 'n_content_live'), 'biomass'] * 100
gr_df[which(gr_df$label == 'perc_green'), 'biomass'] <- gr_df[which(gr_df$label == 'perc_green'), 'biomass'] * 100
gr_df$label <- factor(gr_df$label, levels=c("weighted_cp_avg", "total_biomass", 'perc_green', "live_biomass", "new_growth", 
                                            "n_content_live", "liveweight_gain", 'liveweight_gain_herd'),
                      labels=c("c", "Total biomass (g/m2)", "Percent green biomass (%)", "Live biomass (g/m2)",
                               "New growth (g/m2)", "Crude protein in live biomass (%)",
                               "Indiv. liveweight_gain (kg/ha)", "Herd liveweight gain (kg/ha)"))
gr_df_sub <- subset(gr_df, gr_df$label %in% c("c", "Total biomass (g/m2)", "Percent green biomass (%)", 
                                               "New growth (g/m2)",
                                               "Herd liveweight gain (kg/ha)",
                                               "Crude protein in live biomass (%)"))
gr_df_sub <- gr_df
gr_df_sub <- gr_df_sub[order(gr_df_sub$stocking_density, gr_df_sub$label,
                             gr_df_sub$year, gr_df_sub$month), ]
labs <- gr_df_sub$month
p <- ggplot(gr_df_sub, aes(x=year_month, y=biomass, group=stocking_density))
p <- p + geom_line(aes(linetype=stocking_density))
p <- p + scale_linetype_manual(values=lines, guide=guide_legend(title="Stocking density"))
p <- p + facet_wrap(~label, nrow=2, scales="free")
p <- p + print_theme + scale_x_discrete(labels=labs)
p <- p + xlab("Month") + ylab("")
p <- p + theme(legend.key=element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3.7, "line"))
# p <- p + theme(legend.margin=unit(-0.1,"cm")) 
p <- p + theme(legend.position="bottom")
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/n_mult_start_2013"
pngname <- paste(imgpath, "New_growth_live_biomass~stocking_density_restart_monthly.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# summarize individual livweight gain
atest <- gr_df[gr_df$label == "Indiv. liveweight_gain (kg/ha)", ]
atest_agg <- aggregate(biomass~stocking_density, data=atest, FUN=mean)
write.csv(atest, "C:/Users/Ginger/Desktop/fuckit.csv", row.names=FALSE)  # reshape by hand in Excel
t_res <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/n_mult_start_2013/fuckit.csv")
t_res$high_minus_low <- t_res$indiv_gain_0.8 - t_res$indiv_gain_0.1
NROW(t_res[t_res$high_minus_low > 0 & !is.na(t_res$high_minus_low), ])  # majority of months

# diff between high and low density treatments in % green
emp_months <- c("2015_04", "2015_11", "2015_05", "2014_11", "2015_09", "2015_06", "2015_02", "2015_03", "2015_12")
low_sd <- gr_df[which(gr_df$stocking_density == '0.1' &
                        gr_df$label == 'Percent green biomass (%)'), 
                c('biomass', 'label', 'year', 'month', 'year_month',
                  'stocking_density')]
colnames(low_sd) <- c('perc_green_low', 'label', 'year', 'month', 'year_month',
                      'stocking_density')

high_sd <- gr_df[which(gr_df$stocking_density == '0.8' &
                        gr_df$label == 'Percent green biomass (%)'), 
                c('biomass', 'label', 'year', 'month', 'year_month',
                  'stocking_density')]
colnames(high_sd) <- c('perc_green_high', 'label', 'year', 'month', 'year_month',
                      'stocking_density')

comb <- merge(low_sd, high_sd, by=c('label', 'year', 'month', 'year_month'))
comb$diff <- comb$perc_green_high - comb$perc_green_low
comb <- comb[which(comb$year_month %in% emp_months), ]
comb <- comb[order(comb$year, comb$month), ]
labs <- comb$month
p <- ggplot(comb, aes(x=year_month, y=diff))
p <- p + geom_point()
p <- p + scale_x_discrete(labels=labs) + xlab("")
p <- p + ylab("Difference in % green") + print_theme
p <- p + geom_abline(intercept=0, slope=0, linetype=2)
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/n_mult_start_2013/diff_perc_green_high_low_sd.png"
png(file=pngname, units="in", res=300, width=2.1, height=1.725)
print(p)
dev.off()

# messing around, extra results for MS
lines <- c("solid", "dotted", "longdash")
growth_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/summary_old_herb_csv_old_sd.csv"
gr_df <- read.csv(growth_file)
gr_df$stocking_density <- as.factor(gr_df$stocking_density)
perc_live <- gr_df[which(gr_df$label == 'live_biomass'), 'biomass'] / 
  gr_df[which(gr_df$label == 'total_biomass'), 'biomass']
subs <- gr_df[which(gr_df$label == 'live_biomass'), ]
subs$biomass <- perc_live * 100

p <- ggplot(subs, aes(x=month, y=biomass, group=stocking_density))
p <- p + geom_line(aes(linetype=stocking_density))
p <- p + scale_linetype_manual(values=lines, guide=guide_legend(title="Stocking density"))
p <- p + xlab("Month") + ylab("% live biomass")
p <- p + ggtitle("Old herb params")
print(p)
imgpath <- "C:/Users/Ginger/Desktop"
pngname <- paste(imgpath, "perc_live_old_herb_no_restart.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=6)
print(p)
dev.off()

p <- ggplot(gr_df_sub, aes(x=month, y=biomass, group=stocking_density))
p <- p + geom_line(aes(linetype=stocking_density))
p <- p + scale_linetype_manual(values=lines, guide=guide_legend(title="Stocking density"))
p <- p + facet_wrap(~label, nrow=2, scales="free")
p <- p + print_theme + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + xlab("Month") + ylab("")
p <- p + theme(legend.key=element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3.7, "line"))
p <- p + theme(legend.margin=unit(-0.7,"cm")) 
p <- p + theme(legend.position="bottom")
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/stocking_density_new_growth/n_mult"
pngname <- paste(imgpath, "New_growth_live_biomass~stocking_density_restart_monthly.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

## write marginal table for input to optimizer
write_marginal_table <- function(outerdir, sd_table, save_as){
  sd_df <- read.csv(sd_table)
  folders <- list.files(outerdir)
  rows = length(folders)
  marginal_table <- data.frame('subbasin'=numeric(rows),
                               'animal'=character(rows), 'density'=character(rows),
                               'perc_gain'=numeric(rows), 'total_delta_weight_kg'=
                                 numeric(rows), stringsAsFactors=FALSE)
  for(i in 1:length(folders)){
    folder <- folders[i]
    subbasin <- substr(unlist(strsplit(folder, "_"))[1], 2, 3)
    anim <- unlist(strsplit(folder, "_"))[2]
    density <- unlist(strsplit(folder, "_"))[3]
    sd <- sd_df[which(sd_df$animal_level == paste(anim, density, sep="_")),
                'stocking_density']
    summary <- read.csv(paste(outerdir, folder, 'summary_results.csv', sep="/"),
                        stringsAsFactors=FALSE)
    summary[, paste(anim, '_kg', sep="")] = as.numeric(
      summary[, paste(anim, '_kg', sep="")])
    summary[, paste(anim, '_gain_kg', sep="")] <- as.numeric(
      summary[, paste(anim, '_gain_kg', sep="")])
    start_wt <- summary[1, paste(anim, '_kg', sep="")] -
      summary[1, paste(anim, '_gain_kg', sep="")]
    end_wt <- summary[dim(summary)[1], paste(anim, '_kg', sep="")]
    if (is.na(end_wt)){
      r <- 1
        while(is.na(end_wt)){
          end_wt <- summary[dim(summary)[1] - r, paste(anim, '_kg', sep="")]
          r <- r + 1
        }
    }
    delta_wt <- end_wt - start_wt
    delta_wt_herd <- delta_wt * sd
    perc_gain <- (delta_wt / start_wt) * 100
    marginal_table[i, ] <- c(subbasin, anim, density, perc_gain,
                             delta_wt_herd)
  }
  write.csv(marginal_table, save_as, row.names=FALSE)
}

outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/raw_5.2.16"
sd_table <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Stocking_density_table.csv"
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_table_7.26.16.csv"
write_marginal_table(outerdir, sd_table, save_as)

marginal_table <- read.csv(save_as)
anova_fit <- aov(perc_gain ~ soil_zone + clim_zone + animal + density, data=marginal_table)
summary(anova_fit)

lm_fit <- lm(perc_gain ~ soil_zone + clim_zone + animal + density, data=marginal_table)
summary(lm_fit)

################ forage model results for SWAT
SWAT_inputs <- function(outerdir, sd_table, swat_dir, save_as, known_rows){
  sd_df <- read.csv(sd_table)
  folders <- list.files(outerdir)
  valid_folders <- c()
  for(folder in folders){
    if(file.exists(paste(outerdir, folder, 'summary_results.csv', sep="/"))){
      summary <- read.csv(paste(outerdir, folder, 'summary_results.csv', sep="/"),
                          stringsAsFactors=FALSE)
      rows <- dim(summary)[1]
      if(rows == known_rows){
        valid_folders <- c(valid_folders, folder)
      }
    }
  }
  rows = length(valid_folders)
  marginal_table <- data.frame('subbasin'=numeric(rows), 'animal'=character(rows),
                               'density'=character(rows), 'perc_gain'=numeric(rows), 
                               'total_delta_weight_kg'= numeric(rows),
                                stringsAsFactors=FALSE)
  for(i in 1:length(valid_folders)){
    folder <- valid_folders[i]
    basin <- substr(unlist(strsplit(folder, "_"))[1], 2, 3)
    anim <- unlist(strsplit(folder, "_"))[2]
    density <- unlist(strsplit(folder, "_"))[3]
    sd <- sd_df[which(sd_df$animal_level == paste(anim, density, sep="_")),
                'stocking_density']
    summary <- read.csv(paste(outerdir, folder, 'summary_results.csv', sep="/"),
                        stringsAsFactors=FALSE)
    summary[, paste(anim, '_intake_forage_per_indiv_kg', sep="")] = as.numeric(
            summary[, paste(anim, '_intake_forage_per_indiv_kg', sep="")])
    summary[, paste(anim, '_kg', sep="")] = as.numeric(
      summary[, paste(anim, '_kg', sep="")])
    summary[, paste(anim, '_gain_kg', sep="")] <- as.numeric(
      summary[, paste(anim, '_gain_kg', sep="")])
    num_y = length(unique(summary$year))
    rows = num_y * 12
    SWAT_input_table <- data.frame('year'=numeric(rows), 'month'=numeric(rows),
                                   'day_initiated'=numeric(rows), 'grz_days'=numeric(rows),
                                   'bio_eat'=numeric(rows), 'manure_kg'=numeric(rows))
    start_wt <- summary[1, paste(anim, '_kg', sep="")] - summary[1, paste(anim, '_gain_kg', sep="")]
    total_gain = 0
    for(year in 1:num_y){
        end_wt <- summary[(year * 12), paste(anim, '_kg', sep="")]
        if (is.na(end_wt)){
          r <- 1
            while(is.na(end_wt)){
              end_wt <- summary[(year * 12) - r, paste(anim, '_kg', sep="")]
              r <- r + 1
            }
        }
        total_gain = total_gain + (end_wt - start_wt)
    }
    delta_wt = total_gain / num_y
    perc_gain = (delta_wt / start_wt) * 100
    delta_wt_herd = delta_wt * sd
    marginal_table[i, ] <- c(basin, anim, density, perc_gain, delta_wt_herd)
    r <- 1
    for(year in unique(summary$year)){
        for(month in 1:12){
            if(month %in% c(1,3,5,7,8,10,12)){
                num_days = 31
            }
            else if(month %in% c(4,6,9,11)){
                num_days = 30
            }
            else{
                num_days = 29.5  # TODO how does SWAT treat leap year?
            }
            bio_eat = summary[which(summary$year == year & summary$month == month), 
                              paste(anim, '_intake_forage_per_indiv_kg', sep="")] * sd / 30.4
            manure_kg = (bio_eat / 2.5) * 2.27 * 0.3
            SWAT_input_table[r, ] <- c(year, month, 1, num_days, bio_eat, manure_kg)
            r <- r + 1
        }
    }
    SWAT_table_name <- paste(swat_dir, paste("subbasin", basin, anim, density,
                             'SWAT_inputs.csv', sep="_"), sep="/")
    write.csv(SWAT_input_table, SWAT_table_name, row.names=FALSE)
  }
  write.csv(marginal_table, save_as, row.names=FALSE)
}

outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/raw_5.2.16"
save_as  <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_5.2.16.csv"
sd_table <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Stocking_density_table.csv"
swat_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/SWAT_inputs_5.2.16"
known_rows <- 204
SWAT_inputs(outerdir, sd_table, swat_dir, save_as, known_rows)

################# facilitation tests
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/test_segregation2.csv"
df <- read.csv(sum_csv)
df$total_biomass <- as.factor(df$total_biomass)
biomass_levels <- levels(df$total_biomass)[c(1:4, 8:10)]
df$cp_mean <- as.factor(df$cp_mean)
cp_levels <- levels(df$cp_mean)[c(1:3, 10)]
sub <- subset(df, df$abundance_ratio == 10
              & df$cp_ratio == 10 & df$abundance_cp_same == 0
              & df$cp_mean %in% cp_levels)
              # & df$total_biomass %in% biomass_levels)
              # )
p <- ggplot(sub, aes(x=total_biomass, y=segregation, group=cp_mean))
p <- p + geom_line(aes(colour=cp_mean))
p <- p + xlab('total biomass (kg / ha)')
print(p)
p <- ggplot(sub, aes(x=cp_mean, y=segregation, group=total_biomass))
p <- p + geom_line(aes(colour=total_biomass))
print(p)

p <- ggplot(df, aes(x=total_biomass, y=segregation, group=weight_1))
p <- p + geom_point(aes(colour=weight_1))
print(p)
p <- ggplot(df, aes(x=weight_1, y=segregation))
p <- p + geom_point()
print(p)


################# empirical stocking density test
fig_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test/Figures"

animal_type_list <- list()
outerdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test/Animal_type_test'
for(idx in 1:4){
  folder <- paste(outerdir, paste('outputs_', idx, sep=""), sep="/")
  sfile <- paste(folder, 'summary_results.csv', sep="/")
  summary <- as.data.frame(read.csv(sfile, header=TRUE, stringsAsFactors=FALSE))
  animal_type_list[[idx]] <- summary
}
df <- do.call(rbind, animal_type_list)
df$total_biomass = df$Research_green_kgha + df$Research_dead_kgha

initial_weight <- list()
initial_weight[['bull']] <- 511.8
initial_weight[['cow']] <- 381.4
initial_weight[['steer']] <- 447.5
initial_weight[['weaner']] <- 209.7
  
df$animal_type <- as.factor(df$animal_type)
num_types <- length(levels(df$animal_type))
animal_type <- c()
offtake <- c()
gain <- c()
for(i in 1:length(levels(df$animal_type))){
  type <- levels(df$animal_type)[i]
  subset <- df[df$animal_type == type, ]
  offtake_t <- sum(subset$total_offtake)
  gain_t <- subset[subset$step == 10, 'weight_kg'] - initial_weight[[type]]
  animal_type <- c(animal_type, type)
  offtake <- c(offtake, offtake_t)
  gain <- c(gain, gain_t)
}
sum_df <- data.frame(animal_type, offtake, gain)
max_underestimation_offtake <- (max(sum_df$offtake) - min(sum_df$offtake))/min(sum_df$offtake)
max_underestimation_gain <- (max(sum_df$gain) - min(sum_df$gain))/min(sum_df$gain)
max_overestimation_offtake <- (max(sum_df$offtake) - min(sum_df$offtake))/max(sum_df$offtake)
max_overestimation_gain <- (max(sum_df$gain) - min(sum_df$gain))/max(sum_df$gain)

gain <- ggplot(df, aes(x=step, y=total_biomass))
gain <- gain + geom_line(size=1.3)
gain <- gain + geom_line(aes(x=step, y=total_offtake))
gain <- gain + geom_line(aes(x=step, y=weight_kg))
gain <- gain + facet_wrap(~ animal_type)
gain <- gain + print_theme #+ scale_y_continuous(limits=c(0, max(livestock_df$percent_consumed)))
gain <- gain + theme(plot.title=element_text(size=12), legend.text=element_text(size=12), 
                     axis.title=element_text(size=12), legend.title=element_blank(),
                     legend.key = element_blank())
pngname <- paste(fig_dir, paste("biomass_gain_", site, ".png", sep=""), sep = "/")
png(file = pngname, units="in", res = 150, width=7, height=4)
print(gain)
dev.off() 

#########################
livestock_df_list <- list()
outerdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test'
site_list <- c('Kamok', 'Loidien', 'Rongai', 'Research')
for(n_folder in c('DMD_0.5_CP_0.06', 'DMD_0.64_CP_0.1', 'Calculated_from_CENTURY')){
  for(site in site_list){
    dir <- paste(outerdir, n_folder, site, sep='/')
    sfile <- paste(dir, 'summary_results.csv', sep='/')
    summary <- as.data.frame(read.csv(sfile, header=TRUE, stringsAsFactors=FALSE))
    summary$site <- site
    summary$nutrient_level <- n_folder
    factor <- paste(site, n_folder, sep="-")
    livestock_df_list[[factor]] <- summary
  }
}
livestock_df <- do.call(rbind, livestock_df_list)

livestock_df$nutrient_level <- as.factor(livestock_df$nutrient_level)
livestock_df$date <- paste('01', livestock_df$month, livestock_df$year, sep="-")
livestock_df$date <- as.Date(livestock_df$date, format="%d-%m-%Y")
cols = c('Steers_gain_kg', 'Cows_gain_kg', 'Bulls_gain_kg', 'Heifers_gain_kg',
         'Calves_gain_kg', 'steer.heifer_gain_kg')
for(col in cols){
  livestock_df[, col] = as.numeric(livestock_df[, col])
}
livestock_df$total_biomass = livestock_df$Research_green_kgha
                              + livestock_df$Research_dead_kgha
livestock_df$total_gain = rowSums(livestock_df[, cols], na.rm=TRUE)
for(row in 1:dim(livestock_df)[1]){
  if (all(is.na(livestock_df[row, cols]))){
    livestock_df[row, 'total_gain'] <- NA
  }
}
livestock_df$animals <- livestock_df$stocking_density * 1600
livestock_df$gain_per_animal <- livestock_df$total_gain / livestock_df$animals

cbPalette <- c("#999999", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00", "#009E73", "#F0E442", "#0072B2")

for(site in site_list){
# site <- 'Research'
  restr <- livestock_df[livestock_df$site == site
                        & livestock_df$nutrient_level == 'DMD_0.5_CP_0.06', ]
  gain <- ggplot(restr, aes(x=date, y=total_biomass))
  gain <- gain + geom_line(size=1.3)
  gain <- gain + geom_line(aes(x=date, y=total_offtake))
  gain <- gain + geom_point(aes(x=date, y=gain_per_animal))
                           #size=1.3), linetype='dotted')
  gain <- gain + print_theme #+ scale_y_continuous(limits=c(0, max(livestock_df$percent_consumed)))
  gain <- gain + theme(plot.title=element_text(size=12), legend.text=element_text(size=12), 
                         axis.title=element_text(size=12), legend.title=element_blank(),
                         legend.key = element_blank())
  pngname <- paste(fig_dir, paste("biomass_gain_", site, ".png", sep=""), sep = "/")
  png(file = pngname, units="in", res = 150, width=7, height=4)
  print(gain)
  dev.off() 
}

for(site in site_list){
  restr <- livestock_df[livestock_df$site == site, ]
  biomass <- ggplot(restr, aes(x=date, y=total_biomass, group=nutrient_level, colour=nutrient_level))
  biomass <- biomass + geom_line(size=1.3)
  biomass <- biomass + geom_line(aes(x=date, y=total_offtake, group=nutrient_level, colour=nutrient_level))
  biomass <- biomass + print_theme
  pngname <- paste(fig_dir, paste("biomass_", site, ".png", sep=""), sep = "/")
  png(file = pngname, units="in", res = 150, width=7, height=4)
  print(biomass)
  dev.off() 
}

########### is animal density greater at veg transects?
weather_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/From_Sharon_5.29.15/Matched_GPS_records/Matched_with_weather_stations"
veg_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/From_Sharon_5.29.15/Matched_GPS_records/Matched_with_veg_transects"

density_list <- list()
files <- list.files(weather_dir, pattern='^average_animals')
for(file in files){
  data <- read.csv(paste(weather_dir, file, sep="/"))
  site <- substr(file, 17, 30)
  data$site <- site
  data$type <- 'weather_station'
  density_list[[site]] <- data
}
files <- list.files(veg_dir, pattern='^average_animals')
for(file in files){
  data <- read.csv(paste(veg_dir, file, sep="/"))
  site <- substr(file, 17, 27)
  data$site <- site
  data$type <- 'veg_transect'
  density_list[[site]] <- data
}
density_df <- do.call(rbind, density_list)
density_df[is.na(density_df)] <- 0
means <- aggregate(density_df, by=list(site=density_df$site), FUN=mean, na.rm=TRUE)
means$type <- 'weather_station'

means$total <- means$Bulls + means$Cows + means$Calves + means$Heifers + 
               means$Steers + means$Weaners + means$steer.heifer

bplot <- ggplot(means, aes(x=type, y=total)) + geom_boxplot()
bplot <- bplot + print_theme
print(bplot)

# only the weather stations I simulated
subset <- means[(means$type == 'veg_transect'), ]
subset2 <- means[c(4,5,14,15), ]
subset <- rbind(subset, subset2)
bplot <- ggplot(subset, aes(x=type, y=total)) + geom_boxplot()
bplot <- bplot + print_theme + xlab('') + ylab('Total animals in 16 square km')
print(bplot)

pngname <- paste(fig_dir, "animals_veg_transect_vs_weather_station.png", sep = "/")
png(file = pngname, units="in", res = 150, width=5, height=4)
print(bplot)
dev.off()

#### weight gain consequences 8.3.16
gain_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/diet_summary.csv"
gain_df <- read.csv(gain_csv)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/scatter_matrix.png"
png(file=pngname, units="in", res=150, width=8, height=8)
pairs(gain_df[c(12,10,11,4,5,1,2,8)], upper.panel=NULL)
dev.off()

# exploratory scatterplots
p <- ggplot(gain_df, aes(x=total_biomass, y=gain_kg_one_month))
p <- p + geom_point() + xlab("Total biomass (g/m2)") + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/total_biomass_v_gain.png"
png(file=pngname, units="in", res=150, width=3, height=3)
print(p)
dev.off()

p <- ggplot(gain_df, aes(x=Green_biomass, y=gain_kg_one_month))
p <- p + geom_point() + xlab("Green biomass (g/m2)") + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/green_biomass_v_gain.png"
png(file=pngname, units="in", res=150, width=3, height=3)
print(p)
dev.off()

p <- ggplot(gain_df, aes(x=percent_green, y=gain_kg_one_month))
p <- p + geom_point() + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/percent_green_v_gain.png"
png(file=pngname, units="in", res=150, width=3, height=3)
print(p)
dev.off()

p <- ggplot(gain_df, aes(x=total_intake, y=gain_kg_one_month))
p <- p + geom_point() + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/total_intake_v_gain.png"
png(file=pngname, units="in", res=150, width=3, height=3)
print(p)
dev.off()

p <- ggplot(gain_df, aes(x=total_intake, y=total_intake))
p <- p + geom_point() + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/total_intake_v_gain.png"
png(file=pngname, units="in", res=150, width=3, height=3)
print(p)
dev.off()

p <- ggplot(gain_df, aes(x=factor(Ecological_Classification), y=gain_kg_one_month,
                         group=Year))
p <- p + geom_point(aes(shape=factor(Year)))
p <- p + xlab("Ecological classification") + ylab("Gain in one month (kg)")
p <- p + print_theme
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/weight_gain_consequences/figs/gain_summary__.png"
png(file=pngname, units="in", res=150, width=5, height=3)
print(p)
dev.off()

