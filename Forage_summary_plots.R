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
  
  biomass_subset <- summary_df[(summary_df$label %in% c('total_grass', animal_types)), ]
  p <- ggplot(biomass_subset, aes(x=month, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("biomass (kg)") + ggtitle("Total biomass")
  pngname <- paste(imgpath, "Biomass_summary.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  grass_biomass_subset <- summary_df[(summary_df$label %in% c('total_grass', grass_types)), ]
  p <- ggplot(grass_biomass_subset, aes(x=month, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("biomass (kg)") + ggtitle("Grass biomass")
  pngname <- paste(imgpath, "Grass_biomass.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  animal_biomass_subset <- summary_df[(summary_df$label %in% animal_types), ]
  p <- ggplot(animal_biomass_subset, aes(x=month, y=biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("biomass (kg)") + ggtitle("Herbivore biomass")
  p <- p + scale_linetype_manual(values = lines, name = "")
  pngname <- paste(imgpath, "Herbivore_biomass.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
  
  p <- ggplot(animal_biomass_subset, aes(x=month, y=offtake, group=label, linetype=label))
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
                          'label'=rep(label, length(summary_file$step)),
                          'proportion_biomass'=prop)
    prop_list[[label]] <- prop_df
  }
  prop_df <- do.call(rbind, prop_list)
  
  p <- ggplot(prop_df, aes(x=month, y=proportion_biomass, group=label, linetype=label))
  p <- p + geom_line() + ylab("percentage of total biomass") + 
    ggtitle("Proportion of total biomass by grass type")
  p <- p + scale_linetype_manual(values = lines, name = "")
  pngname <- paste(imgpath, "Grass_proportion.png", sep="/")
  png(file=pngname, units="in", res=300, width=7.5, height=5)
  print(p)
  dev.off()
}

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

# new growth
# an example file structure
outer_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs"
run_folder <- "herb1_equal_sd_opposite_grass"
grasses <- c("grass1", "grass2")
summary_df <- data.frame('month'=c(1:12), 'context'=rep(c, 12),
                         'treatment'=rep(t, 12), 'grass'=rep('total', 12),
                         'new_growth'=numeric(12), 'perc_new_growth'=numeric(12))
new_growth_t <- rep(0, 12)
biomass_t <- rep(0, 12)
for(gr in grasses){
  file <- paste(outer_dir, run_folder, cent_dir, paste(gr, '.lis', sep=""), sep="/")
  gr_df <- read.table(file, header=TRUE)
  year <- 2015
  y_sub <- subset(gr_df, gr_df$time > year & gr_df$time <= year + 1)[, c('time', 'agcacc', 'aglivc')]
  y_sub <- y_sub[!duplicated(y_sub), ]
  growth_v <- as.vector(y_sub[, 'agcacc'])
  biomass_v <- as.vector(y_sub[, 'aglivc'])
  new_growth <- growth_v[1]
  for(idx in c(2:12)){
    new_growth <- c(new_growth, growth_v[idx] - growth_v[idx - 1])
  }
  new_growth_t <- new_growth_t + new_growth
  biomass_t <- biomass_t + biomass_v
}
new_gr_perc_t <- new_growth_t / biomass_t

# calculated in script
lines <- c("solid", "dotted", "longdash")
growth_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/model_runs/sd_new_growth/cattle_new_growth_summary.csv"
gr_df <- read.csv(growth_file)
gr_df$stocking_density <- as.factor(gr_df$stocking_density)
gr_df$label <- factor(gr_df$label, levels=c("total_biomass", "new_growth", "perc_new_growth"),
                      labels=c("Total biomass", "New growth", "New growth / total biomass"))
gr_df_sub <- subset(gr_df, gr_df$stocking_density %in% c("0.1", "0.75", "1.25"))
p <- ggplot(gr_df_sub, aes(x=month, y=biomass, group=stocking_density))
p <- p + geom_line(aes(linetype=stocking_density))
p <- p + scale_linetype_manual(values=lines, guide=guide_legend(title="Stocking density"))
p <- p + facet_wrap(~label, nrow=2, scales="free")
p <- p + print_theme + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + ylab("") + theme(legend.key = element_blank())
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/facilitation_exploration/figs"
pngname <- paste(imgpath, "Fig10_New_growth~stocking_density.png", sep="/")
png(file=pngname, units="in", res=300, width=6, height=5)
print(p)
dev.off()

## write marginal table for input to optimizer
write_marginal_table <- function(outerdir, sd_table, save_as){
  sd_df <- read.csv(sd_table)
  folders <- list.files(outerdir)
  rows = length(folders)
  marginal_table <- data.frame('soil_zone'=numeric(rows), 'clim_zone'=numeric(rows),
                               'animal'=character(rows), 'density'=character(rows),
                               'perc_gain'=numeric(rows), 'total_delta_weight_kg'=
                                 numeric(rows), stringsAsFactors=FALSE)
  for(i in 1:length(folders)){
    folder <- folders[i]
    s_zone <- substr(unlist(strsplit(folder, "_"))[1], 2, 2)
    c_zone <- substr(unlist(strsplit(folder, "_"))[2], 2, 2)
    anim <- unlist(strsplit(folder, "_"))[3]
    density <- unlist(strsplit(folder, "_"))[4]
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
    marginal_table[i, ] <- c(s_zone, c_zone, anim, density, perc_gain,
                             delta_wt_herd)
  }
  write.csv(marginal_table, save_as, row.names=FALSE)
}

outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/raw_4.11.16"
sd_table <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Stocking_density_table.csv"
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_table_4.11.16.csv"
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