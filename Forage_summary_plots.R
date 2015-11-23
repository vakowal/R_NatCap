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

results_dir <- "C:/Users/Ginger/Documents/Python/Output"
results_folder <- "Forage_Model_140728-1412"
suffix <- gsub("Forage_Model_", "", results_folder)
imgpath <- paste(results_dir, results_folder, "/", sep = "/")

summary_file <- read.csv(paste(results_dir, results_folder, "summary.csv", sep = "/"), header = TRUE)
steps <- max(summary_file[, "Step"])

veg_plot <- ggplot(summary_file, aes(x = Step, y = Standing_veg)) + geom_line() + ylab("Standing vegetation (kg dry matter)") # + print_theme
livestock_plot <- ggplot(summary_file, aes(x = Step, y = Herd_avg_weight)) + geom_line() + ylab("Average herbivore weight (kg)") # + print_theme

pngname <- paste(imgpath, "Standing_veg.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=9)
print(veg_plot)
dev.off()

pngname <- paste(imgpath, "Herd_average_weight.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=9)
print(livestock_plot)
dev.off()

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