# Plot output data from CENTURY
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

read_CENTURY_results <- function(datadir, origdir, sites) {
  outvars <- read.table(paste(origdir, '/outvars.txt', sep = ""), header = FALSE)
  widths <- rep(16, length(outvars[, 1]) + 1)
  col_names = read.table(paste(datadir, '/', sites[1], '.lis', sep = ""), nrow = 1, as.is = TRUE)
  col_names <- gsub("\\(", ".", col_names)
  col_names <- gsub(")", ".", col_names)
  
  site_list <- list()
  for (site in sites){
    data <- read.fwf(paste(datadir, '/', site, '.lis', sep = ""), widths, skip = 2)
    colnames(data) <- col_names
    data$site <- site
    data$standing_live <- data$aglivc * 2.5
    data$standing_dead <- data$stdedc * 2.5
    data$total_biomass <- data$standing_live + data$standing_dead
    data$cp_live <- (data$aglive.1. * 6.25) / data$standing_live
    data$cp_dead <- (data$stdede.1. * 6.25) / data$standing_dead
    data$cp_overall <- ((data$cp_live * data$standing_live) + 
                          (data$cp_dead * data$standing_dead)) / data$total_biomass
    data$percent_green <- data$aglivc / data$total_biomass
    # data$month <- seq(1, dim(data)[1])
    site_list[[site]] <- data
  }
  sites_df <- do.call(rbind, site_list)
  return(sites_df)
}

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

cbPalette <- c("#999999", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00", "#009E73", "#F0E442", "#0072B2")

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/identical_management/CENTURY_files'
origdir <-  'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Century46_PC_Jan-2014' 
outdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/output'
sites <- c('W06_id', 'W3_id', 'MO_id') #'W06_1', 'W06_2', 'W06_3', 'W06_4', 'W06_5')

sites_df <- read_CENTURY_results(datadir, origdir, sites)

sites_df$site <- factor(sites_df$site, levels=c('W06_id', 'W3_id', 'MO_id'), labels = c('W06', 'W3', 'MO')) #'W06', 'W06_1', 'W06_2', 'W06_3', 'W06_4', 'W06_5'),
                        # labels=c('actual', '90% sand', '90% clay', '90% silt', '0.1 bulkd', '3 bulkd'))

restrict_dates <- c(2000, 2015)

# look at biomass at each site
restrict_time <- sites_df[sites_df$time >= restrict_dates[1] & sites_df$time <= restrict_dates[2], c('time', 'site', 'standing_live', 'total_biomass')]
live_combined <- ggplot(restrict_time, aes(x = time, y = standing_live, group = site, colour = site))
live_combined <- live_combined + geom_line(size = 1.3) + scale_y_continuous(limits=c(0, 400)) # + print_theme #+ scale_x_continuous(limits = c(2011.8, 2015.2))
live_combined <- live_combined + xlab('date') + ylab('Live biomass (g per sq m)') + ggtitle('Live biomass: Three sites, identical management')
live_combined <- live_combined + scale_colour_manual(values=cbPalette)
live_combined <- live_combined + theme(plot.title=element_text(size=18), legend.text=element_text(size=18), 
                                       axis.title=element_text(size=18), legend.title=element_blank(), legend.key = element_blank())
print(live_combined)

# look at crude protein variation
restrict_time <- sites_df[sites_df$time >= restrict_dates[1] & sites_df$time <= restrict_dates[2], c('time', 'site', 'cp_live', 'cp_dead', 'cp_overall', 'standing_live', 'standing_dead')]
restrict_time$year <- floor(restrict_time$time)
cent_cv_df <- aggregate(restrict_time$cp_overall, by = list(restrict_time$year), FUN = coeff_var)

cp <- ggplot(restrict_time, aes(time))
cp <- cp + geom_line(aes(y = cp_overall, colour = 'cp_overall'), size = 1.3)
cp <- cp + print_theme
cp <- cp + ylab('Crude protein (percent)') + ggtitle('Total crude protein') + scale_y_continuous(limits=c(0, 0.2)) 
cp <- cp + geom_hline(aes(yintercept=0.08)) # approximate cutoff for limitation on intake by protein
print(cp)

# look at crude protein and biomass, percent green
restrict_time <- sites_df[sites_df$time >= 2000 & sites_df$time <= 2014, c('time', 'site', 'cp_live', 'cp_dead', 'cp_overall', 'total_biomass', 'percent_green')]
restrict_time$cp_overall_sc <- restrict_time$cp_overall / max(restrict_time$cp_overall)
restrict_time$perc_green_sc <- restrict_time$percent_green / max(restrict_time$percent_green)
restrict_time$biomass_sc <- restrict_time$total_biomass / max(restrict_time$total_biomass)

# line plots
cp <- ggplot(restrict_time, aes(time))
cp <- cp + geom_line(aes(y = cp_overall_sc, colour = 'cp_overall_sc'), size = 1.3)
cp <- cp + geom_line(aes(y = biomass_sc, colour = 'biomass_sc'), size = 1.3)
cp <- cp + print_theme
cp <- cp + ylab('Percent maximum') + ggtitle('Total crude protein, total biomass') #+ scale_y_continuous(limits=c(0, 0.2)) 
print(cp)

cp <- ggplot(restrict_time, aes(time))
cp <- cp + geom_line(aes(y = cp_overall_sc, colour = 'cp_overall_sc'), size = 1.3)
cp <- cp + geom_line(aes(y = perc_green_sc, colour = 'perc_green_sc'), size = 1.3)
cp <- cp + print_theme
cp <- cp + ylab('Percent maximum') + ggtitle('Total crude protein, greenness') #+ scale_y_continuous(limits=c(0, 0.2)) 
print(cp)

# what's the correlation between crude protein and biomass, and greenness?
bio_lm <- lm(cp_overall_sc ~ biomass_sc, data = restrict_time)
gre_lm <- lm(cp_overall_sc ~ perc_green_sc, data = restrict_time)

# scatterplots
cp <- ggplot(restrict_time, aes(x = biomass_sc, y = cp_overall_sc))
cp <- cp + geom_point()
cp <- cp + print_theme
cp <- cp + xlab('scaled biomass') + ylab('scaled crude protein') + ggtitle('Crude protein concentration v total biomass') #+ scale_y_continuous(limits=c(0, 0.2)) 
cp <- cp + geom_abline(intercept = 0.95946, slope = -0.31370)
cp <- cp + annotate("text", label = "R^2 == 0.46", x = 0.9, y = 1, size = 4, parse = TRUE)
cp <- cp + theme(plot.title = element_text(size=9))
print(cp)

pngname <- paste(outdir, "cp_biomass.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(cp)
dev.off()

cp <- ggplot(restrict_time, aes(x = perc_green_sc, y = cp_overall_sc))
cp <- cp + geom_point()
cp <- cp + print_theme
cp <- cp + xlab('scaled greenness') + ylab('scaled crude protein') + ggtitle('Crude protein concentration v greenness') #+ scale_y_continuous(limits=c(0, 0.2)) 
cp <- cp + geom_abline(intercept = 0.62801, slope = 0.17377)
cp <- cp + annotate("text", label = "R^2 == 0.07", x = 0.48, y = 1, size = 4, parse = TRUE)
cp <- cp + theme(plot.title = element_text(size=9))
print(cp)

pngname <- paste(outdir, "cp_greenness.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(cp)
dev.off()

## find maximum difference between sites for each time step
diff_table <- as.data.frame(matrix(NA, nrow = length(unique(restrict_time$date)), ncol = 4))
colnames(diff_table) <- c('time', 'max_diff', 'min_value', 'perc_of_min')
i <- 0
for (row in unique(restrict_time$date)){
  values <- restrict_time[restrict_time$date == row, 'mm_prec']
  max_diff <- max(values) - min(values)
  perc <- max_diff / min(values) * 100
  diff_table[i, 'time'] <- row
  diff_table[i, 'max_diff'] <- max_diff
  diff_table[i, 'min_value'] <- min(values)
  diff_table[i, 'perc_of_min'] <- perc
  i <- i + 1
}

p <- ggplot(diff_table, aes(time)) + 
  geom_line(aes(y = max_diff, colour = "red"), size = 1.3)
# p <- p + geom_line(aes(y = mean, colour = "mean"), size = 1.3)
p <- p + print_theme #+ scale_x_continuous(limits = c(2011.8, 2015.2))
p <- p + xlab('date') + ylab('g per square m') + ggtitle('Maximum difference between sites: precipitation')
p <- p + theme(legend.position = "none", axis.text.x = element_blank())
# p <- p + scale_x_continuous(limits = c(2000, 2015), breaks=seq(2001, 2015, by = 2))
diff_plot_precip <- p
print(diff_plot_precip)
  
## make summary plot of precipitation
precip_summary <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Kenya/precipitation_summary.txt"
precip <- read.table(precip_summary, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
precip$Site <- as.factor(precip$Site)

precip$date <- as.Date(precip$Date, format = "%m/%d/%Y")
precip_sites <- c('kamok', 'research', 'rongai')
precip_rest <- precip[precip$Site %in% precip_sites, ] #, c('time', 'site', 'standing_live', 'total_biomass')]
precip <- precip_rest
p <- ggplot(precip, aes(x = date, y = mm_prec, group = Site, colour = Site)) + geom_line()
p <- p + xlab("date") + ylab("cm") + ggtitle("Monthly precipitation: Kamok")
p <- p + scale_x_date(breaks = "2 years", labels = date_format("%Y"),
             limits = as.Date(c('2000-01-01','2015-01-01')))
p <- p + print_theme
precip_plot <- p
print(precip_plot)

# plot rainfall and diff between sites together
pngname <- paste(outdir, 'identical_management', 'rainfall_and_diff.png', sep = '/')
png(file = pngname, units="in", res=300, width = 11, height=8)
grid.arrange(arrangeGrob(diff_plot + theme(legend.position="none"),
                         diff_plot_precip + theme(legend.position="none"),
                         nrow=2))
dev.off()

# random leftovers
live_b <- ggplot(sites_df, aes(x = month, y = standing_live)) + geom_line() + print_theme
live_b <- live_b + facet_wrap(~ site, nrow = length(sites), ncol = 1)
live_b <- live_b + scale_y_continuous(limits=c(50, 500))
live_b <- live_b + xlab('Sampling month') + ylab('Live biomass (g per sq m)') + ggtitle("CENTURY: live biomass")

live_bplot <- ggplot(restrict_time, aes(x = site, y = standing_live)) + geom_boxplot() + print_theme
live_bplot <- live_bplot + ggtitle("CENTURY: live biomass")
live_bplot <- live_bplot + scale_y_continuous(limits=c(50, 500))
live_bplot <- live_bplot + xlab('Site') + ylab('Live biomass (g per sq m)')

total <- ggplot(restrict_time, aes(x = time, y = total_biomass, group = site, linetype = site, colour = site)) + geom_line(size = 1.3)
total <- total + print_theme
#total <- total + scale_y_continuous(limits=c(50, 500))
total <- total + xlab('Time') + ylab('Total biomass (g per sq m)') + ggtitle("Total biomass: different grazing presure")
print(total)

## summary plots of soil characteristics at four sites
soil_summary <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Kenya/Jenny_sites_soil_summary.csv"
soil <- read.csv(soil_summary, header = TRUE)
soil$site <- as.factor(soil$site)
soil$attribute <- as.factor(soil$attribute)
composition <- soil[soil$attribute %in% c('clay', 'sand', 'silt'), ]

p <- ggplot(composition, aes(x = attribute, y = value, shape = site)) #+ geom_point()
p <- p + geom_jitter(position = position_jitter(width = 0.1), size = 3)
p <- p + xlab("Composition") + ylab("Percent") + print_theme
print(p)

bulk_d <- soil[soil$attribute == 'bulk_density', ]
p <- ggplot(bulk_d, aes(x = site, y = value)) #, shape = site)) #+ geom_point()
p <- p + geom_point(size = 3)
p <- p + scale_y_continuous(limits=c(0.75, 1.5))
p <- p + xlab("Site") + ylab("Bulk density") + print_theme
print(p)

ph <- soil[soil$attribute == 'ph', ]
p <- ggplot(ph, aes(x = site, y = value)) #, shape = site)) #+ geom_point()
p <- p + geom_point(size = 3)
p <- p + scale_y_continuous(limits=c(5, 9))
p <- p + xlab("Site") + ylab("pH") + print_theme
print(p)

######## CENTURY simulations following back-calculated management, vs Jenny's caged sites
file = 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/Calculated_management_reshape.txt'
sites <- c('M05', 'MO', 'W06', 'W3', 'GT', 'N4')
comparison <- read.table(file, header = TRUE)
comparison$site <- as.factor(comparison$site)
comparison <- comparison[which(comparison$site %in% sites), ]
comparison$source <- as.factor(comparison$source)
comparison$date <- as.Date(comparison$date, "%m/%d/%y")
p <- ggplot(comparison, aes(x = date, y = total_biomass, colour = source))
p <- p + geom_line(aes(group = source), size = 1) + geom_point() + facet_wrap(~ site, scales="free_x")
p <- p + ylab("Biomass (g per square m)") + xlab("Date") + scale_x_date(labels = date_format("%m/%d"))
p <- p + theme(legend.position = "bottom")
print(p)
pngname = 'C:/Users/Ginger/Desktop/CENTURY.png'
png(file=pngname, units="in", res=400, width=8, height=5.5)
print(p)
dev.off()
