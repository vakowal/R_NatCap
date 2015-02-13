# Plot output data from CENTURY
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Century46_PC_Jan-2014' 
outdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/output/'
outvars <- read.table(paste(datadir, '/outvars.txt', sep = ""), header = FALSE)
sites <- c('grass1', 'grass2') #'MO', 'GT', 'W06')

widths <- rep(16, length(outvars[, 1]) + 1)
col_names = read.table(paste(datadir, '/', sites[1], '.lis', sep = ""), nrow = 1, as.is = TRUE)
site_list <- list()
for (site in sites){
  data <- read.fwf(paste(datadir, '/', site, '.lis', sep = ""), widths, skip = 2)
  colnames(data) <- col_names
  data$site <- site
  data$standing_live <- data$aglivc * 2.5
  data$standing_dead <- data$stdedc * 2.5
  data$total_biomass <- data$standing_live + data$standing_dead
  data$percent_green <- data$aglivc / data$total_biomass
  # data$month <- seq(1, dim(data)[1])
  site_list[[site]] <- data
}
sites_df <- do.call(rbind, site_list)

#M05_diff <- read.table("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/M05-GH-none_difference.txt",
 #                      header = TRUE)
sites_df$site <- factor(sites_df$site) #, levels = c('M05', 'GT', 'MO', 'W06'))

restrict_time <- sites_df[sites_df$time >= 2012 & sites_df$time <= 2014, c('time', 'site', 'standing_live', 'total_biomass')]
# restrict_time <- rbind(restrict_time, M05_diff)
live_combined <- ggplot(restrict_time, aes(x = time, y = standing_live, group = site, linetype = site, colour = site))
live_combined <- live_combined + geom_line(size = 1.3) + scale_y_continuous(limits=c(0, 400)) + print_theme #+ scale_x_continuous(limits = c(2011.8, 2015.2))
live_combined <- live_combined + ylab('Live biomass (g per sq m)') + ggtitle('Live biomass: M05 high-intensity and no grazing')
print(live_combined)

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

towrite2 <- restrict_time[, c('site', 'time', 'standing_live', 'total_biomass')]
diff <- read.table("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/M05-W06-diff.txt", header = TRUE)

total <- ggplot(diff, aes(x = time, y = diff_total, group = site, linetype = site, colour = site)) + geom_line(size = 1.3)
total <- total + print_theme
#total <- total + scale_y_continuous(limits=c(50, 500))
total <- total + xlab('Time') + ylab('Total biomass (g per sq m)') + ggtitle("Difference: total biomass by site")
print(total)

live_combined <- ggplot(diff, aes(x = time, y = diff_live, group = site, linetype = site, colour = site))
live_combined <- live_combined + geom_line(size = 1.3) + print_theme #+ scale_x_continuous(limits = c(2011.8, 2015.2))
live_combined <- live_combined + ylab('Delta live biomass (g per sq m)') + ggtitle('Difference: live biomass by site')
print(live_combined)

total_bplot <- ggplot(sites_df, aes(x = site, y = total_biomass)) + geom_boxplot() + print_theme
total_bplot <- total_bplot + ggtitle("CENTURY: total biomass")
total_bplot <- total_bplot + scale_y_continuous(limits=c(50, 500))
total_bplot <- total_bplot + xlab('Site') + ylab('Total biomass (g per sq m)')

perc_green <- ggplot(sites_df, aes(x = month, y = percent_green)) + geom_line() + ggtitle("CENTURY: percent live biomass")
perc_green <- perc_green + facet_wrap(~ site, nrow = length(sites), ncol = 1) + print_theme
perc_green <- perc_green + scale_y_continuous(limits=c(0, 1))
perc_green <- perc_green + xlab('Sampling month') + ylab('Live biomass / total biomass')

print(live_b)
print(live_bplot)
print(total)
print(perc_green)

pngname <- paste(outdir, "M05-GH-M05-none_total.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 10, height = 6)
print(total)
dev.off()

pngname <- paste(outdir, "CENTURY_Jenny_live_biomass_boxplot.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(live_bplot)
dev.off()

pngname <- paste(outdir, "CENTURY_Jenny_total_biomass.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(total)
dev.off()

pngname <- paste(outdir, "CENTURY_Jenny_total_biomass_boxplot.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(total_bplot)
dev.off()

pngname <- paste(outdir, "CENTURY_Jenny_perc_green.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(perc_green)
dev.off()

## make summary plot of precipitation at the three sites used above
precip_summary <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Kenya/precipitation_summary.txt"
precip <- read.table(precip_summary, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
precip$Site <- as.factor(precip$Site)

precip$date <- as.Date(precip$Date, format = "%m/%d/%Y")
p <- ggplot(precip, aes(x = date, y = mm_prec, colour = Site)) + geom_line()
p <- p + xlab("Date") + ylab("Monthly precipitation (cm)")
p <- p + print_theme
print(p)

pngname <- paste(outdir, "Rainfall_kamok_research_rongai.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 10, height = 5)
print(p)
dev.off()

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
sites <- c('M05', 'MO', 'W06', 'W3')
comparison <- read.table(file, header = TRUE)
comparison$site <- as.factor(comparison$site)
comparison <- comparison[which(comparison$site %in% sites), ]
comparison$source <- as.factor(comparison$source)
comparison$date <- as.Date(comparison$date, "%m/%d/%y")
p <- ggplot(comparison, aes(x = date, y = total_biomass, colour = source))
p <- p + geom_line(aes(group = source), size = 1) + geom_point() + facet_wrap(~ site, scales="free_x")
p <- p + ylab("Biomass (g per square m)") + xlab("Date") #+ scale_x_date(labels = format.Date("%m/%d"))
print(p)
pngname = 'C:/Users/Ginger/Desktop/CENTURY.png'
png(file=pngname, units="in", res=400, width=7.75, height=5.5)
print(p)
dev.off()
