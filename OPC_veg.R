## what is the variability in biomass within and between transects at OPC?
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

coeff_var <- function(values){
  cv <- sd(values) / mean(values) * 100
  return(cv)
}

outdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/output/'

Jenny_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Jenny_biomass_reshaped.txt"
Sharon_file = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Vegetation data_July-August_2014_biomass.txt"

cp_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/CP_combined.txt"

## Jenny's crude protien data
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
