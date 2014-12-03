## what is the variability in biomass within and between transects at OPC?
library(ggplot2)

Jenny_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Jenny_biomass_reshaped.txt"
Sharon_file = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Vegetation data_July-August_2014_biomass.txt"

## Jenny's data
veg_data <- read.table(Jenny_file, header = TRUE, sep = "\t")
veg_data$Transect <- as.factor(veg_data$site)
veg_data$caged <- as.factor(veg_data$caged)
veg_data$habitat <- as.factor(veg_data$habitat)

p <- ggplot(veg_data, aes(x = Transect, y = DM_g_per_sq_m, colour = habitat)) + geom_boxplot()
p <- p + facet_wrap(~ caged, nrow = 2, ncol = 1)
p <- p + xlab('Site') + ylab('Dry matter (g per sq m)')
print(p)

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
