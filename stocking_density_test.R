# check out output from stocking density test
# what is the relationship between stocking density, aboveground and belowground
# biomass, and water in the soil and leaving the soil?

library(ggplot2)

sum_csv = "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test/sustainable_limit_test/summary.csv"
sumdf = read.csv(sum_csv)
sumdf$stocking_density_f = as.factor(sumdf$stocking_density)
sumdf$date_f = as.factor(sumdf$date)
sumdf$below_biomass_kg_ha <- sumdf$below_biomass_gm2 * 10
sumdf$root_shoot <- sumdf$above_total_biomass_kg_ha / sumdf$below_biomass_kg_ha

sd_levels = c(levels(sumdf$stocking_density_f)[1],
                  levels(sumdf$stocking_density_f)[4],
                  levels(sumdf$stocking_density_f)[7],
                  levels(sumdf$stocking_density_f)[10])
subset <- sumdf[which(sumdf$stocking_density_f %in% sd_levels), ] 
subset <- sumdf[which(sumdf$stocking_density <= 3.17535),]

# is there a threshold sd above which cattle lose weight?
p <- ggplot(sumdf, aes(x=date, y=animal_gain_kg,
                        group=stocking_density_f))
p <- p + geom_point(aes(colour=stocking_density_f))
p <- p + geom_line(aes(colour=stocking_density_f))
print(p)
p <- ggplot(subset, aes(x=date, y=total_offtake_kg,
                       group=stocking_density_f))
p <- p + geom_point(aes(colour=stocking_density_f))
p <- p + geom_line(aes(colour=stocking_density_f))
print(p)

# biomass varying with date
p <- ggplot(subset, aes(x=date, y=above_total_biomass_kg_ha,
                       group=stocking_density_f))
p <- p + geom_point(aes(colour=stocking_density_f))
print(p)
p <- ggplot(subset, aes(x=date, y=below_biomass_gm2,
                       group=stocking_density_f))
p <- p + geom_point(aes(colour=stocking_density_f))
print(p)

# biomass varying with stocking density
p <- ggplot(sumdf, aes(x=stocking_density, y=below_biomass_gm2,
                       group=date_f))
p <- p + geom_point(aes(colour=date_f))
print(p)  # effect of stocking density much more apparent at later dates
subset <- sumdf[which(sumdf$date_f == "2015.92"), ]
p <- ggplot(subset, aes(x=stocking_density, y=below_biomass_gm2))
p <- p + geom_point()
print(p)
p <- ggplot(subset, aes(x=stocking_density, y=above_total_biomass_kg_ha))
p <- p + geom_point()
print(p)
p <- ggplot(subset, aes(x=stocking_density, y=root_shoot))
p <- p + geom_point()
print(p)

