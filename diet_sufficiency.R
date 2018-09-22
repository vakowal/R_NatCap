# plot regional productivity energy balance for Kenya MS

library(ggplot2)
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/energy_balance_summary.csv")

p <- ggplot(sum_df, aes(x=step, y=herd_avg_diet_sufficiency))
p <- p + geom_line(aes(group=site))
p <- p + facet_grid(conception_step~density)
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/diet_sufficiency_conception_step_x_stocking_density.png"
png(file=pngname, units="in", res=300, width=6, height=8)
print(p)
dev.off()

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
p <- ggplot(precip_suff, aes(x=avg_annual_rainfall_mm, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
p <- p + ylab("# months with insufficient diet")
p <- p + xlab("Average annual rainfall (mm)")
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_rainfall.png"
png(file=pngname, units="in", res=300, width=4, height=7)
print(p)
dev.off()
