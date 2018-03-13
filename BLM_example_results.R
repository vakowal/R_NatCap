# example totally made up BLM results

gain_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/BLM/forage_model_great_basin/example_liveweight_gain.csv")

gain_df$year <- factor(gain_df$year)
gain_df$site <- factor(gain_df$site, labels=c('Site A', 'Site B'))
gain_df$veg_treatment <- factor(gain_df$veg_treatment, labels=c("Invaded", "Restored"))

library(ggplot2)
p <- ggplot(gain_df, aes(x=veg_treatment, y=liveweight_gain_month))
p <- p + geom_boxplot()
p <- p + facet_grid(site~year)
p <- p + xlab("Vegetation treatment") + ylab("Average monthly liveweight gain (kg)")
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/BLM/forage_model_great_basin/example_liveweight_gain.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()
