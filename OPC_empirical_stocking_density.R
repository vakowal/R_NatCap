# make figures: empirical stocking density test at OPC weather stations

library(ggplot2)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
print_theme <- theme(strip.text.y=element_text(size=9), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=9), 
                     axis.title.y=element_text(size=9),
                     axis.text=element_text(size=9),
                     plot.title=element_text(size=9, face="bold"),
                     legend.text=element_text(size=9),
                     legend.title=element_text(size=9)) + theme_bw()

fig_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test/Figures"
outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Output/Stocking_density_test"

nutrient_level_list <- c("Calculated_from_CENTURY", "DMD_0.5_CP_0.06", "DMD_0.64_CP_0.1")
site_list <- c("Loidien", "Research")

df_list <- list()
i <- 1
for(nut in nutrient_level_list){
  for(site in site_list){
    file <- paste(outerdir, nut, site, "summary_results.csv", sep="/")
    df <- read.csv(file)
    df$total_biomass <- df$Research_green_kgha + df$Research_dead_kgha
    df$site <- site
    df$nutrient_level <- nut
    df_list[[i]] <- df
    i <- i + 1
  }
}
df <- do.call(rbind, df_list)

df$total_gain = rowSums(df[ , c("Bulls_gain_kg", "Calves_gain_kg", "Cows_gain_kg", "Heifers_gain_kg",
  "Steers_gain_kg", "Weaners_gain_kg", "steer.heifer_gain_kg")], na.rm=TRUE)
df[which(df$stocking_density == 0), c("stocking_density", "total_gain", "total_offtake")] <- NA
df[which(is.na(df$stocking_density)), "total_offtake"] <- NA
df$total_gain_x_10 <- df$total_gain * 10
df$nutrient_level <- as.factor(df$nutrient_level)
levels(df$nutrient_level)[levels(df$nutrient_level)=="Calculated_from_CENTURY"] <- "Calculated"
levels(df$nutrient_level)[levels(df$nutrient_level)=="DMD_0.5_CP_0.06"] <- "50% DMD, 6% CP"
levels(df$nutrient_level)[levels(df$nutrient_level)=="DMD_0.64_CP_0.1"] <- "64% DMD, 10%CP"

df$date <- as.Date(paste(df$year, sprintf("%02d", df$month), '01', sep="-"), format="%Y-%m-%d")

p <- ggplot(df, aes(x=date, y=total_biomass)) + geom_line(aes(color="biomass"))
p <- p + geom_point(aes(x=date, y=total_offtake, color="total offtake"))
p <- p + geom_point(aes(x=date, y=total_gain_x_10, color="total gain (* 10)"))
p <- p + facet_grid(site~nutrient_level) + ylab("kg")
p <- p + scale_color_manual(values=c("biomass"=cbPalette[1], 
                                     "total offtake"=cbPalette[2],
                                     "total gain (* 10)"=cbPalette[3]))
p <- p + print_theme + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.position="bottom")
pngname <- paste(fig_dir, "Empirical_stocking_density.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()
