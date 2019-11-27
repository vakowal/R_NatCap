# compare monthly EO index (EVI2) to modeled biomass without grazing
library(ggplot2)

# modeled biomass, no grazing
model_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/pycentury_dev"
model_df_list <- list()
for (results_dir in list.files(model_dir)) {
  path <- paste(model_dir, results_dir, sep='/')
  if (file.info(path)[path, 'isdir']) {
    model_csv <- paste(model_dir, results_dir, 'summary_results.csv', sep='/')
    model_df <- read.csv(model_csv)
    grass_g <- grep("green_kgha", colnames(model_df), value=TRUE)
    grass_d <- grep("dead_kgha", colnames(model_df), value=TRUE)
    site_label <- gsub("_green_kgha", "", grass_g)
    model_df$total_biomass <- model_df[, grass_g] + model_df[, grass_d]
    model_df$site_label <- site_label
    subs_2016_g <- model_df[model_df$year == 2016, c(grass_g, 'month', 'site_label')]
    colnames(subs_2016_g) <- c('biomass', 'month', 'site_label')
    subs_2016_g$source <- 'modeled_green'
    subs_2016_total <- model_df[model_df$year == 2016, c('total_biomass', 'month', 'site_label')]
    colnames(subs_2016_total) <- c('biomass', 'month', 'site_label')
    subs_2016_total$source <- 'modeled_total'
    # site_df <- rbind(subs_2016_g, subs_2016_total)
    site_df <- subs_2016_g
    model_df_list[[site_label]] <- site_df
  }
}
model_df <- do.call(rbind, model_df_list)

# EVI2
evi2_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_vs_EO/EVI2_aoi_small.csv")
evi2_df$source <- 'EVI2'
colnames(evi2_df) <- c('month', 'biomass', 'site_label', 'source')
summary_df <- rbind(model_df, evi2_df)

img_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_vs_EO"
p <- ggplot(summary_df, aes(x=month, y=biomass, group=site_label))
p <- p + geom_line()
p <- p + facet_wrap(~source, scales='free')
print(p)

pngname <- "C:/Users/ginge/Desktop/modeled_biomass.png" # paste(img_dir, "biomass_line.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

# throwaway: make an example plot of biomass
subs_df <- summary_df[summary_df$source == 'modeled_green' & summary_df$site_label == 'X0', ]
p <- ggplot(subs_df, aes(x=month, y=biomass, group=site_label))
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_x_continuous(limits = c(1, 12), breaks=seq(1, 12, by = 2))
p <- p + labs(x ='Month', y =("Biomass (gC m-2)")) + print_theme
print(p)
pngname <- "C:/Users/ginge/Desktop/rangeland_model_FWAT_6.11.19/biomass_time_series.png"
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

# make a fake plot of diet sufficiency
diet_suff_df <- data.frame(
  seq(1, 12, by=1), 
  c(0.8, 0.9, 1.2, 1.24, 1.3, 1.5, 1.55, 1.6, 1.1, 0.9, 0.8, 0.7)
)
colnames(diet_suff_df) <- c('Month', 'Diet_sufficiency')
p <- ggplot(diet_suff_df, aes(x=Month, y=Diet_sufficiency))
p <- p + geom_point() + geom_line() + print_theme
p <- p + geom_abline(slope=0, intercept=1, color='grey42', size=0.3)
p <- p + ylab("Diet sufficiency")
p <- p + scale_x_continuous(limits = c(1, 12), breaks=seq(1, 12, by = 2))
print(p)
pngname <- "C:/Users/ginge/Desktop/rangeland_model_FWAT_6.11.19/diet_sufficiency_time_series.png"
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()