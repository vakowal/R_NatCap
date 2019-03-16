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
