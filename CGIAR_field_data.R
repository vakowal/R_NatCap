# PDM records: Peru field data

library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

norm <- function(vec){
  normalized = (vec - min(vec)) / (max(vec) - min(vec))
  return(normalized)
}

PDM_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/PDM_records.csv"
PDM_df <- read.csv(PDM_file)
biomass_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/clipped_biomass_g.csv"
biomass_df <- read.csv(biomass_file)

# set up data frames to plot PDM vs biomass
biomass_df$unique_POI <- paste(biomass_df$Polygon_id, biomass_df$POI, sep="-")
PDM_df$unique_POI <- paste(PDM_df$Polygon_id, PDM_df$POI, sep="-")
PDM_sub <- PDM_df[which(PDM_df$unique_POI %in% biomass_df$unique_POI), ]
PDM_means <- aggregate(PDM_df$PDM, by=list(id=PDM_df$Polygon_id), FUN=mean, na.rm=TRUE)
PDM_means$id == PDM_sub$Polygon_id  # assert

PDM_plot_df <- data.frame('Polygon_id'=PDM_sub$Polygon_id, 'norm_value'=norm(PDM_sub$PDM),
                          'raw_val'=PDM_sub$PDM, 'raw_mean'=PDM_means$x,
                          'norm_mean'=norm(PDM_means$x), 'label'=rep('PDM', length(PDM_sub$Polygon_id)))
biomass_plot_df <- data.frame('Polygon_id'=biomass_df$Polygon_id,
                              'norm_value'=norm(biomass_df$g), 'raw_val'=biomass_df$g, 
                              'raw_mean'=biomass_df$g, 'norm_mean'=norm(biomass_df$g),
                              'label'=rep('clipped_biomass', length(biomass_df$Polygon_id)))

# create column of order of PDM samples, add to biomass df
PDM_ordered <- PDM_plot_df[order(-PDM_plot_df$norm_value), ] 
PDM_ordered$PDM_order <- c(1:11)
PDM_plot_df <- PDM_ordered[order(PDM_ordered$Polygon_id), ]
biomass_plot_df$PDM_order <- PDM_plot_df$PDM_order

# do same for order of biomass samples, add to PDM plot
biomass_ordered <- biomass_plot_df[order(-biomass_plot_df$norm_value), ]
biomass_ordered$biomass_order <- c(1:11)
biomass_plot_df <- biomass_ordered[order(biomass_ordered$Polygon_id), ]
PDM_plot_df$biomass_order <- biomass_plot_df$biomass_order

plot_df <- rbind(biomass_plot_df, PDM_plot_df)

imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/figs"

p <- ggplot(plot_df, aes(x=biomass_order, y=norm_value, group=label,
                         linetype=label))
p <- p + geom_point(aes(colour=label))
p <- p + geom_line()
p <- p + ylab("normalized point-level value")
pngname <- paste(imgpath, "PDM_biomass_point_normalized_by_biomass.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

p <- ggplot(plot_df, aes(x=PDM_order, y=norm_value, group=label,
                         linetype=label))
p <- p + geom_point(aes(colour=label))
p <- p + geom_line()
p <- p + ylab("normalized point-level value")
pngname <- paste(imgpath, "PDM_biomass_point_normalized_by_PDM.png", sep="/")
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

# scatterplot: PDM vs biomass
p <- ggplot(sc_df, aes(x=PDM_norm, y=biomass_norm))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + xlab("Normalized PDM") + ylab("Normalized biomass")
p <- p + print_theme
pngname <- paste(imgpath, "PDM_biomass_point.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

sc_df_sub <- sc_df[which(sc_df$PDM_norm < 1 & sc_df$biomass_norm < 1), ]
p <- ggplot(sc_df_sub, aes(x=PDM_norm, y=biomass_norm))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + xlab("Normalized PDM") + ylab("Normalized biomass")
p <- p + scale_x_continuous(limits=c(0, 0.3)) + scale_y_continuous(limits=c(0, 0.3))
p <- p + print_theme
pngname <- paste(imgpath, "PDM_biomass_point_subset.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

## regressions and correlations
cor_pearson <- cor.test(PDM_plot_df$raw_val, biomass_plot_df$raw_val)
cor_spearman <- cor.test(PDM_plot_df$raw_val, biomass_plot_df$raw_val, method="spearman")
# pearson r = 0.68, p = 0.02
# spearman rho = 0.78, p = 0.005

# without outlier
sc_df_s <- sc_df[which(sc_df$PDM_norm < 1), ]
cor_sub_p <- cor.test(sc_df_s$PDM_raw, sc_df_s$biomass_raw)
cor_sub_s <- cor.test(sc_df_s$PDM_raw, sc_df_s$biomass_raw, method="spearman")
# pearson r = 0.98, p < 0.0001
# spearman rho = 0.89, p = < 0.0001

fit <- lm(biomass_raw ~ PDM_raw, data=sc_df_s)
sc_df_s$fitted_values <- fit[['fitted.values']]

p <- ggplot(sc_df_s, aes(x=PDM_raw, y=biomass_raw))
p <- p + geom_point(color="red")
p <- p + geom_point(aes(x=sc_df_s$PDM_raw, y=sc_df_s$fitted_values))
print(p)
