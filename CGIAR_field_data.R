# Peru field data and remote sensing products

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

# compare remotely sensed biomass to modeled
remote_sens_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/remote_sensing_products/1115_results/Summary_11.15.16.csv")
mean_cor <- cor.test(remote_sens_df$MEAN_S2Rf, remote_sens_df$MEAN_L8Rf, method="spearman")
max_cor <- cor.test(remote_sens_df$MAX_S2Rf, remote_sens_df$MAX_L8Rf, method="spearman")

outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/biomass_time_series_zero_sd_11.16.16"
df_list <- list()
for(sub in c(1,2,3,4,5,6,7,9)){
  df <- read.csv(paste(outerdir, paste('s', sub, '_zero_sd.csv', sep=""), sep="/"))
  df$total_g_m2 <- (df[, paste('sub_', sub, '_dead_kgha', sep="")] + 
                    df[, paste('sub_', sub, '_green_kgha', sep="")]) / 10
  df_sub <- df[which(df$month == 5 | df$month == 6), c('month', 'year', 'total_g_m2')]
  df_sub$subbasin <- rep(sub, NROW(df_sub))
  df_list[[sub]] <- df_sub
}
sim_biomass <- do.call(rbind, df_list)
sim_biomass$month <- as.factor(sim_biomass$month)

#### excluding polygons 2 & 3
remote_sens_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/remote_sensing_products/excluding_polygons_2&3/summary_ex23.csv")
####

# reshape remote sensing data to plot with simulated data
reshape1 <- remote_sens_df[, c('SUBBASIN', 'MIN_S2Rf', 'MAX_S2Rf', 'RANGE_S2Rf',
                                          'MEAN_S2Rf', 'STD_S2Rf')]
colnames(reshape1) <- c('subbasin', 'MIN', 'MAX', 'RANGE',
                                   'MEAN', 'STD')
reshape1$dataset <- rep('S2Rf', NROW(reshape1))
reshape2 <- remote_sens_df[, c('SUBBASIN', 'MIN_L8Rf', 'MAX_L8Rf', 'RANGE_L8Rf',
                               'MEAN_L8Rf', 'STD_L8Rf')]
colnames(reshape2) <- c('subbasin', 'MIN', 'MAX', 'RANGE',
                        'MEAN', 'STD')
reshape2$dataset <- rep('L8Rf', NROW(reshape2))
remote_sens_reshape <- rbind(reshape1, reshape2)
remote_sens_reshape$month <- as.factor(rep(6, NROW(remote_sens_reshape)))

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/remote_sensing_products/excluding_polygons_2&3/comparisons_with_zero_sd_simulations"

# x axis = subbasin, month 6 only
sim_biomass = sim_biomass[which(sim_biomass$month == 6), ]
sim_biomass$subbasin = as.factor(sim_biomass$subbasin)
remote_sens_reshape$subbasin = as.factor(remote_sens_reshape$subbasin)
p <- ggplot(sim_biomass, aes(x=subbasin, y=total_g_m2))
p <- p + geom_boxplot()
p <- p + geom_point(data=remote_sens_reshape, aes(x=subbasin, y=MEAN, colour=dataset))
p <- p + ggtitle("simulated biomass vs mean remotely sensed biomass")
print(p)
pngname <- paste(imgdir, "zero_sd_simulated_vs_mean_remotesens_mo6.png", sep='/')
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

p <- ggplot(sim_biomass, aes(x=subbasin, y=total_g_m2))
p <- p + geom_boxplot()
p <- p + geom_point(data=remote_sens_reshape, aes(x=subbasin, y=MAX, colour=dataset))
p <- p + ggtitle("simulated biomass vs max remotely sensed biomass")
print(p)
pngname <- paste(imgdir, "zero_sd_simulated_vs_max_remotesens_mo6.png", sep='/')
png(file=pngname, units="in", res=300, width=7.5, height=5)
print(p)
dev.off()

## field data
PDM_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/PDM_records.csv"
PDM_df <- read.csv(PDM_file)
PDM_df$PDM  <- PDM_df$PDM...28.2..actual.height.of.disk.above.ground.
biomass_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/clipped_biomass_g.csv"
biomass_df <- read.csv(biomass_file)

# set up data frames to plot PDM vs biomass
biomass_df$unique_POI <- paste(biomass_df$Polygon_id, biomass_df$POI, sep="-")
PDM_df$unique_POI <- paste(PDM_df$Polygon_id, PDM_df$POI, sep="-")
PDM_sub <- PDM_df[which(PDM_df$unique_POI %in% biomass_df$unique_POI), ]
PDM_means <- aggregate(PDM_df$biomass_g_per_sq_m, by=list(id=PDM_df$Polygon_id), FUN=mean, na.rm=TRUE)
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
sc_df <- data.frame('PDM_norm'=PDM_plot_df$norm_value, 'biomass_norm'=biomass_plot_df$norm_mean,
                    'PDM_raw'=PDM_plot_df$raw_val, 'biomass_raw'=biomass_plot_df$raw_val)
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

# without polygons 2 and 3
sc_df_s <- sc_df[which(sc_df$PDM_raw < 30), ]
cor_sub_p <- cor.test(sc_df_s$PDM_raw, sc_df_s$biomass_raw)
cor_sub_s <- cor.test(sc_df_s$PDM_raw, sc_df_s$biomass_raw, method="spearman")

fit <- lm(biomass_raw ~ PDM_raw, data=sc_df_s)
sc_df_s$fitted_values <- fit[['fitted.values']]

p <- ggplot(sc_df_s, aes(x=PDM_raw, y=biomass_raw))
p <- p + geom_point(color="red")
p <- p + geom_point(aes(x=sc_df_s$PDM_raw, y=sc_df_s$fitted_values))
print(p)

# regression line and points
# just points used to fit the regression
p <- ggplot(sc_df_s, aes(x=PDM_raw, y=biomass_raw))
p <- p + geom_point() + print_theme
p <- p + geom_abline(slope=coef(summary(fit))["PDM_raw","Estimate"],
                     intercept=coef(summary(fit))["(Intercept)","Estimate"],
                     linetype=2)
p <- p + xlab("PDM (cm)") + ylab("Biomass (g)")
pngname <- paste(imgpath, "PDM_biomass_point_regression_ex_2and3.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

# all points
p <- ggplot(sc_df, aes(x=PDM_raw, y=biomass_raw))
p <- p + geom_point() + print_theme
p <- p + geom_abline(slope=coef(summary(fit))["PDM_raw","Estimate"],
                     intercept=coef(summary(fit))["(Intercept)","Estimate"],
                     linetype=2)
p <- p + xlab("PDM (cm)") + ylab("Biomass (g)")
pngname <- paste(imgpath, "PDM_biomass_point_regression_all_points.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

# empirical biomass: boxplot
PDM_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/PDM_records.csv"
PDM_df <- read.csv(PDM_file)
PDM_df$Polygon_id <- as.factor(PDM_df$Polygon_id)

p <- ggplot(PDM_df, aes(x=Polygon_id, y=biomass_g_per_sq_m))
p <- p + geom_boxplot()
p <- p + xlab('polygon') + ylab('biomass (g/m2)')
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/figs"
pngname <- paste(imgpath, "biomass_boxplot.png", sep="/")
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()


# compare empirical to simulated biomass
PDM_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/PDM_records.csv"
PDM_df <- read.csv(PDM_file)

simulated_val_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/calibration_results.csv"
sim_df <- read.csv(simulated_val_file)

ungrazed_sub <- subset(PDM_df, Polygon_id %in% sim_df$polygon)
p <- ggplot(ungrazed_sub, aes(factor(Polygon_id), biomass_kg_ha))
p <- p + geom_boxplot()
p <- p + xlab("Polygon") + ylab("Biomass (kg/ha)")
p <- p + geom_point(data=sim_df, aes(x=factor(polygon),
                                     y=biomass_kg_ha, group=source,
                                     color=source))
print(p)
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/figs"
pngname <- paste(imgpath, "biomass_calibration_comparison.png", sep="/")
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

#### compare SWAT to CENTURY biomass
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/SWAT_biomass_comparison/time_series_comparison.csv"
comp_df <- read.csv(comparison_csv)

df_list <- list()
for(sb in c(1,2,3,4,5,6,7,9)){
  subdf <- comp_df[which(comp_df$subbasin == sb), ]
  SWAT_fesc_means <- aggregate(subdf$SWAT_FESC_mean, by=list(subdf$month), FUN=mean)
  SWAT_ryeg_means <- aggregate(subdf$SWAT_RYEG_mean, by=list(subdf$month), FUN=mean)
  CENTURY_means <- aggregate(subdf$biomass_kgha_CENTURY, by=list(subdf$month), FUN=mean)
  
  means_df <- data.frame('month'=rep(c(1:12), 3),
                         'kg_ha'=c(SWAT_fesc_means$x, SWAT_ryeg_means$x, CENTURY_means$x),
                         'model_source'=c(rep('SWAT_FESC', 12), rep('SWAT_RYEG', 12), rep('CENTURY', 12)),
                         'subbasin'=rep(sb, 36))
  df_list[[sb]] <- means_df
}
plot_df <- do.call(rbind, df_list)

p <- ggplot(plot_df, aes(x=month, y=kg_ha, group=model_source))
p <- p + geom_point(aes(colour=model_source))
p <- p + geom_line(aes(colour=model_source))
# p <- p + geom_ribbon(aes(ymin=SWAT_min_mean, ymax=SWAT_max_mean), alpha=0.2)
p <- p + facet_wrap(~subbasin)  #, scales='free')
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/SWAT_biomass_comparison/summary_SWAT_CENTURY_biomass_by_lulc.png"
png(file=pngname, units="in", res=300, width=8, height=9)
print(p)
dev.off()
