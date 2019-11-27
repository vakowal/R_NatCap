# prototype workflow for animal grazing pressure distribution inside polygon
# assumptions:
#   - modeled biomass vector (raster) contains modeled biomass without grazing
#   - EO biomass vector contains remotely sensed index that is linearly related to biomass
#   - absolute values of modeled vs EO indices are unrelated
#   - relative modeled and EO indices are linearly related
#   - we know the total number of animals grazing the total area
#   - areas of high modeled biomass and low EO biomass have high animal density

library(raster)
library(ggplot2)

# fake data
modeled_index <- runif(100, 100, 200)
EO_index <- modeled_index + runif(100, -50, 5)
total_animals <- 100

plotdf <- data.frame('potential_biomass'=modeled_index, 'observed_biomass'=EO_index)

p <- ggplot(plotdf, aes(x=potential_biomass, y=observed_biomass))
p <- p + geom_point()
p <- p + geom_abline(intercept = 0, slope=1)
print(p)

# OLD WAY #
# normalize each index relative to its minimum and maximum values inside the AOI
modeled_normalized <- (
  (modeled_index - min(modeled_index)) / (max(modeled_index) - min(modeled_index)))
EO_normalized <- (
  (EO_index - min(EO_index)) / (max(EO_index) - min(EO_index)))

# translate modeled index to be >= EO index
max_diff <- max(EO_normalized - modeled_normalized)
modeled_translated <- modeled_normalized + max_diff

# adjusted difference in normalized values is proportional to
# estimated number of grazing animals
adjusted_diff <- modeled_translated - EO_normalized
# rescale adjusted difference to proportion of total
proportional_adjusted_diff <- adjusted_diff / sum(adjusted_diff)
# number of animals on each pixel
num_animals <- proportional_adjusted_diff * total_animals

# plots, to visualize
plotdf <- data.frame(
  'modeled_index'=modeled_index, 'EO_index'=EO_index,
  'modeled_norm'=modeled_normalized, 'EO_norm'=EO_normalized,
  'modeled_translated'=modeled_translated)

p <- ggplot(plotdf, aes(x=EO_index, y=modeled_index))
p <- p + geom_point()
p <- p + ggtitle("raw (fake) indices")
print(p)

p <- ggplot(plotdf, aes(x=EO_normalized, y=modeled_norm))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + ggtitle("normalized indices")
print(p)

p <- ggplot(plotdf, aes(x=EO_normalized, y=modeled_translated))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + ggtitle("normalized modeled index >= EO index")
print(p)

# real data: NDVI and EVI vs modeled aglivc
poster_theme <- theme_bw() + theme(strip.text.y=element_text(size=18), 
                     strip.text.x=element_text(size=18), 
                     axis.title.x=element_text(size=22), 
                     axis.title.y=element_text(size=22),
                     axis.text=element_text(size=18),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10),
                     panel.border=element_rect(size=2))

outerdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/iems_2018/animal_spatial_distribution"
# df_list <- list()
# for (EO_dir in c('aveEVI2', 'aveNDVI', 'EVI', 'NDVI')){
EO_dir <- 'NDVI'
modeled_normalized_path <- paste(outerdir, EO_dir, "normalized_biomass.tif", sep="/")
EO_normalized_path <- paste(outerdir, EO_dir, "normalized_EO_index.tif", sep="/")
est_density_path <- paste(outerdir, EO_dir, "animal_distribution.tif", sep="/")
modeled_translated_path <- paste(outerdir, EO_dir, "modeled_translated.tif", sep="/")

# plot points from raster
real_df <- data.frame('modeled_normalized'=values(raster(modeled_normalized_path)),
                      'EO_normalized'=values(raster(EO_normalized_path)),
                      'est_density'=values(raster(est_density_path)),
                      'modeled_translated'=values(raster(modeled_translated_path)))
real_df$EO_source <- EO_dir
real_df <- real_df[!is.na(real_df$modeled_normalized), ]
# df_list[[EO_dir]] <- real_df
# }
# real_df <- do.call(rbind, df_list)

p <- ggplot(real_df, aes(x=modeled_normalized, y=est_density))
p <- p + geom_point()
print(p)

p <- ggplot(real_df, aes(x=EO_normalized, y=est_density))
p <- p + geom_point()
print(p)

real_df$diff <- real_df$EO_normalized - real_df$modeled_normalized
randsample <- real_df[sample(nrow(real_df), 25), ]
lowest <- real_df[real_df$diff == min(real_df$diff, na.rm=TRUE), ]
randsample <- rbind(randsample, lowest)
figsample <- randsample  # save a good-looking random sample
p <- ggplot(figsample, aes(x=EO_normalized, y=modeled_translated))
p <- p + geom_point(size=4)
p <- p + geom_abline(slope=1, intercept=0, linetype=2, size=1.6)
p <- p + ylim(0, 1)
p <- p + xlab("Normalized EO index") + ylab("Translated modeled biomass")
p <- p + poster_theme
print(p)
pngname <- paste(outerdir, EO_dir, "figs", "EO_v_modeled_translated_subset.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=6.5)
print(p)
dev.off()

p <- ggplot(real_df, aes(x=EO_normalized, y=modeled_normalized))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2, size=1.6)
p <- p + xlab("Normalized EO index") + ylab("Normalized modeled aboveground biomass")
p <- p + poster_theme
p <- p + xlim(0,1)
print(p)

pngname <- paste(outerdir, "normalized_indices_v_modeled.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=6.5)
print(p)
dev.off()

# best fit line: normalized modeled biomass ~ normalized end of month NDVI
ndvi_df <- real_df[real_df$EO_source == 'NDVI', ]
ndvi_fit <- lm(modeled_normalized~EO_normalized, data=ndvi_df)
model_coeff <- coefficients(ndvi_fit)
p <- ggplot(ndvi_df, aes(x=EO_normalized, y=modeled_normalized))
p <- p + geom_point()
p <- p + geom_abline(intercept=model_coeff[[1]], slope=model_coeff[[2]])
print(p)

p <- ggplot(real_df, aes(x=EO_normalized, y=modeled_translated))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + ggtitle("normalized modeled index >= EO index")
print(p)

# try out published regressions relating NDVI and biomass


pngname <- paste(outerdir, EO_dir, "figs/EO_vs_modeled_translateds_without_extreme_point.png", sep="/")
png(file=pngname, units="in", res=300, width=4.5, height=4)
print(p)
dev.off()
