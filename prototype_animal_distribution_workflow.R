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
modeled_index <- runif(100, 1, 100)
EO_index <- modeled_index * 1.3 + runif(100, -50, 50)
total_animals <- 100

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
modeled_normalized_path <- "C:/Users/ginge/Desktop/test_animal_distribution_dir/NDVI/normalized_biomass.tif"
EO_normalized_path <- "C:/Users/ginge/Desktop/test_animal_distribution_dir/NDVI/normalized_EO_index.tif"
modeled_translated_path <- "C:/Users/ginge/Desktop/test_animal_distribution_dir/NDVI/modeled_translated.tif"

# plot points from raster
real_df <- data.frame('modeled_normalized'=values(raster(modeled_normalized_path)),
                      'EO_normalized'=values(raster(EO_normalized_path)),
                      'modeled_translated'=values(raster(modeled_translated_path)))

p <- ggplot(real_df, aes(x=EO_normalized, y=modeled_normalized))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + ggtitle("normalized indices")
print(p)

pngname <- "C:/Users/ginge/Desktop/test_animal_distribution_dir/NDVI/figs/normalized_indices_without_extreme_point.png"
png(file=pngname, units="in", res=300, width=4.5, height=4)
print(p)
dev.off()

p <- ggplot(real_df, aes(x=EO_normalized, y=modeled_translated))
p <- p + geom_point()
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
p <- p + ggtitle("normalized modeled index >= EO index")
print(p)

pngname <- "C:/Users/ginge/Desktop/test_animal_distribution_dir/EVI/figs/EO_vs_modeled_translateds_without_extreme_point.png"
png(file=pngname, units="in", res=300, width=4.5, height=4)
print(p)
dev.off()
