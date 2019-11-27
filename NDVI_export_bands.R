# export single-band tifs from Lingling's multiple-band NDVI files
library(raster)

ndvi_img_pattern <- "C:/Users/ginge/Downloads/MODISCMG_V6_NDVI_<year>_Mongolia_fitted_Monthly.img"

output_dir <- "C:/Users/ginge/Dropbox/sample_inputs/NDVI"
for (year in c(2016, 2017)) {
  ndvi_img_path <- gsub('<year>', year, ndvi_img_pattern)
  ndvi_stack <- stack(ndvi_img_path)
  for (month in 1:nlayers(ndvi_stack)) {
    band <- ndvi_stack[[month]]
    output_filename <- paste(
      output_dir, '/', paste('ndvi', year, sprintf('%02d', month), sep="_"),
      '.tif', sep="")
    writeRaster(band, output_filename)
  }
}


