# export single-band tifs from Lingling's multiple-band NDVI files
library(raster)

ndvi_img_pattern <- "E:/GIS_local_archive/Kenya_ticks/NDVI/fitted_3_day_average_monthly/Monthly_EVI2_<year>_Kenya_1km.img"

output_dir <- "E:/GIS_local_archive/Kenya_ticks/NDVI/fitted_3_day_average_monthly/export_to_tif"
for (year in c(2015)){  # c(2013, 2014, 2015)) {
  ndvi_img_path <- gsub('<year>', year, ndvi_img_pattern)
  # ndvi_img_path <- ndvi_img_pattern
  ndvi_stack <- stack(ndvi_img_path)
  for (month in 1:nlayers(ndvi_stack)) {
    band <- ndvi_stack[[month]]
    output_filename <- paste(
      output_dir, '/', paste('NDVI', year, sprintf('%02d', month), sep="_"),
      '.tif', sep="")
    writeRaster(band, output_filename)
  }
}

# calculate monthly average NDVI from 3-daily files
img_pattern <- "E:/GIS_local/Mongolia/NDVI/fitted_NDVI_3day/H25V04_H26V04_fitted_NDVI_<year>.006_Mongolia.img"
output_dir <- "E:/GIS_local/Mongolia/NDVI/fitted_NDVI_3day/monthly_avg_aggregated"
target_pixel_size <- xres(stack("C:/Users/ginge/Dropbox/sample_inputs/NDVI/ndvi_2016_01.tif"))
in_pixel_size <- xres(stack("E:/GIS_local/Mongolia/NDVI/fitted_NDVI_3day/H25V04_H26V04_fitted_NDVI_2016.006_Mongolia.img"))
aggregate_factor = round(target_pixel_size / in_pixel_size)
for (year in c(2019)) { # c(2016, 2017, 2018)) {
  ndvi_img_path <- gsub('<year>', year, img_pattern)
  ndvi_stack <- stack(ndvi_img_path)
  for (month in 1:12) {
    # select the 10 bands falling inside this month
    band_idx <- c(1:10) + (month - 1) * 10
    stack_subs <- ndvi_stack[[band_idx]]
    # reclassify 32767 to NA
    stack_subs[stack_subs == 32767] <- NA
    # calculate mean within the month
    stack_mean <- calc(stack_subs, fun=mean, na.rm=TRUE)
    # aggregate to coarse resolution
    agg_ras <- aggregate(
      stack_mean, fact=aggregate_factor, fun=mean, expand=TRUE, na.rm=TRUE)
    output_filename <- paste(
      output_dir, '/', paste('ndvi', year, sprintf('%02d', month), sep="_"),
      '.tif', sep="")
    writeRaster(agg_ras, output_filename, dataType="FLT4S")
  }
}

