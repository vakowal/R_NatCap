# memory-intensive raster processing for KBA+ES
library(raster)

raw_input_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/CV_summary_11.30.18/habitat_attribution_rasters"
  # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/IPBES_data_layers/pollination"
intermediate_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/CV_summary_11.30.18/10km/block_statistic_aggregate"
  # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/pollination_summary_11.30.18/10km/block_statistic_aggregate"

service_raster_list = ("service_mosaic.tif")
#    c(
#  "prod_poll_dep_realized_en_10s_ESACCI_LC_L4_LCSS.tif",
#  "prod_poll_dep_realized_fo_10s_ESACCI_LC_L4_LCSS.tif",
#  "prod_poll_dep_realized_va_10s_ESACCI_LC_L4_LCSS.tif"
#)
raw_input_list = unlist(
  lapply(service_raster_list, function(x) paste(raw_input_dir, x, sep="/")))
intermediate_output_list = unlist(
  lapply(service_raster_list, function(x) paste(intermediate_dir, x, sep="/")))

target_pixel_size = 0.083333333
service_pixel_size = 0.0022382581 # coastal vulnerability service raster # 0.0027777778 (pollination service rasters)
aggregate_factor = round(target_pixel_size / service_pixel_size)

for(index in seq(1, length(service_raster_list), by=1)) {
  in_ras = raster(raw_input_list[index])
  out_ras = intermediate_output_list[index]

  aggregate(
    in_ras, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE,
    filename=out_ras)
}

# pygeoprocessing didn't interpret default nodata correctly;
# try resetting nodata value
for(index in seq(1, length(service_raster_list), by=1)){
  in_ras <- raster(intermediate_output_list[index])
  out_ras <- reclassify(in_ras, cbind(NA, -9999))
  writeRaster(out_ras, intermediate_output_list[index], NAflag=-9999,
              overwrite=TRUE)
}

# resample biomass rasters from 30m to 10km,
# first masking out areas of forest loss < 2015
library(raster)
biomass_dir = "F:/GFW_ALWBD_2000"
lossyear_base_path = "F:/Hansen_lossyear/Hansen_GFC-2017-v1.5_lossyear_<loc_string>.tif"
resample_dir = "C:/Users/ginge/Desktop/biomass_working_dir/GFW_ALWBD_2015_10km_resample"

biomass_raster_list = list.files(path=biomass_dir, pattern="+.tif$")
input_path_list = unlist(
  lapply(biomass_raster_list, function(x) paste(biomass_dir, x, sep="/")))
output_list = unlist(
  lapply(biomass_raster_list, function(x) paste(resample_dir, x, sep="/")))

target_pixel_size = 0.083333333
service_pixel_size = 0.00025
aggregate_factor = round(target_pixel_size / service_pixel_size)

reclass_values = c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA, 9, NA,
                   10, NA, 11, NA, 12, NA, 13, NA, 14, NA)
reclass_mat <- matrix(reclass_values, ncol=2, byrow=TRUE)

for(index in seq(1, length(biomass_raster_list), by=1)) {
  print("working on ")
  print(index)
  biomass_path = input_path_list[index]
  loc_string = substr(biomass_raster_list[index], 0, 8)
  lossyear_path = gsub("<loc_string>", loc_string, lossyear_base_path)
  resampled_path = output_list[index]
  if (!file.exists(resampled_path)) {
    # mask out areas of biomass loss before 2015
    biomass_ras <- raster(biomass_path)
    mask_ras <- raster(lossyear_path)
    mask_rc <- reclassify(mask_ras, reclass_mat)
    biomass_masked = mask(biomass_ras, mask_rc)
    
    # aggregate 2015 biomass to ~10km resolution
    temp_ras <- aggregate(
      biomass_masked, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
    out_ras <- reclassify(temp_ras, cbind(NA, -9999))
    writeRaster(
      out_ras, resampled_path, NAflag=-9999, overwrite=TRUE)
    removeTmpFiles()
  }
}

# compare results at 10km and 30m resolution
library(ggplot2)
summary_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/carbon_summary_results"
summary_csv <- list.files(path=summary_dir, pattern="^biomass_KBA_summary+")
if (length(summary_csv) > 1) {
  df_list <- list()
  for (i in seq(1, length(summary_csv), by=1)) {
    df_list[[i]] <- read.csv(paste(summary_dir, summary_csv[i], sep='/'))
  }
  raw_df <- do.call(rbind, df_list)
} else {
  raw_df <- read.csv(paste(summary_dir, summary_csv, sep='/'))
}
sum_df <- raw_df[!is.na(raw_df$n_pixels), ]
sum_df <- sum_df[!duplicated(sum_df[, c('resolution', 'biomass_tile')]), ]
sum_df$percent_service_in_KBAs <- sum_df$global_service_sum_in_KBA / sum_df$global_service_sum
sum_df$percent_area_in_KBAs <- sum_df$n_KBA_pixels / sum_df$n_pixels
sum_df$perc_change_service_v_area <- (
  (sum_df$percent_service_in_KBAs - sum_df$percent_area_in_KBAs) / sum_df$percent_area_in_KBAs)

# summary: distribution of "service v area" across tiles, at two resolutions
p <- ggplot(sum_df, aes(x=resolution, y=perc_change_service_v_area))
p <- p + geom_boxplot()
pngname <- paste(summary_dir, "KBA_service_v_area_by_resolution.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

# global summary, all tiles combined
percent_service_10km <- (
  sum(sum_df[sum_df$resolution == '10km', 'global_service_sum_in_KBA']) /
    sum(sum_df[sum_df$resolution == '10km', 'global_service_sum']) * 100)
percent_area_10km <- (
  sum(sum_df[sum_df$resolution == '10km', 'n_KBA_pixels']) /
    sum(sum_df[sum_df$resolution == '10km', 'n_pixels']) * 100)
percent_service_30m <- (
  sum(sum_df[sum_df$resolution == '30m', 'global_service_sum_in_KBA']) /
    sum(sum_df[sum_df$resolution == '30m', 'global_service_sum']) * 100)
percent_area_30m <- (
  sum(sum_df[sum_df$resolution == '30m', 'n_KBA_pixels']) /
    sum(sum_df[sum_df$resolution == '30m', 'n_pixels']) * 100)
service_v_area_10km <- (
  (percent_service_10km - percent_area_10km) / percent_area_10km) * 100
service_v_area_30m <- (
  (percent_service_30m - percent_area_30m) / percent_area_30m) * 100

resolution_summary <- data.frame(
  'resolution' = c('10km', '30m'),
  'percent_area_in_KBAs' = c(percent_area_10km, percent_area_30m),
  'percent_service_in_KBAs' = c(percent_service_10km, percent_service_30m))
write.csv(resolution_summary, paste(summary_dir, 'resolution_summary.csv', sep='/'))

## throwaway test code
sum_30m <- sum_df[sum_df$resolution == '30m', ]
sum_10km <- sum_df[sum_df$resolution == '10km', ]
biomass_subset <- crop(biomass_ras, extent(biomass_ras, 20000, 21000, 39000, 40000))
mask_subset <- crop(mask_ras, extent(mask_ras, 20000, 21000, 39000, 40000))