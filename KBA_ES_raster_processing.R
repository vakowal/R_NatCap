# memory-intensive raster processing for KBA+ES
library(raster)

# generate aligned raster of pixel area values, for calculating area of KBAs
# for carbon
template_raster_path <- "F:/carbon_lpj_guess_workspace/aligned_inputs/LPJ-GUESS_rcp2p6_IMAGE_cVeg_2015_1x1.tif"
area_km2_raster <- area(raster(template_raster_path))
writeRaster(area_km2_raster, "F:/carbon_lpj_guess_workspace/aligned_inputs/pixel_area_km2.tif")

# for pollination
template_raster_path <- "F:/pollination_summary/aligned_inputs/onekmgaul_country__world.asc"
area_km2_raster <- area(raster(template_raster_path))
writeRaster(area_km2_raster, "F:/pollination_summary/aligned_inputs/pixel_area_km2.tif")

# for NDR
template_raster_path <- "F:/ndr_workspace/aligned_inputs/onekmgaul_country__world.asc"
area_km2_raster <- area(raster(template_raster_path))
writeRaster(area_km2_raster, "F:/ndr_workspace/aligned_inputs/pixel_area_km2.tif")

## throwaway code: calculate total global C in gigatons by mimicking Peter's calculations
example_c_path <- "F:/LPJ_Guess_carbon_scenarios/exported_geotiff/LPJ-GUESS_rcp2p6_IMAGE_cVeg_2015_1x1.tif"
area_m2_raster <- area(raster(example_c_path)) * 1000000  # approx area of each pixel in m2
gtC_raster <- raster(example_c_path) * area_m2_raster / 1000000000000
sum <- cellStats(gtC_raster, 'sum')

AIM_path <- "F:/LPJ_Guess_carbon_scenarios/exported_geotiff/LPJ-GUESS_rcp6p0_AIM_cVeg_2015_1x1.tif"
gtC_raster <- raster(AIM_path) * area_m2_raster / 1000000000000
sum_AIM <- cellStats(gtC_raster, 'sum')

MAGPIE_path <- "F:/LPJ_Guess_carbon_scenarios/exported_geotiff/LPJ-GUESS_rcp8p5_MAGPIE_cVeg_2015_1x1.tif"
gtC_raster <- raster(MAGPIE_path) * area_m2_raster / 1000000000000
sum_MAGPIE <- cellStats(gtC_raster, 'sum')

# calculate carbon values in Gt by multiplying kg/m2 by m2 and dividing by 1e12
input_dir <- "F:/carbon_lpj_guess_workspace/resampled_kg_m-2"
output_dir <- "F:/carbon_lpj_guess_workspace/aligned_inputs"
raster_list <- list.files(path=input_dir, pattern="^LPJ-GUESS_.*tif$")
raw_input_list = unlist(
  lapply(raster_list, function(x) paste(input_dir, x, sep="/")))
output_list = unlist(
  lapply(raster_list, function(x) paste(output_dir, x, sep="/")))

example_c_path <- raw_input_list[1]
area_m2_raster <- area(raster(example_c_path)) * 1000000  # approx area of each pixel in m2
for(index in seq(1, length(raw_input_list), by=1)) {
  save_as_filename = output_list[index]
  if (!file.exists(save_as_filename)) {
    print("working on")
    print(save_as_filename)
    c_ras <- raster(raw_input_list[index])
    c_ras_kg2 <- c_ras * area_m2_raster / 1000000000000
    out_ras <- reclassify(c_ras_kg2, cbind(NA, -9999))
    writeRaster(out_ras, filename=save_as_filename, NAflag=-9999)
  }
}
checkSum <- cellStats(raster("F:/carbon_lpj_guess_workspace/aligned_inputs/LPJ-GUESS_rcp2p6_IMAGE_cVeg_2015_1x1.tif"),
                      'sum')  # 335.1 Gt
checkSum <- cellStats(raster("F:/carbon_lpj_guess_workspace/aligned_inputs/LPJ-GUESS_rcp2p6_IMAGE_cVeg_2050_1x1.tif"),
                      'sum')  # 376.3 Gt

raw_input_dir = "F:/IPBES_data_layers_3.24.19"
    # "F:/cv_workspace"
    # "C:/Users/ginge/Desktop/pollination_desktop/IPBES_data_layers_3.24.19"
    # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/CV_summary_11.30.18/habitat_attribution_rasters"
    # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/IPBES_data_layers/pollination"
intermediate_dir = "F:/ndr_workspace/block_statistic_aggregate"
  # "F:/cv_workspace/block_statistic_aggregate"
  # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/pollination_summary/block_statistic_aggregate"
  # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/CV_summary_11.30.18/10km/block_statistic_aggregate"
  # "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/pollination_summary_11.30.18/10km/block_statistic_aggregate"

service_raster_list = list.files(path=raw_input_dir, pattern="^worldclim.*tif$")
  # c('service_mosaic_ssp1.tif', 'service_mosaic_ssp3.tif', 'service_mosaic_cur.tif', 'service_mosaic_ssp5.tif')
  # list.files(path=raw_input_dir, pattern="^water.*tif$")
# list.files(path=raw_input_dir, pattern="^prod_poll_dep_realized+")
# ("service_mosaic.tif")
#    c(
#  "prod_poll_dep_realized_en_10s_ESACCI_LC_L4_LCSS.tif",
#  "prod_poll_dep_realized_fo_10s_ESACCI_LC_L4_LCSS.tif",
#  "prod_poll_dep_realized_va_10s_ESACCI_LC_L4_LCSS.tif"
#)
raw_input_list = unlist(
  lapply(service_raster_list, function(x) paste(raw_input_dir, x, sep="/")))
intermediate_output_list = unlist(
  lapply(service_raster_list, function(x) paste(intermediate_dir, x, sep="/")))

target_pixel_size = 0.083333333  # 10km
for(index in seq(1, length(service_raster_list), by=1)) {
  in_ras = raster(raw_input_list[index])
  service_pixel_size = xres(in_ras)
  aggregate_factor = round(target_pixel_size / service_pixel_size)
  save_as_filename = intermediate_output_list[index]

  if (!file.exists(save_as_filename)) {
    print("working on")
    print(save_as_filename)
    agg_ras <- aggregate(
      in_ras, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
    out_ras <- reclassify(agg_ras, cbind(NA, -9999))
    writeRaster(out_ras, filename=save_as_filename, NAflag=-9999)
  }
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

#### summarize KBA pollination contributions by SSP scenario ####
img_dir <- "F:/pollination_summary/summary_tables_and_maps"
sum_csv <- "F:/pollination_summary/summary_tables_and_maps/global_service_summary.csv"
sum_df <- read.csv(sum_csv)

sum_df$service <- factor(sum_df$service, labels=c('Energy', 'Folate', 'Vitamin A'))
# total global service change by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_sum))
p <- p + geom_point()
p <- p + facet_wrap(~service, scales='free')
p <- p + ylab("Pollination service sum")
print(p)
pngname <- paste(img_dir, "poll_global_service_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=2)
print(p)
dev.off()

# percent service in KBAs by scenario x nutrient
p <- ggplot(sum_df, aes(x=scenario, y=global_service_percent_in_KBA))
p <- p + geom_point()
p <- p + facet_wrap(~service) #, scales='free')
p <- p + ylab("% pollination service in KBA")
pngname <- paste(img_dir, "poll_global_service_in_KBAs_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=3)
print(p)
dev.off()

### contribution of KBAs to coastal vulnerability service by scenario ###
img_dir <- "F:/cv_workspace/summary_tables_and_maps"
sum_csv <- "F:/cv_workspace/summary_tables_and_maps/cv_global_service_summary.csv"
sum_df <- read.csv(sum_csv)

# total global service change by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_sum))
p <- p + geom_point()
p <- p + ylab("Global CV service sum")
print(p)
pngname <- paste(img_dir, "cv_global_service_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=2)
print(p)
dev.off()

# percent service in KBAs by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_percent_in_KBA))
p <- p + geom_point()
p <- p + ylab("% CV service in KBA")
pngname <- paste(img_dir, "cv_global_service_in_KBAs_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

## contribution of KBAs to N retention service by scenario ###
img_dir <- "F:/ndr_workspace/summary_tables_and_maps"
sum_csv <- "F:/ndr_workspace/summary_tables_and_maps/global_service_summary.csv"
sum_df <- read.csv(sum_csv)

# total global service change by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_sum))
p <- p + geom_point()
p <- p + ylab("N retention service sum")
print(p)
pngname <- paste(img_dir, "global_service_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=2)
print(p)
dev.off()

# percent service in KBAs by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_percent_in_KBA))
p <- p + geom_point()
p <- p + ylab("% N retention service in KBA")
pngname <- paste(img_dir, "global_service_in_KBAs_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

# summarize carbon from LPJ-GUESS by scenario
img_dir <- "F:/carbon_lpj_guess_workspace/summary_tables_and_maps"
sum_csv <- "F:/carbon_lpj_guess_workspace/summary_tables_and_maps/global_service_summary.csv"
sum_df <- read.csv(sum_csv)
sum_df[sum_df$year == 2015, 'scenario'] <- 'cur'
sum_df[(sum_df$source == 'rcp2p6_IMAGE' & sum_df$year == 2050), 'scenario'] <- 'ssp1'
sum_df[(sum_df$source == 'rcp6p0_AIM' & sum_df$year == 2050), 'scenario'] <- 'ssp3'
sum_df[(sum_df$source == 'rcp8p5_MAGPIE' & sum_df$year == 2050), 'scenario'] <- 'ssp5'

# total global service change by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_sum))
p <- p + geom_point()
p <- p + ylab("C vegetation (Gt)")
print(p)
pngname <- paste(img_dir, "global_service_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=2)
print(p)
dev.off()

# percent service in KBAs by scenario
p <- ggplot(sum_df, aes(x=scenario, y=global_service_percent_in_KBA))
p <- p + geom_point()
p <- p + ylab("% carbon storage service in KBA")
pngname <- paste(img_dir, "global_service_in_KBAs_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()
