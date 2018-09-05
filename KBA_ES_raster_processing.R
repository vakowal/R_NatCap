# memory-intensive raster processing for KBA+ES
library(raster)

raw_input_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/IPBES_data_layers/pollination"
intermediate_dir = "C:/Users/ginge/Documents/NatCap/GIS_local/KBA_ES/pollination_summary/block_statistic_aggregate"

service_raster_list = c(
  "prod_poll_dep_realized_en_10s_cur.tif",
  "prod_poll_dep_realized_fo_10s_cur.tif",
  "prod_poll_dep_realized_va_10s_cur.tif"
)
raw_input_list = unlist(
  lapply(service_raster_list, function(x) paste(raw_input_dir, x, sep="/")))
intermediate_output_list = unlist(
  lapply(service_raster_list, function(x) paste(intermediate_dir, x, sep="/")))

target_pixel_size = 0.0833333
service_pixel_size = 0.00277778
aggregate_factor = round(target_pixel_size / service_pixel_size)

index = 1
for(index in c(2, 3)){
  in_ras = raster(raw_input_list[index])
  out_ras = intermediate_output_list[index]
  
  aggregate(
    in_ras, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE,
    filename=out_ras)
}

# try resetting nodata value
for(index in c(1, 2, 3)){
  in_ras <- raster(intermediate_output_list[index])
  out_ras <- reclassify(in_ras, cbind(NA, -9999))
  writeRaster(out_ras, intermediate_output_list[index], NAflag=-9999,
              overwrite=TRUE)
}

