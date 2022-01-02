# Aggregate by sum
# Snippet to aggregate a fine resolution raster to up a more coarse resolution
# via sum. Define the resolution of the aggregated result from a template
# raster at coarse resolution.
# The aggregated result will not match the resolution of the template exactly,
# but it will be close. The aggregated result will contain the exact sum of
# fine-resolution inside each coarse-resolution pixel, ignoring NA values.
# Following this aggregation, the aggregated result can be resampled to the
# exact resolution of the template raster (if necessary) via nearest neighbor
# resampling.
library(raster)

local_dir <- "F:/NCI_NDR/Data NDR/updated_3.27.21"
downloads_prefix <- 'https://storage.googleapis.com/nci-ecoshards/one_last_run/full_run_original_resolution/compressed_'
basename_df <- read.csv(paste(local_dir, 'basename_list.csv', sep='/'))
colnames(basename_df)[1] <- 'downloaded'

template_raster_path <- "F:/NCI_NDR/Data NDR/updated_5.18.20/sum_aggregate_to_0.084100_n_export_extensification_bmps_irrigated_global.tif"  # local path to raster at coarse resolution
target_pixel_size <- xres(raster(template_raster_path))

for (r in (1:NROW(basename_df))) {  # (bn in basename_df$basename) {'
  if (basename_df[r, 'downloaded'] == 'x') {
    bn <- basename_df[r, 'basename']
    fine_destination <- paste(local_dir, 'orig_resolution', paste(
      'compressed_', bn, '.tif', sep=''), sep='/')
    # if (!file.exists(fine_destination)) {
    #   url <- paste(downloads_prefix, paste(bn, 'tif', sep='.'), sep='')
    #   download.file(url, fine_destination, method='auto')
    # }
    output_raster_path <- paste(local_dir, 'resampled_via_sum', paste(bn, 'tif', sep='.'), sep='/')
    if (!file.exists(output_raster_path)) {
      aggregate_factor <- 28  # round(target_pixel_size / orig_pixel_size)
      orig_raster <- raster(fine_destination)
      agg_ras <- aggregate(
        orig_raster, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
      out_ras <- reclassify(agg_ras, cbind(NA, -9999))
      writeRaster(out_ras, filename=output_raster_path, NAflag=-9999)
    }
  }
}

outdir <- "F:/NCI_NDR/Data NDR/updated_3.27.21/resampled_via_sum"
fine_destination <- "F:/NCI_NDR/Data NDR/updated_3.27.21/orig_resolution/compressed_extensification_bmps_rainfed_300.0_D8_export.tif"
output_raster_path <- paste(outdir, "compressed_extensification_bmps_rainfed_300.0_D8_export.tif", sep='/')
orig_pixel_size <- xres(raster(fine_destination))
aggregate_factor <- 28  # match Becky  # round(target_pixel_size / orig_pixel_size)
orig_raster <- raster(fine_destination)
agg_ras <- aggregate(
  orig_raster, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
out_ras <- reclassify(agg_ras, cbind(NA, -9999))
output_raster_path <- paste("F:/NCI_NDR/Data NDR/updated_3.27.21/resampled_via_sum", "compressed_extensification_bmps_irrigated_300.0_D8_export_aggregate_viaR.tif", sep='/')
writeRaster(out_ras, filename=output_raster_path, NAflag=-9999)
