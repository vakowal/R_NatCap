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
outdir <- "C:/Users/ginge/Desktop/temp_out"
downloads_prefix <- 'https://storage.googleapis.com/nci-ecoshards/one_last_run/full_run_original_resolution/compressed_'
basename_df <- read.csv(paste(local_dir, 'basename_list.csv', sep='/'))
colnames(basename_df)[1] <- 're-do'

template_raster_path <- "F:/NCI_NDR/Data NDR/updated_5.18.20/n_export_30s_fixedarea_currentpractices_global.tif"  # local path to raster at coarse resolution
target_pixel_size <- xres(raster(template_raster_path))

for (r in (1:NROW(basename_df))) {  # (bn in basename_df$basename) {
  if (basename_df[r, 're-do'] == 'x') {
    bn <- basename_df[r, 'basename']
    fine_destination <- paste(local_dir, 'orig_resolution', paste(
      'compressed_', bn, '.tif', sep=''), sep='/')
    # if (!file.exists(fine_destination)) {
    #   url <- paste(downloads_prefix, paste(bn, 'tif', sep='.'), sep='')
    #   download.file(url, fine_destination, method='auto')
    # }
    output_raster_path <- paste(outdir, paste(bn, 'tif', sep='.'), sep='/')  # paste(local_dir, 'resampled_via_sum', paste(bn, 'tif', sep='.'), sep='/')
    if (!file.exists(output_raster_path)) {
      orig_pixel_size <- xres(raster(fine_destination))
      aggregate_factor <- round(target_pixel_size / orig_pixel_size)
      orig_raster <- raster(fine_destination)
      agg_ras <- aggregate(
        orig_raster, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
      out_ras <- reclassify(agg_ras, cbind(NA, -9999))
      writeRaster(out_ras, filename=output_raster_path, NAflag=-9999)
    }
  }
}



