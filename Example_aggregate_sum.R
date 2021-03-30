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

template_raster_path <- ''  # local path to raster at coarse resolution
orig_resolution_raster_path <- ''  # local path to raster at original, fine resolution
output_raster_path <- ''  # local path where aggregated raster should be written

# the target pixel size can optionally be supplied directly here
target_pixel_size <- xres(raster(template_raster_path))
orig_pixel_size <- xres(raster(orig_resolution_raster_path))
aggregate_factor <- round(target_pixel_size / orig_pixel_size)

orig_raster <- raster(orig_resolution_raster_path)
agg_ras <- aggregate(
    orig_raster, fact=aggregate_factor, fun=sum, expand=TRUE, na.rm=TRUE)
out_ras <- reclassify(agg_ras, cbind(NA, -9999))
writeRaster(out_ras, filename=output_raster_path, NAflag=-9999)