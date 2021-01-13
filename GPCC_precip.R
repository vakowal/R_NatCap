# Process GPCC precipitation data
library(R.utils)
library(raster)

# unzip raw downloads
zipped_dir <- "E:/GIS_local/Mongolia/GPCC/compressed"
unzipped_dir <- "E:/GIS_local/Mongolia/GPCC/un_compressed"
for (file in list.files(zipped_dir, full.names=FALSE)) {
  source <- paste(zipped_dir, file, sep='/')
  destination <- paste(unzipped_dir, substring(file, 0, 30), sep='/')
  gunzip(source, destination, remove=FALSE)
}

# convert to tif
tif_dir <- "E:/GIS_local/Mongolia/GPCC/tif"
dir.create(tif_dir)
for (file in list.files(unzipped_dir, full.names=FALSE)) {
  in_path <- paste(unzipped_dir, file, sep='/')
  out_path <- paste(tif_dir, paste(substring(file, 0, 28), 'tif', sep=''), sep='/')
  ras <- raster(in_path)
  writeRaster(ras, out_path)
}

