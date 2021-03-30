# process NetCDF files
# summarize LUH2 data for Chaglla
ssp126_path <- "F:/Moore_Amazon_backups/LUH2_future_land_use/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc"
ncin <- nc_open(ssp126_path)
print(ncin)

lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- length(lat)
head(lat)

print(c(nlon, nlat)) # confirms 5 minute data resolution


# The variable and its longname, units and fill value (_FillValue) attributes are read next.

pft_names <- names(ncin$var)[c(1:14)]
year_list <- c(1, 36, 56)  # corresponding to years=2015, 2050, 2070
# dlname <- ncatt_get(ncin, dname, "Longname")
fillvalue <- 1.00000002004088e+20  # ncatt_get(ncin, dname, "Missval")

# write individual PFT slices for one PFT, one year, to tiff
pft_df <- expand.grid(lon=lon, lat=lat)
for (pft in pft_names) {
  pft_array = ncvar_get(ncin, pft)
  pft_array[pft_array == fillvalue] <- NA
  for (year in year_list) {
    actual_year <- 2015 + year - 1
    zval = as.vector(pft_array[, , year])
    xyz <- cbind(pft_df, zval)
    ras <- rasterFromXYZ(xyz, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
    tif_filename <- paste("C:/Users/ginge/Desktop/LUH2/",
                          paste(pft, '_', actual_year, ".tif", sep=""), sep="/")
    writeRaster(ras, tif_filename, format="GTiff", overwrite=TRUE)
  }
}

#### Replace NetCDF fill values with NAs #####
pft.array[pft.array == fillvalue] <- NA
#total number of non-missing grid cells
length(na.omit(as.vector(pft.array[, , 1]))) #80494

# make rasters for PFTs we care about
pft_df <- expand.grid(lon=lon, lat=lat)
for(pft_idx in c(1, 10:15)){
  zval <- as.vector(pft.array[, , pft_idx])
  xyz <- cbind(pft_df, zval)
  ras <- rasterFromXYZ(xyz, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  tif_filename <- paste("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/PFT/CLM",
                        paste(rownames(parameters)[pft_idx], ".tif", sep=""), sep="/")
  writeRaster(ras, tif_filename, format="GTiff", overwrite=TRUE)
}