# information sources for monthly Kc values for Chaglla
library(ncdf4)
library(raster)

# summarize monthly LAI values
lai_path <- "F:/Moore_Amazon_backups/mean_seasonal_LAI/Mean_Seasonal_LAI_1653/data/LAI_mean_monthly_1981-2015.nc4"
lai_ncin <- nc_open(lai_path)
print(lai_ncin)

lon <- ncvar_get(lai_ncin, 'lon')
lat <- ncvar_get(lai_ncin, 'lat')
fillvalue <- -9999  # from metadata

# bounding box for Chaglla dam
min_lat <- -11
max_lat <- -9
min_lon <- -77
max_lon <- -75

longlatgrid <- expand.grid(lon=lon, lat=lat)
lai_array = ncvar_get(lai_ncin, 'LAI')
lai_array[lai_array == fillvalue] <- NA
for (m_index in c(1:12)) {
  month <- m_index + 7
  if (month > 12) {
    month <- month - 12
  }
  zval = as.vector(lai_array[, , m_index])
  xyz <- cbind(longlatgrid, zval)
  xyz_subs <- xyz[(xyz$lon >= min_lon) & (xyz$lon <= max_lon) &
                    (xyz$lat >= min_lat) & (xyz$lat <= max_lat), ]
  ras <- rasterFromXYZ(xyz_subs, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  tif_filename <- paste("F:/Moore_Amazon_backups/mean_seasonal_LAI/Mean_Seasonal_LAI_1653/data/export_to_tiff",
                        paste('LAI_', month, ".tif", sep=""), sep="/")
  writeRaster(ras, tif_filename, format="GTiff", overwrite=TRUE)
}

# plot LAI extracted to regular points within Chaglla watershed
library(ggplot2)
point_df <- read.csv("F:/Moore_Amazon_backups/mean_seasonal_LAI/Mean_Seasonal_LAI_1653/Chaglla_points_table.csv")
point_df$id <- factor(point_df$id)
p <- ggplot(point_df, aes(x=month, y=LAI))
p <- p + geom_line(aes(color=id))
print(p)

nonoutlier <- point_df[point_df$id != 3, ]
p <- ggplot(nonoutlier, aes(x=id, y=LAI))
p <- p + geom_boxplot()
print(p)

nonoutlier$month_f <- factor(nonoutlier$month)
p <- ggplot(nonoutlier, aes(x=month_f, y=LAI))
p <- p + geom_boxplot()
print(p)

p <- ggplot(nonoutlier, aes(x=month, y=LAI))
p <- p + geom_line(aes(color=id))
print(p)

# calculate mean Kc per month, using the relationship Kc = LAI/3 (from SWY users guide)
mean_LAI <- aggregate(LAI~month, nonoutlier, FUN=mean)
# mean_LAI$Kc <- mean_LAI$LAI / 3
# calculate seasonal values relative to maximum
relativize <- function(vec){
  relativized = vec / (max(vec))
  return(relativized)
}
mean_LAI$relative_LAI <- relativize(mean_LAI$LAI)

# summarize seasonal Kc values by landcover type from Liu et al 2016
liu_mean <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_data_inputs/Liu_et_al_2016_mean_annual_Kc.csv")
p <- ggplot(liu_mean, aes(x=description, y=mean_annual_Kc))
p <- p + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)

# apply seasonal curve from LAI to annual values from Liu et al
kc_seasonal <- data.frame(description=character(), Kc_1=numeric(), Kc_2=numeric(), Kc_3=numeric(),
                          Kc_4=numeric(), Kc_5=numeric(), Kc_6=numeric(), Kc_7=numeric(),
                          Kc_8=numeric(), Kc_9=numeric(), Kc_10=numeric(), Kc_11=numeric(),
                          Kc_12=numeric())
r <- 1
for (code in liu_mean$description) {
  seasonal_vals <- liu_mean[liu_mean$description == code, 'mean_annual_Kc'] *
    mean_LAI[order(mean_LAI$month), 'relative_LAI']
  kc_seasonal[r, ] <- c(code, seasonal_vals)
  r <- r + 1
}
write.csv(kc_seasonal, "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_data_inputs/Kc_seasonal_Liu_adjusted_LAI.csv",
          row.names=FALSE)

liu_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_data_inputs/Liu_et_al_2016_table 1.csv")
liu_df$Season = factor(liu_df$Season)
p <- ggplot(liu_df, aes(x=Season, y=Kc, group=description))
p <- p + geom_line(aes(color=description))
print(p)

p <- ggplot(liu_df, aes(x=description, y=Kc))
p <- p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p)
