########## Parameters by PFT ##########
########## By Lindsey Sloat ##########
########## February 20, 2018 ##########

setwd("~/Documents/NatCap_CENTURY_extension")

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf)

library(sp)
library(rgdal)
library(maptools)
library(raster)

library(rasterVis)
library(maps)

library(RNetCDF)
library(ncdf4)

pft_param_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/PFT/CLM/pft-physiology_03022018.csv"
parameters = read.csv(pft_param_file, row.names = 1, header=TRUE)
head(parameters)

#### Read in NetCDF using NetCDF package #####
nc_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/PFT/CLM/mksrf_pft_0.5x0.5_simyr2005.c090313.nc"
ncin <- nc_open(nc_file)

lon <- ncvar_get(ncin, "LON")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "LAT", verbose = F)
nlat <- length(lat)
head(lat)

print(c(nlon, nlat)) # confirms 5 minute data resolution

# The variable and its longname, units and fill value (_FillValue) attributes are read next.

dname <- "PCT_PFT"

pft.array <- ncvar_get(ncin, dname)
# dlname <- ncatt_get(ncin, dname, "Longname")
fillvalue <- -999.0  # ncatt_get(ncin, dname, "Missval")

# PFT names
# p <- ncvar_get(ncin, "pft")
names(p) <- rownames(parameters)

#
nc_close(ncin)

#### Replace NetCDF fill values with NAs #####
pft.array[pft.array == fillvalue] <- NA
#total number of non-missing grid cells
length(na.omit(as.vector(pft.array[, , 1]))) #80494

# make rasters for PFTs we care about
pft_df <- expand.grid(lon=lon, lat=lat)
for(pft_idx in c(10:15)){
  zval <- as.vector(pft.array[, , pft_idx])
  xyz <- cbind(pft_df, zval)
  ras <- rasterFromXYZ(xyz, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  tif_filename <- paste("C:/Users/ginge/Documents/NatCap/GIS_local/Mongolia/PFT/CLM",
                        paste(rownames(parameters)[pft_idx], ".tif", sep=""), sep="/")
  writeRaster(ras, tif_filename, format="GTiff", overwrite=TRUE)
}


#### Individual maps #####
NET_temperate = pft.array[,,2]
image(lon, lat, NET_temperate, col = rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon = lon, lat = lat)
cutpts <- c( 0, 0.0001, 1, 5, 50, 100)
levelplot(NET_temperate ~ lon * lat, data = grid, at = cutpts, cuts = 6, pretty = T, 
          # par.settings=list(panel.background=list(col="white")),
          col.regions = (brewer.pal(10, "YlGn")))

#### Create dataframe #####

          # for a single variable 
            # m <- 1
            # tmp.slice <- pft.array[, , m]
            # 
            # lonlat <- expand.grid(lon, lat)
            # pft.vec <- as.vector(tmp.slice)
            # length(pft.vec)


#convert whole array to dataframe
pft.vec.long <- as.vector(pft.array)
length(pft.vec.long)
pft.mat <- matrix(pft.vec.long, nrow = nlon * nlat, ncol = length(rownames(parameters)))
dim(pft.mat)
hist(pft.mat[,2]) #NET temperate


# data frame
lonlat <- expand.grid(lon, lat)
pft.df <- data.frame(cbind(lonlat, pft.mat))
names(pft.df) <- c('lon', 'lat', 'Bareground',
                   'NET temperate',
                   'NET boreal',
                   'NDT boreal',
                   'BET tropical',
                   'BET temperate',
                   'BDT tropical',
                   'BDT temperate',
                   'BDT boreal',
                   'BES temperate',
                   'BDS temperate',
                   'BDS boreal',
                   'C3 grass arctic',
                   'C3 grass',
                   'C4 grass',
                   'Crops')
hist(pft.df[,4]) #NET temperate
# write.csv(pft.df, "Percent_PFT_flat.csv")


#
#
#
### Create community parameters ###
parameters2 = parameters[2:16,]
pft.df2=pft.df[4:18]

# *** if re-running skip this and load pft_norm.csv ****      
      pft_norm <- data.frame(matrix(NA, nrow=259200, ncol=15))
      for (i in 1:nrow(pft.df2)){
        x = pft.df2[i,]
        #grasses_norm[i,] = (x-min(x))/(max(x)-min(x))
        pft_norm[i,] = x/sum(x)
        
      }

#write.csv(pft_norm, file="pft_norm.csv")
pft_norm <- read.csv("pft_norm.csv")


## *** If re-running skip this and load 'Community_parameters_03022018.csv' ***
      store <- data.frame(matrix(NA, nrow=259200, ncol=17))
      # Loop
      for (i in 1:length(parameters2)){
        temp = pft_norm * parameters[,i]
        store[i] = rowSums(temp)
      }
      
      names(store) <- names(parameters)
      Community.parameters <- cbind(lonlat, store)
      colnames(Community.parameters)[1] <-"lon"
      colnames(Community.parameters)[2] <-'lat'

# write.csv(Community.parameters, file="Community_parameters_03022018.csv")
Community.parameters <- read.csv("Community_parameters_03022018.csv")
i=5
hist(parameters2[,i])
hist(Community.parameters[,i+2])


#
#
#


#### Extract PFTs for NutNet validation sites #####

## To use the extract function, need to read netcdf as a raster
r <- brick("mksrf_pft_0.5x0.5_simyr2005.c090313.nc") # reads all bands
r
plot(r, main='PFTs')
# a single layer
bare <- raster(r, layer=1)
plot(bare, main='Bare ground')


## validation_sites into points
# Try with shapefile

#import shapefile
val_pts<-readOGR(".","validation_sites")

# Define shapefile's current CRS
projection(val_pts) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Change raster to match spatial points 
#bb <- extent(-124.0484, 151.6067, -37.71516, 78.7)
bb <- extent( -180, 180, -89.9989, 83.5996)
extent(r) <- bb
bare <- raster(r, layer=1)
plot(bare)
points(val_pts)

            # check with ggmap
            lat2<-as.numeric(val_pts$latitude)
            long2<-as.numeric(val_pts$longitude)
            require(ggmap)# bounds is left, bottom, right, top
            bounds <- (c(min(long2),min(lat2),max(long2),max(lat2)))
            # mp <-  get_map(center="USA", maptype='roadmap',source='google',zoom=3)
            mp <-  get_map(location=bounds, maptype='roadmap',source='google',zoom=3)
            
            ggmap(mp)
            
            # the cool thing is then you can annotate them ggplot style
            ggmap(mp) + geom_point(aes(x=longitude,y=latitude,col=factor(experiment)),data=dat,cex=5)



extract(r, val_pts)

data <- data.frame(coordinates(val_pts),
                   val_pts$site_code, 
                   extract(r, val_pts))

names(data) <- c('lon', 'lat', 'site_code',
                 'Bareground',
                 'NET temperate',
                 'NET boreal',
                 'NDT boreal',
                 'BET tropical',
                 'BET temperate',
                 'BDT tropical',
                 'BDT temperate',
                 'BDT boreal',
                 'BES temperate',
                 'BDS temperate',
                 'BDS boreal',
                 'C3 grass arctic',
                 'C3 grass',
                 'C4 grass',
                 'Crops')

# write.csv(data,file="validation_point_PFTs.csv")



####"Community weighted" parameters for NutNet validation sites #####

#parameters2 = parameters[2:16,]

grasses=data[16:18]

grasses_norm <- data.frame(matrix(NA, nrow=51, ncol=3))
for (i in 1:nrow(grasses)){
  x = grasses[i,]
  #grasses_norm[i,] = (x-min(x))/(max(x)-min(x))
  grasses_norm[i,] = x/sum(x)
  
}
#write.csv(grasses_norm, "grasses_norm.csv")


store <- data.frame(matrix(NA, nrow=51, ncol=45))
# Loop
for (i in 1:length(parameters)){
  temp = grasses_norm * parameters[,i]
  store[i] = rowSums(temp)
}

names(store) <- names(parameters)
Community.parameters.validation <- cbind(data$lon,data$lat, data$site_code, store)
colnames(Community.parameters.validation)[1] <-"lon"
colnames(Community.parameters.validation)[2] <-'lat'
colnames(Community.parameters.validation)[3] <-'site_code'

#write file
write.csv(Community.parameters.validation, file="Grass_parameters_validation_03022018.csv")


## Parametes from TRY
pt <- read.csv(file="TRY_parameters.csv", header=TRUE, row.names=1)
head(pt)
dim(pt)

grasses=data[16:18]

# #if re-doing skip this and read in "grasses_norm.csv"
# grasses_norm <- data.frame(matrix(NA, nrow=51, ncol=3))
# for (i in 1:nrow(grasses)){
#   x = grasses[i,]
#   #grasses_norm[i,] = (x-min(x))/(max(x)-min(x))
#   grasses_norm[i,] = x/sum(x)
#   
# }
# #write.csv(grasses_norm,"grasses_norm.csv" )

grasses_norm <- read.csv("grasses_norm.csv", header=TRUE)

store <- data.frame(matrix(NA, nrow=51, ncol=10))
# Loop
for (i in 1:length(pt)){
  temp = grasses_norm * pt[,i]
  store[i] = rowSums(temp)
}

names(store) <- names(pt)
Community.TRY.params.validation <- cbind(data$lon,data$lat, data$site_code, store)
colnames(Community.TRY.params.validation)[1] <-"lon"
colnames(Community.TRY.params.validation)[2] <-'lat'
colnames(Community.TRY.params.validation)[3] <-'site_code'

#write file
write.csv(Community.TRY.params.validation, file="Community.TRY.params.validation.csv")




