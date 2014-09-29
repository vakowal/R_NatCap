## Process monthly cloud cover data for the entire globe
# (downloaded from http://www.ipcc-data.org/observ/clim/get_30yr_means.html)
# extract monthly values for one grid cell where Laikipia Kenya is located
# metadata for cloud cover data can be found in 'cloud_readme.txt' located in datadir

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Kenya/Data/Climate'
cloud <- read.fwf(paste(datadir, '/ccld6190.dat', sep = ""), widths = rep(5, 720), skip = 2, header = FALSE)
cloud[cloud == -9999] <- NA

# find grid cell position in dataset of a given location on earth
coords <- c(0.333194, 36.799429) # geographic coordinates of Laikipia, Kenya
x_seq <- seq(0.25, 359.75, by = 0.5)  # define coordinates represented by grid cells in table
y_seq <- seq(-89.75, 89.75, by = 0.5)
x_index <- 74  # closest position in x seq to coords[2]
y_index <- 181  # closest position in y seq to coords[1]

# divide data in monthly grids
clouds_list <- list()
lower <- 1
upper <- 360
for (list_index in seq(1, 12, by = 1)) {
  clouds_list[[list_index]] <- cloud[lower:upper, ]
  lower <- lower + 360
  upper <- upper + 360
}

# get monthly values for one grid
Laikipia_monthly <- c()
for (month in seq(1, 12, by = 1)){
  Laikipia_monthly[month] <- clouds_list[[month]][y_index, x_index]
}

write.table(Laikipia_monthly, paste(datadir, '/Laikipia_monthly_cloud.txt', sep = ""), row.names = FALSE, col.names = FALSE)
