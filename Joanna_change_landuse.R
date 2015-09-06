######### 
# Measure change in area of landcover types within small NHD watersheds for Joanna.
# 8.14.15
# Inputs were calculated with the 'tabulate area' tool in ArcMap.

watersheds <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/NHD_watersheds.csv",
                        header = TRUE)
rip05 <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/rip_2005_tab_intersect.csv",
                        header = TRUE)
rip12 <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/rip_2012_tab_intersect.csv",
                        header = TRUE)
ag05 <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/ag_2005_tab_intersect.csv",
                        header = TRUE)
ag12 <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/ag_2012_tab_intersect.csv",
                        header = TRUE)

num_wsheds <- length(unique(watersheds$OBJECTID))
diff_df <- data.frame(wshed = integer(num_wsheds), diff_rip_veg_05_12 = numeric(num_wsheds),
                      diff_ag_05_12 = numeric(num_wsheds))
# for now, combine the two riparian vegetation types
i <- 1
for (wshed in watersheds$OBJECTID){
  diff_rip <- sum(rip12[rip12$OBJECTID_1 == wshed, 'AREA']) - sum(rip05[rip05$OBJECTID_1 == wshed, 'AREA'])
  diff_ag <- sum(ag12[ag12$OBJECTID_1 == wshed, 'AREA']) - sum(ag05[ag05$OBJECTID_1 == wshed, 'AREA'])
  diff_df[i, ] <- c(wshed, diff_rip, diff_ag)
  i <- i + 1
}

save_as <- "C:/Users/Ginger/Documents/NatCap/GIS_local/Joanna/Change_riparian_AIS_2005_2012/diff_table.csv"
write.csv(diff_df, save_as)

########### misc snippets for analyzing time series sediment data ###########
datafile <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/OpenWater_qry_2015_08_13_10_28_55.csv"

data = read.csv(datafile)
levels(data$SiteTag)

data$stime <- strptime(data$SampleTime, format="%Y-%m-%d %H:%M:%S")
data$sdate <- strptime(data$SampleDate, format="%Y-%m-%d %H:%M:%S")
