grand_dbf <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/GRanD_Version_1_3/GRanD_dams_v1_3.csv"
grand_df <- read.csv(grand_dbf)
brazil_ecuador_peru_df <- grand_df[(grand_df$COUNTRY.C.50 == 'Brazil') |
                                     (grand_df$COUNTRY.C.50 == 'Ecuador') |
                                     (grand_df$COUNTRY.C.50 == 'Peru'), ]
write.csv(brazil_ecuador_peru_df, "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/GRanD_Brazil_Ecuador_Peru.csv",
          row.names=FALSE)
dams_df <- read.csv("G:/Shared drives/Moore Amazon Hydro/dam_subset_black_orange.csv")

# summarize farm size distribution in watersheds
ws_full_df <- read.csv("G:/Shared drives/Moore Amazon Hydro/1_base_data/Other/watershed_characteristics/watershed_characteristics_combined.csv",
                       sep=';')
ws_full_df <- ws_full_df[, c('fid', 'area_ha')]
size_intersect_df <- read.csv("C:/Users/ginge/Desktop/Farm_size_Samberg_etal_2016/watershed_farm_size_intersect.csv")
total_area_intersect <- aggregate(area_ha~fid, data=size_intersect_df, FUN=sum)
colnames(total_area_intersect) <- c('fid', 'total_area_intersect')
total_area_df <- merge(ws_full_df, total_area_intersect)
total_area_df$perc_area_in_size_dataset <- total_area_df$total_area_intersect / total_area_df$area_ha
total_area_df <- total_area_df[, c('fid', 'total_area_intersect', 'perc_area_in_size_dataset')]
sum_area_by_size <- aggregate(area_ha~fid + Jun7_cat, data=size_intersect_df, FUN=sum)
colnames(sum_area_by_size) <- c('fid', 'Jun7_cat', 'area_ha_size_category')
area_merge <- merge(total_area_df, sum_area_by_size, all=TRUE)
area_merge$perc_area_size_category <- area_merge$area_ha_size_category / area_merge$total_area_intersect
long_df <- area_merge[, c('fid', 'Jun7_cat', 'perc_area_size_category')]
wide_df <- reshape(long_df, idvar='fid', timevar='Jun7_cat', v.names='perc_area_size_category', direction='wide')
total_area_df <- total_area_df[, c('fid', 'perc_area_in_size_dataset')]
summary_df <- merge(total_area_df, wide_df)
summary_df[is.na(summary_df)] <- 0
write.csv(summary_df, "G:/Shared drives/Moore Amazon Hydro/1_base_data/Other/watershed_characteristics/farm_size_summary.csv",
          row.names=FALSE)
