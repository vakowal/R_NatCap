# calculate nested zonal statistics for KBAs in countries and in biomes

ES_col_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/service_column_names.csv",
                      header=FALSE)
ES_cols <- unique(ES_col_df$V1)
zonal_stat_table_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables"

# combine separate zonal stat tables
country_csv_list <- list.files(zonal_stat_table_dir,
                               pattern="TM_WORLD_BORDERS-0.3", full.names=TRUE)
country_df_list = lapply(country_csv_list, FUN=read.csv)
country_df_combined <- country_df_list[[1]]
if (length(country_df_list) > 1) {
  for (i in seq(2, length(country_df_list))) {
    country_df_combined <- merge(country_df_combined, country_df_list[[i]],
                                 all=TRUE)
  }
}
country_df_cols <- c("ISO3", "areakm2sum", "carbon1km_", "carbon1k_1",
                     "carbon1k_2", "fuelsum", "fuelmean",  "fuelstdev",
                     "grazingsum", "grazingmea", "grazingstd", "nitro_sum",
                     "nitro_mean", "nitro_stde", "sedim_sum", "sedim_mean",
                     "sedim_stde", "coastalsum", "coastalmea", "coastalstd")
country_df_subs <- country_df_combined[, country_df_cols]
write.csv(country_df_subs, paste(zonal_stat_table_dir, 'zonal_stat_countries_ES_area.csv', sep='/'),
          row.names=FALSE)

biome_csv_list <- list.files(zonal_stat_table_dir,
                             pattern="wwf_terr_ecos", full.names=TRUE)
biome_df_list <- lapply(biome_csv_list, FUN=read.csv)
biome_df_combined <- biome_df_list[[1]]
if (length(biome_df_list) > 1) {
  for (i in seq(2, length(biome_df_list))) {
    biome_df_combined <- merge(biome_df_combined, biome_df_list[[i]],
                               all=TRUE)
  }
}
biome_df_cols <- c("BIOME", "areakm2sum", "carbon1km_", "carbon1k_1",
                   "carbon1k_2", "fuelsum", "fuelmean",  "fuelstdev",
                   "grazingsum", "grazingmea", "grazingstd", "nitro_sum",
                   "nitro_mean", "nitro_stde", "sedim_sum", "sedim_mean",
                   "sedim_stde", "coastalsum", "coastalmea", "coastalstd")
biome_col_subs <- intersect(biome_df_cols, colnames(biome_df_combined))
biome_df_subs <- biome_df_combined[, biome_col_subs]
write.csv(biome_df_subs, paste(zonal_stat_table_dir, 'zonal_stat_biomes_area.csv', sep='/'),
          row.names=FALSE)

# zonal stats on ES calculated by Rachel
kba_zonal_stat_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/KBAs_2020_biomes_ES_28April2020_full_table.csv")
colnames(kba_zonal_stat_df)[1] <- 'SitRecID'
kba_area_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/KBA_pixel_area_zonal_stat_table.csv")
kba_combined_df <- merge(kba_zonal_stat_df, kba_area_df)
kba_service_cols_orig <- c("carbon1km_sum", "coast2sum", "floodsum", "fuelsum",
                      "grazingsum", "acc100sum", "nitro_sum", "pollin_sum",
                      "reeftour_s", "sedim_sum", "areakm2sum")
kba_service_cols_mod <- paste(kba_service_cols_orig, '_kba', sep="")
kba_cols_subs <- c('SitRecID', 'ISO3', 'wwf_BIOME', kba_service_cols_orig)
kba_df_subs <- kba_combined_df[, kba_cols_subs]
colnames(kba_df_subs) <- c('SitRecID', 'ISO3', 'BIOME', kba_service_cols_mod)

# merge KBA stats with country stats
library(reshape2)
kba_valid_country <- kba_df_subs[(kba_df_subs$ISO3 != '---') &
                                   (!is.na(kba_df_subs$ISO3)), c('ISO3', kba_service_cols_mod)]
# aggregate all service fields by country: giving sum of service in KBAs by country
kba_by_country_melt <- melt(kba_valid_country, id='ISO3')
kba_by_country <- dcast(kba_by_country_melt, ISO3 ~ variable, sum)
kba_country_df <- merge(kba_by_country, country_df_subs, by='ISO3')
# calculate % service contained by KBAs for each country
kba_country_df$carbon1km_percent_kba <- kba_country_df$carbon1km_sum_kba / kba_country_df$carbon1km_
kba_country_df$coast2_percent_kba <- kba_country_df$coast2sum_kba / kba_country_df$coastalsum
kba_country_df$fuel_percent_kba <- kba_country_df$fuelsum_kba / kba_country_df$fuelsum
kba_country_df$grazing_percent_kba <- kba_country_df$grazingsum_kba / kba_country_df$grazingsum
kba_country_df$nitrogen_percent_kba <- kba_country_df$nitro_sum_kba / kba_country_df$nitro_sum
kba_country_df$sediment_percent_kba <- kba_country_df$sedim_sum_kba / kba_country_df$sedim_sum
kba_country_df$area_percent_kba <- kba_country_df$areakm2sum_kba / kba_country_df$areakm2sum
write.csv(kba_country_df, paste(zonal_stat_table_dir, 'percent_service_area_in_kbas_by_country.csv', sep='/'),
          row.names=FALSE)
