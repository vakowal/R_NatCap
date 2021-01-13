# calculate nested zonal statistics for KBAs in countries and in biomes
zonal_stat_table_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables"

# combine separate zonal stat tables
# country_csv_list <- list.files(zonal_stat_table_dir,
#                                pattern="TM_WORLD_BORDERS-0.3", full.names=TRUE)
country_csv_list <- c(
  "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_countries/zonal_stat_countries_combined.csv",
  "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/TM_WORLD_BORDERS-0.3_area_table.csv"
)
country_df_list = lapply(country_csv_list, FUN=read.csv)
country_df_combined <- country_df_list[[1]]
if (length(country_df_list) > 1) {
  for (i in seq(2, length(country_df_list))) {
    country_df_combined <- merge(country_df_combined, country_df_list[[i]],
                                 all=TRUE)
  }
}
country_df_cols <- c("ISO3", "areakm2sum", "X_Carbo_sum", "X_Carbo_count", "X_Carbo_mean",
                     "coasta_sum", "coasta_count", "coasta_mean", "flood__sum", "flood__count", "flood__mean", "fuelwo_sum",
                     "fuelwo_count", "fuelwo_mean", "grazin_sum", "grazin_count", "grazin_mean", "nitrog_sum", "nitrog_count",
                     "nitrog_mean", "pollin_sum", "pollin_count", "pollin_mean", "reefto_sum", "reefto_count", "reefto_mean",
                     "sedime_sum", "sedime_count", "sedime_mean",
                     "natureaccess10__sum", "natureaccess10__count", "natureaccess10__mean",
                     "natureaccess100_sum", "natureaccess100_count", "natureaccess100_mean")
country_df_subs <- country_df_combined[, country_df_cols]
write.csv(country_df_subs, paste(zonal_stat_table_dir, 'zonal_stat_countries_ES_area.csv', sep='/'),
          row.names=FALSE)

# biome_csv_list <- list.files(zonal_stat_table_dir,
#                              pattern="wwf_terr_ecos", full.names=TRUE)
biome_csv_list <- c(
  "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_biome/zonal_stat_biome_combined.csv",
  "C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/wwf_terr_ecos_diss_area_table.csv"
)
biome_df_list <- lapply(biome_csv_list, FUN=read.csv)
biome_df_combined <- biome_df_list[[1]]
if (length(biome_df_list) > 1) {
  for (i in seq(2, length(biome_df_list))) {
    biome_df_combined <- merge(biome_df_combined, biome_df_list[[i]],
                               all=TRUE)
  }
}
biome_df_cols <- c("BIOME", "areakm2sum", "X_Carbo_sum", "X_Carbo_count", "X_Carbo_mean",
                   "coasta_sum", "coasta_count", "coasta_mean", "flood__sum", "flood__count", "flood__mean", "fuelwo_sum",
                   "fuelwo_count", "fuelwo_mean", "grazin_sum", "grazin_count", "grazin_mean", "nitrog_sum", "nitrog_count",
                   "nitrog_mean", "pollin_sum", "pollin_count", "pollin_mean",  "reefto_sum", "reefto_count", "reefto_mean",
                   "sedime_sum", "sedime_count", "sedime_mean",
                   "natureaccess10__sum", "natureaccess10__count", "natureaccess10__mean",
                   "natureaccess100_sum", "natureaccess100_count", "natureaccess100_mean")
biome_col_subs <- intersect(biome_df_cols, colnames(biome_df_combined))
biome_df_subs <- biome_df_combined[, biome_col_subs]
write.csv(biome_df_subs, paste(zonal_stat_table_dir, 'zonal_stat_biomes_ES_area.csv', sep='/'),
          row.names=FALSE)

# zonal stats on ES calculated by Rachel
kba_zonal_stat_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/KBAs_2020_biomes_ES_28April2020_full_table.csv")
colnames(kba_zonal_stat_df)[1] <- 'SitRecID'
kba_area_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/KBA+ES/processing_2020/zonal_stats_tables/KBA_pixel_area_zonal_stat_table.csv")
kba_combined_df <- merge(kba_zonal_stat_df, kba_area_df)
kba_service_cols_orig <- c("carbon1km_sum", "coast2sum", "floodsum", "fuelsum",
                      "grazingsum", "acc10sum", "acc100sum", "nitro_sum", "pollin_sum",
                      "reeftour_s", "sedim_sum", "areakm2sum")
kba_service_cols_mod <- paste(kba_service_cols_orig, '_kba', sep="")
kba_cols_subs <- c('SitRecID', 'ISO3', 'wwf_BIOME', kba_service_cols_orig)
kba_df_subs <- kba_combined_df[, kba_cols_subs]
colnames(kba_df_subs) <- c('SitRecID', 'ISO3', 'BIOME', kba_service_cols_mod)

library(reshape2)
# merge KBA stats with country stats
kba_valid_country <- kba_df_subs[(kba_df_subs$ISO3 != '---') &
                                   (!is.na(kba_df_subs$ISO3)), c('ISO3', kba_service_cols_mod)]
# aggregate all service fields by country: giving sum of service in KBAs by country
kba_by_country_melt <- melt(kba_valid_country, id='ISO3')
kba_by_country <- dcast(kba_by_country_melt, ISO3 ~ variable, sum)
kba_country_df <- merge(kba_by_country, country_df_subs, by='ISO3')
# calculate % service contained by KBAs for each country
kba_country_df$carbon1km_percent_kba <- kba_country_df$carbon1km_sum_kba / kba_country_df$X_Carbo_sum * 100
kba_country_df$coast2_percent_kba <- kba_country_df$coast2sum_kba / kba_country_df$coasta_sum * 100
kba_country_df$flood_percent_kba <- kba_country_df$floodsum_kba / kba_country_df$flood__sum * 100
kba_country_df$fuel_percent_kba <- kba_country_df$fuelsum_kba / kba_country_df$fuelwo_sum * 100
kba_country_df$grazing_percent_kba <- kba_country_df$grazingsum_kba / kba_country_df$grazin_sum * 100
kba_country_df$nitrogen_percent_kba <- kba_country_df$nitro_sum_kba / kba_country_df$nitrog_sum * 100
kba_country_df$pollin_percent_kba <- kba_country_df$pollin_sum_kba / kba_country_df$pollin_sum * 100
kba_country_df$reefto_percent_kba <- kba_country_df$reeftour_s_kba / kba_country_df$reefto_sum * 100
kba_country_df$sediment_percent_kba <- kba_country_df$sedim_sum_kba / kba_country_df$sedime_sum * 100
kba_country_df$acc10_percent_kba <- kba_country_df$acc10sum_kba / kba_country_df$natureaccess10__sum * 100
kba_country_df$acc100_percent_kba <- kba_country_df$acc100sum_kba / kba_country_df$natureaccess100_sum * 100
kba_country_df$area_percent_kba <- kba_country_df$areakm2sum_kba / kba_country_df$areakm2sum * 100
write.csv(kba_country_df, paste(zonal_stat_table_dir, 'percent_service_area_in_kbas_by_country.csv', sep='/'),
          row.names=FALSE)
kba_country_df <- read.csv(paste(zonal_stat_table_dir, 'percent_service_area_in_kbas_by_country.csv', sep='/'))

# merge KBA stats with biome stats
kba_valid_biome <- kba_df_subs[(!is.na(kba_df_subs$BIOME)), c('BIOME', kba_service_cols_mod)]
# aggregate all service fields by biome, giving sum of service in KBAs by biome
kba_by_biome_melt <- melt(kba_valid_biome, id='BIOME')
kba_by_biome <- dcast(kba_by_biome_melt, BIOME ~ variable, sum)
kba_biome_df <- merge(kba_by_biome, biome_df_subs)
# calculate % service contained by KBAs for each country
kba_biome_df$carbon1km_percent_kba <- kba_biome_df$carbon1km_sum_kba / kba_biome_df$X_Carbo_sum * 100
kba_biome_df$coast2_percent_kba <- kba_biome_df$coast2sum_kba / kba_biome_df$coasta_sum * 100
kba_biome_df$flood_percent_kba <- kba_biome_df$floodsum_kba / kba_biome_df$flood__sum * 100
kba_biome_df$fuel_percent_kba <- kba_biome_df$fuelsum_kba / kba_biome_df$fuelwo_sum * 100
kba_biome_df$grazing_percent_kba <- kba_biome_df$grazingsum_kba / kba_biome_df$grazin_sum * 100
kba_biome_df$nitrogen_percent_kba <- kba_biome_df$nitro_sum_kba / kba_biome_df$nitrog_sum * 100
kba_biome_df$pollination_percent_kba <- kba_biome_df$pollin_sum_kba / kba_biome_df$pollin_sum * 100
kba_biome_df$reefto_percent_kba <- kba_biome_df$reeftour_s_kba / kba_biome_df$reefto_sum * 100
kba_biome_df$sediment_percent_kba <- kba_biome_df$sedim_sum_kba / kba_biome_df$sedime_sum * 100
kba_biome_df$acc10_percent_kba <- kba_biome_df$acc10sum_kba / kba_biome_df$natureaccess10__sum * 100
kba_biome_df$acc100_percent_kba <- kba_biome_df$acc100sum_kba / kba_biome_df$natureaccess100_sum * 100
kba_biome_df$area_percent_kba <- kba_biome_df$areakm2sum_kba / kba_biome_df$areakm2sum * 100
kba_biome_table_path <- paste(zonal_stat_table_dir, 'percent_service_area_in_kbas_by_biome.csv', sep='/')
write.csv(kba_biome_df, kba_biome_table_path, row.names=FALSE)

# identify biome containing highest area of overlap with each KBA
overlap_df <- read.csv("F:/KBAs_3.1.20/intersect_wwf_terr_ecos.csv")
biome_cols <- colnames(overlap_df)[c(20:30, 34)]
biome_df <- overlap_df[!duplicated(overlap_df$BIOME), biome_cols]
keep_cols <- c('SitRecID', 'BIOME', 'area_inter')
overlap_df <- overlap_df[, keep_cols]
# calculate total area of overlap between each biome+KBA combination
sum_overlap <- aggregate(area_inter~SitRecID+BIOME, overlap_df, FUN=sum)
# find maximum area of overlap by KBA
sum_max <- aggregate(area_inter~SitRecID, sum_overlap, FUN=max)
max_overlap <- merge(sum_max, sum_overlap)
overlap_with_biome_cols <- merge(max_overlap, biome_df)
# drop intersect area column
overlap_with_biome_cols <- subset(overlap_with_biome_cols, select=-c(area_inter))
write.csv(overlap_with_biome_cols, "F:/KBAs_3.1.20/max_overlap_wwf_terr_ecos.csv")
