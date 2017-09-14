# summarize landcover for USFS project

# overstory
# reclassify according to my crosswalk
mad_ov <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_overstory_Palmer_creek_1km_buf.csv",
                   stringsAsFactors=FALSE)
crosswalk <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Overstory_classification.csv")
hemlock_types <- unique(c(grep('T/', mad_ov$DOMINANTVE, value=TRUE),
                          grep('-T', mad_ov$DOMINANTVE, value=TRUE),
                          grep('T-', mad_ov$DOMINANTVE, value=TRUE)))
mad_ov$Ecogroup.mod <- mad_ov$Ecogroup
mad_ov[mad_ov$DOMINANTVE %in% hemlock_types, 'Ecogroup.mod'] <- "Hemlock dominated"
mad_ov <- mad_ov[, c("FID", 'OBJECTID', 'Ecogroup.mod')]
mad_ov <- merge(mad_ov, crosswalk, by.x="Ecogroup.mod", by.y="Ecogroup")
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/overstory_lucode.csv"
write.csv(mad_ov, save_as, row.names=FALSE)

# exploratory stats etc
# is there a dominant spp for each ecogroup? calc on area
mad_ov <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_overstory_Palmer_creek_1km_buf.csv",
                   stringsAsFactors=FALSE)
df_list <- list()
for(eg in unique(mad_ov$Ecogroup)){
  eg_df <- data.frame('code'=character(), 'area'=numeric(),
                      stringsAsFactors=FALSE)
  i <- 1
  subs <- mad_ov[mad_ov$Ecogroup == eg, c('DOMINANTVE', 'area_ha')]
  for(val in unique(subs$DOMINANTVE)){
    val_subs <- subs[subs$DOMINANTVE == val, ]
    if (length(strsplit(val, '-')[[1]]) > 1) {
      for(j in c(1, 2)){
        eg_df[i, 'code'] <- strsplit(val, '-')[[1]][j]
        eg_df[i, 'area'] <- sum(val_subs$area_ha) / 2
        i <- i + 1
      }
    } else if (length(strsplit(val, '/')[[1]]) > 1) {
        j <- 1
        eg_df[i, 'code'] <- strsplit(val, '/')[[1]][j]
        eg_df[i, 'area'] <- sum(val_subs$area_ha)
        i <- i + 1
      } else {
          eg_df[i, 'code'] <- val
          eg_df[i, 'area'] <- sum(val_subs$area_ha)
          i <- i + 1
    }
  }
  domveg <- aggregate(area~code, data=eg_df, FUN=sum)
  domveg$perc_area <- 0
  for (r in (1:NROW(domveg))){
    domveg[r, 'perc_area'] <- (domveg[r, 'area'] / sum(domveg$area)) * 100
  }
  domveg$Ecogroup <- eg
  domveg <- domveg[domveg$perc_area == max(domveg$perc_area),
                   c('code', 'perc_area', 'Ecogroup')]
  df_list[[eg]] <- domveg
}  
agg_veg_df <- do.call(rbind, df_list)
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/dom_veg_by_ecogroup.csv"
write.csv(agg_veg_df, save_as, row.names=FALSE)

# other fields by area
domve_by_area <- aggregate(area_ha~DOMINANTVE, data=mad_ov, FUN=sum)
shortname_by_area <- aggregate(area_ha~Short_name, data=mad_ov, FUN=sum)
ecogr_by_area <- aggregate(area_ha~Ecogroup, data=mad_ov, FUN=sum)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/ov_w_hemlock.csv"
write.csv(mad_ov, save_as, row.names=FALSE)

mad_un <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_understory_Palmer_creek_1km_buf.csv")