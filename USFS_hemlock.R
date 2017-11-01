# USFS hemlock decline

# results: quickflow (fourth draft run)
qf_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/model_runs/fourth_draft/summary_of_results/monthly_quickflow.csv")
raw <- qf_df[qf_df$scenario != 'difference', ]
p <- ggplot(raw, aes(x=month, y=sum_quickflow, group=scenario))
p <- p + geom_point(aes(colour=scenario))
p <- p + geom_line(aes(colour=scenario))
print(p)

diff_df <- qf_df[qf_df$scenario == 'difference', ]
p <- ggplot(diff_df, aes(x=month, y=sum_quickflow))
p <- p + geom_point()
p <- p + geom_line()
p <- p + ggtitle("Difference: post - pre quickflow")
print(p)

# summarize landcover for USFS project

# overstory
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

crosswalk <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Overstory_classification.csv")
dom_cross <- merge(crosswalk, agg_veg_df, all=TRUE)
write.csv(dom_cross, "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Overstory_classification.csv",
          row.names=FALSE)

# reclassify according to my crosswalk
crosswalk <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Overstory_classification.csv")
mad_ov <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_overstory_Palmer_creek_1km_buf.csv",
                   stringsAsFactors=FALSE)
hemlock_types <- unique(c(grep('T/', mad_ov$DOMINANTVE, value=TRUE),
                          grep('-T', mad_ov$DOMINANTVE, value=TRUE),
                          grep('T-', mad_ov$DOMINANTVE, value=TRUE)))
mad_ov$Ecogroup.mod <- mad_ov$Ecogroup
mad_ov[mad_ov$DOMINANTVE %in% hemlock_types, 'Ecogroup.mod'] <- "Hemlock dominated"
mad_ov <- mad_ov[, c("FID", 'OBJECTID', "area_ha", 'Ecogroup.mod')]
mad_ov <- merge(mad_ov, crosswalk, by.x="Ecogroup.mod", by.y="Ecogroup")
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/overstory_lucode.csv"
write.csv(mad_ov, save_as, row.names=FALSE)

# summarize lucode by area
lucode_area <- aggregate(area_ha~lucode, data=mad_ov, FUN=sum)
lucode_area$perc_area_pre_decline <- 0
for (i in 1:NROW(lucode_area)){
  lucode_area[i, 'perc_area_pre_decline'] <- (lucode_area[i, 'area_ha'] / sum(lucode_area$area_ha)) * 100
}
lucode_area <- lucode_area[, c('lucode', 'perc_area_pre_decline')]
ov_subs <- unique(mad_ov[, c('lu_class', 'lucode')])
biophys_table <- merge(lucode_area, ov_subs)
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/biophys_pre_decline.csv"
write.csv(biophys_table, save_as, row.names=FALSE)

# other fields by area
domve_by_area <- aggregate(area_ha~DOMINANTVE, data=mad_ov, FUN=sum)
shortname_by_area <- aggregate(area_ha~Short_name, data=mad_ov, FUN=sum)
ecogr_by_area <- aggregate(area_ha~Ecogroup, data=mad_ov, FUN=sum)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/ov_w_hemlock.csv"
write.csv(mad_ov, save_as, row.names=FALSE)

## understory
mad_un <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_understory_Palmer_creek_1km_buf.csv")

# identify polygons where Rhododendron is present
mad_un$rhodo <- 0
for (desc in unique(mad_un$Descriptio)) {
  if (length(grep('hododendron', desc, fixed=TRUE, value=TRUE)) > 0){
    mad_un[mad_un$Descriptio == desc, 'rhodo'] <- 1
  }
}
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/understory_with_rhodo.csv"
write.csv(mad_un, save_as, row.names=FALSE)
