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
for(eg in unique(mad_ov$Ecogroup)){
  subs <- mad_ov[mad_ov$Ecogroup == eg, c('DOMINANTVE', 'area_ha')]
  domve_vals <- unique(subs$DOMINANTVE)
  for(val in domve_vals){
    # GINGER FINISH THIS -- IDENTIFY UNIQUE SPP, IDENTIFY THOSE THAT ARE >=50% OF AREA,
    # COUNT UP AREA OF EACH UNIQUE SPP IN THE ECOGROUP
    # then: is there a dominant spp per ecogroup? use to assign classification of the ecogroup
  }
  domveg <- aggregate(area)
}

# other fields by area
domve_by_area <- aggregate(area_ha~DOMINANTVE, data=mad_ov, FUN=sum)
shortname_by_area <- aggregate(area_ha~Short_name, data=mad_ov, FUN=sum)
ecogr_by_area <- aggregate(area_ha~Ecogroup, data=mad_ov, FUN=sum)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/ov_w_hemlock.csv"
write.csv(mad_ov, save_as, row.names=FALSE)

mad_un <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_understory_Palmer_creek_1km_buf.csv")