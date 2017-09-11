# summarize landcover for USFS project

# overstory
mad_ov <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_overstory_Palmer_creek_1km_buf.csv")
domve_by_area <- aggregate(area_ha~DOMINANTVE, data=mad_ov, FUN=sum)
shortname_by_area <- aggregate(area_ha~Short_name, data=mad_ov, FUN=sum)
ecogr_by_area <- aggregate(area_ha~Ecogroup, data=mad_ov, FUN=sum)

hemlock_types <- unique(c(grep('T/', mad_ov$DOMINANTVE, value=TRUE),
                          grep('-T', mad_ov$DOMINANTVE, value=TRUE),
                          grep('T-', mad_ov$DOMINANTVE, value=TRUE)))
mad_ov$hemlock <- 0
mad_ov[mad_ov$DOMINANTVE %in% hemlock_types, 'hemlock'] <- 1
hemlock_area <- aggregate(area_ha~hemlock, data=mad_ov, FUN=sum)

write.csv(mad_ov, "C:/Users/Ginger/Desktop/ov_w_hemlock.csv", row.names=FALSE)


mad_un <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/Madden_understory_Palmer_creek_1km_buf.csv")