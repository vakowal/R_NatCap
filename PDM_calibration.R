# Calculate calibration regression for converting pasture disk meter (PDM) measurements to biomass

calibration_data = read.table("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/pdm_calibration.txt", 
                              header = TRUE)
pdm_area = 1884.79 # sq cm, calculated by Sharon
pdm_area_ha = pdm_area / 100000000  # area of the PDM in ha

calibration_data$kg_dm_per_ha = (calibration_data$Dry_mass_g / pdm_area_ha) / 1000
calibration_data$kg_wet_per_ha = (calibration_data$Wet_mass_g / pdm_area_ha) / 1000
plot(calibration_data$PDM_cm, calibration_data$kg_dm_per_ha)

wet_mass_reg = lm(kg_wet_per_ha ~ PDM_cm, calibration_data)
dry_mass_reg = lm(kg_dm_per_ha ~ PDM_cm, calibration_data)

calibration_data$biomass_OPC = 332.35 * calibration_data$PDM_cm + 15.857

summary(wet_mass_reg)
summary(dry_mass_reg)