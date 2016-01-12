# Calculate calibration regression for converting pasture disk meter (PDM) measurements to biomass
library(ggplot2)

calibration_data = read.table("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/pdm_calibration.txt", 
                              header = TRUE)
pdm_area = 1884.79 # sq cm, calculated by Sharon
pdm_area_ha = pdm_area / 100000000  # area of the PDM in ha

calibration_data$kg_dm_per_ha = (calibration_data$Dry_mass_g / pdm_area_ha) / 1000
calibration_data$kg_wet_per_ha = (calibration_data$Wet_mass_g / pdm_area_ha) / 1000
calibration_data$g_dm_per_sqm = (calibration_data$Dry_mass_g / pdm_area) * 10000
plot(calibration_data$PDM_cm, calibration_data$kg_dm_per_ha)

wet_mass_reg = lm(kg_wet_per_ha ~ PDM_cm, calibration_data)
dry_mass_reg = lm(kg_dm_per_ha ~ PDM_cm, calibration_data)

calibration_data$biomass_OPC = 332.35 * calibration_data$PDM_cm + 15.857 # equation developed by OPC monitoring crew

summary(wet_mass_reg)
summary(dry_mass_reg)

# dry kg/ha = 321.5 * PDM + 2246.4

dm_sqm = lm(g_dm_per_sqm ~ PDM_cm, calibration_data)
summary(dm_sqm)

# dry g/m = 28.571 * PDM + 207.154

p <- ggplot(calibration_data, aes(x=PDM_cm, y=kg_dm_per_ha))
p <- p + geom_point() + geom_point(aes(x=PDM_cm, y=biomass_OPC, colour="red"))
print(p)
