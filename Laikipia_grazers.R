# Estimate total grazers in Laikipia county in 2011-2013
# using data from Ogutu et al. 2016,
# "Extreme Wildlife Declines and Concurrent Increase in Livestock Numbers in
# Kenya: What Are the Causes?"

# wildlife classification used in 2019 Sci Reports paper
group_key <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)

# table S1 from Ogutu et al. 2016, animal population estimates
ogutu_df <- read.csv("C:/Users/ginge/Documents/NatCap/Literature/Ogutu_et_al_2016/S1_table.csv")
colnames(ogutu_df) <- c(
  'species', 'sp_num', 'county_name', 'county_num', 'population_1980',
  'population_2013', 'pop_change', 'total_pop_1977', 'total_pop_2013',
  'proportion_1977', 'proportion_2013', 'proportion_change')
ogutu_df <- ogutu_df[(ogutu_df$county_name == 'Laikipia'),
                     c('species', 'sp_num', 'population_2013')]
ogutu_join_key <- merge(ogutu_df, gr_key_df, by.x='sp_num', by.y='Ogutu_2016_sp_num')
grazers <- ogutu_join_key[ogutu_join_key$Herbivory == 'grazer', ]
grazers$body_weight <- as.numeric(grazers$Unit_weight_kg)
grazers$AUE <- 0
for (r in (1:NROW(grazers))) {
  grazers[r, 'AUE'] <- (grazers[r, 'body_weight']**0.75) / (207**0.75) # (453.6**0.75)
}
grazers$weighted_population_2013 <- grazers$population_2013 * grazers$AUE
total_grazers_2013 <- sum(grazers$weighted_population_2013)

# compare that to cattle reported on each property in 2015
sum_est_cattle <- 69353
sum_properties_area_ha <- 332760
Laikipia_area <- 968396
est_cattle_Laikipia <- (sum_est_cattle * Laikipia_area) / sum_properties_area_ha
