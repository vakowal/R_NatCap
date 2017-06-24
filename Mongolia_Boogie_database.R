## explore Boogie's Access database
# helpful resource: https://www.r-bloggers.com/getting-access-data-into-r/

library(RODBC)

db <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/cashmere_Rangeland_monitoring.accdb"
con2 <- odbcConnectAccess2007(db)

table_names <- sqlTables(con2, tableType="TABLE")$TABLE_NAME
meta_df <- sqlFetch(con2, "Metadata")
str(meta_df)  # summarize structure of the table
livestock_df <- sqlFetch(con2, "bagh_livestock")
str(livestock_df)

columns_by_table <- list()
for(table_n in sqlTables(con2, tableType="TABLE")$TABLE_NAME){
  cols <- sqlColumns(con2, table_n)$COLUMN_NAME
  columns_by_table[[table_n]] <- cols
}
columns_by_table

table_list <- list()
for(table_n in table_names){
  table_list[[table_n]] <- sqlFetch(con2, table_n)
}

biomass_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/SCP_biomass50sites.csv")
soil_df <- sqlFetch(con2, 'site_soil_chemicals')
spp_df <- sqlFetch(con2, 'tblSpecies')

# find dominant species
spp_freq <- as.data.frame(table(biomass_df$plants))
avg_biomass <- aggregate(dry~plants, data=biomass_df, FUN=mean)

dominant_spp_df <- merge(spp_df, spp_freq, by.x='SpeciesCode', by.y='Var1')
dominant_spp_df <- merge(dominant_spp_df, avg_biomass, by.x='SpeciesCode',
                         by.y='plants')
colnames(dominant_spp_df)[15] <- 'avg_dry_biomass_GK'
colnames(dominant_spp_df)[14] <- 'n_times_recorded_GK'
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/tblSpecies_by_freq_avg_biomass.csv"
write.csv(dominant_spp_df, save_as, row.names=FALSE)

palat_df <- sqlFetch(con2, 'Palatability1D')
palat_spp <- palat_df[which(palat_df$Grazer == 'Goat' &
                              palat_df$Palatability == 'P'), 'Genus_spec']
palat_spp <- unique(palat_spp)  # palatable spp for goats
palat_subs <- palat_df[which(palat_df$Genus_spec %in% palat_spp &
                               palat_df$Grazer == 'Goat'),
                       c('Genus_spec', 'Palatability', 'Season')]
dup_palat <- unique(palat_subs[, c('Genus_spec', 'Palatability')]) # species with >1 level of palatability, including 'P', for goats
p_tab <- as.data.frame(table(dup_palat$Genus_spec), useNA='no')
confusing <- p_tab[which(p_tab$Freq > 1), ]

# plant functional types or communities
pft_df <- sqlFetch(con2, 'PlantCommunity')
pft_df$CommunityName <- gsub("<div>", "", pft_df$CommunityName)
pft_df$CommunityName <- gsub("</div>", "", pft_df$CommunityName)
pft_df$CommunityName <- gsub('<font color="#333333">', "", pft_df$CommunityName)
pft_df$CommunityName <- gsub('</font>', "", pft_df$CommunityName)
# write soil table for input to model
soil_df <- sqlFetch(con2, 'site_soil_chemicals')
soil_df <- soil_df[which(soil_df$soil_layers %in% c(0, 1)), ]
soil_0_20_cm <- aggregate(soil_df[, c(8, 22:24, 29)], by=list(soil_df$SiteID), FUN=mean,
                          na.rm=TRUE)
coords <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/monitoring_points_coordinates.csv")
soil_table <- merge(soil_0_20_cm, coords, by.x='Group.1', by.y='site')
colnames(soil_table) <- c('site', 'pH', 'sand_0_20_cm', 'silt_0_20_cm',
                          'clay_0_20_cm', 'bulkd_0_20_cm', 'latitude',
                          'longitude')
write.csv(soil_table, 'C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/soil_0_20_cm.csv',
          row.names=FALSE)
