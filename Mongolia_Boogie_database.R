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
confusing_spp <- p_tab[which(p_tab$Freq > 1), 'Var1']
confusing_tab <- palat_subs[which(palat_subs$Genus_spec %in% confusing_spp), ]

# palatability codes: preferred (P), desirable (D),
# consumed but undesirable (U), not consumable (N), toxic (T).

# plant functional types or communities
pft_df <- sqlFetch(con2, 'PlantCommunity')
pft_df$CommunityName <- gsub("<div>", "", pft_df$CommunityName)
pft_df$CommunityName <- gsub("</div>", "", pft_df$CommunityName)
pft_df$CommunityName <- gsub('<font color="#333333">', "", pft_df$CommunityName)
pft_df$CommunityName <- gsub('</font>', "", pft_df$CommunityName)

# assume annual/perennial and growth habit adheres to genus??
# two genera break this rule, Salsola and Artemisia
# removing Salsola-annual and Artemisia-annualaccording to Wikipedia
biomass_subs <- unique(biomass_df[, c('Genus', 'plant.age', 'GrowthHabit',
                                  'GrowthHabitSub.or.Functional.groups')])
biomass_subs <- biomass_subs[!(biomass_subs$Genus == 'Salsola' &
                                 biomass_subs$plant.age == 'Annual'), ]
biomass_subs <- biomass_subs[!(biomass_subs$Genus == 'Artemisia' &
                                 biomass_subs$plant.age == 'Annual'), ]
in_spp_not_biom <- setdiff(spp_df$Genus, biomass_subs$Genus)  # oops

species_list <- merge(spp_df, biomass_subs, by='Genus', all.x=TRUE)
species_list <- species_list[, colnames(species_list)[c(1:4, 11:12, 15:16)]]
pft_df$growth_habit <- 'NA'
for(r in 1:NROW(pft_df)){
  pft_spp <- pft_df[r, c('COM1', 'COM2', 'COM3')]
  pft_spp <- pft_spp[!is.na(pft_spp)]
  pft_growth <- species_list[which(species_list$SpeciesCode %in% pft_spp),
                             'GrowthHabitSub.or.Functional.groups']
  pft_str <- pft_growth[1]
  if(length(pft_growth) > 1){
    for(l in 2:length(pft_growth)){
      pft_str <- paste(pft_str, pft_growth[l], sep=', ')
    }
  }
  pft_df[r, 'growth_habit'] <- pft_str
}

# average total biomass per site
total_bio <- aggregate(oven.dry.biomass~SiteId, data=biomass_df, FUN=sum)
mean(total_bio$oven.dry.biomass)

# % grass, shrub, forb biomass at each site
prop_biomass_df <- data.frame('GrowthHabitSub.or.Functional.groups'=character(0),
                              'perc'=numeric(0),
                              'site'=character(0),
                              stringsAsFactors=FALSE)
raw_biomass_df <- data.frame('GrowthHabitSub.or.Functional.groups'=character(0),
                             'oven.dry.biomass'=numeric(0),
                             'site'=character(0),
                             stringsAsFactors=FALSE)
for(site in unique(biomass_df$SiteId)){
  subs <- biomass_df[which(biomass_df$SiteId == site), ]
  site_bio <- total_bio[which(total_bio$SiteId == site), 'oven.dry.biomass']
  bio_by_gr <- aggregate(oven.dry.biomass~GrowthHabitSub.or.Functional.groups,
                         data=subs, FUN=sum)
  bio_by_gr$site <- site
  raw_biomass_df <- rbind(raw_biomass_df, bio_by_gr)
  bio_by_gr$perc <- bio_by_gr$oven.dry.biomass / site_bio
  bio_by_gr <- bio_by_gr[, c('GrowthHabitSub.or.Functional.groups', 'perc')]
  bio_by_gr$site <- site
  prop_biomass_df <- rbind(prop_biomass_df, bio_by_gr)
}
p <- ggplot(prop_biomass_df, aes(perc, fill = GrowthHabitSub.or.Functional.groups))
p <- p + geom_density(alpha=0.6)
p <- p + scale_fill_discrete(name="Habit")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/perc_biomass_by_growth_habit.png"
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

site_order <- total_bio[order(total_bio$oven.dry.biomass,
                              decreasing=TRUE), 'SiteId']
raw_biomass_df$site <- factor(raw_biomass_df$site, levels=site_order)
p <- ggplot(raw_biomass_df, aes(x=site, y=oven.dry.biomass))
p <- p + geom_col(aes(fill=GrowthHabitSub.or.Functional.groups))
p <- p + scale_fill_discrete(name="Habit")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/raw_biomass_by_site_x_growth_habit.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

## worldclim data for Boogie's points
worldclim_precip <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Mongolia/Worldclim/monitoring_points_precip.csv")
avg_annual_precip <- mean(aggregate(prec~site, data=worldclim_precip, FUN=sum)[, 'prec'])
worldclim_temp <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/Mongolia/Worldclim/monitoring_points_temp.csv")
worldclim_temp$avg_temp <- colMeans(worldclim_temp[, c('tmax', 'tmin')])
avg_temp <- mean(worldclim_temp$avg_temp)

worldclim_precip$month <- as.factor(worldclim_precip$month)
p <- ggplot(worldclim_precip, aes(x=month, y=prec, group=site))
p <- p + geom_line()
print(p)

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
