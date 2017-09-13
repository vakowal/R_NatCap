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

biomass_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/SCP_biomass50sites.csv",
                       stringsAsFactors=FALSE)
spp_df <- sqlFetch(con2, 'tblSpecies', stringsAsFactors=FALSE)

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
pft_df <- sqlFetch(con2, 'PlantCommunity', stringsAsFactors=FALSE)
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
pft_df$growth_habit <- 0
pft_df$duration <- 0
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
  pft_duration <- species_list[which(species_list$SpeciesCode %in% pft_spp),
                               'Duration']
  pft_str <- pft_duration[1]
  if(length(unique(pft_duration)) > 1){
    for(l in 2:length(pft_duration)){
      pft_str <- paste(pft_str, pft_duration[l], sep=', ')
    }
  }
  pft_df[r, 'duration'] <- pft_str
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

# grass and forb biomass
grass_forb_biom <- biomass_df[which(biomass_df$GrowthHabitSub.or.Functional.groups != 'shrub'), ]
grass_forb_total <- aggregate(oven.dry.biomass~SiteId, data=grass_forb_biom, FUN=sum)

# sites in median 10% of grass+forb biomass
percs <- quantile(grass_forb_total$oven.dry.biomass, c(0.45, 0.55))
med_sites <- grass_forb_total[grass_forb_total$oven.dry.biomass >= percs[[1]] &
                                grass_forb_total$oven.dry.biomass <= percs[[2]],
                              c("SiteId", "oven.dry.biomass")]
  
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/grass_forb_biomass_freq.png"
png(file=pngname, units="in", res=300, width=6, height=4)
hist(grass_forb_total$oven.dry.biomass, breaks=50, xlab="Grass and forb biomass g/m2")
dev.off()
hist(grass_forb_total$oven.dry.biomass, breaks=3)

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

# write site csv for back-calc management routine
biomass_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/SCP_biomass50sites.csv")
for(r in 1:NROW(biomass_df)){
  biomass_df[r, 'site_no'] <- as.numeric(strsplit(as.character(biomass_df[r, 'SiteId']),
                                       split="st")[[1]][2])
}
biomass_df$site <- paste('st', biomass_df$site_no, sep="")

bio_type <- aggregate(biomass_df$oven.dry.biomass,
                           by=list(biomass_df$site, biomass_df$DATE,
                                   biomass_df$GrowthHabitSub.or.Functional.groups),
                           FUN=sum, na.rm=TRUE)
colnames(bio_type) <- c('site', 'date', 'type', 'biomass')
# reshape clumsily ugggggg
grass_biom <- bio_type[which(bio_type$type == 'grass'), ]
colnames(grass_biom)[4] <- 'grass_gm2'
shrub_biom <- bio_type[which(bio_type$type == 'shrub'), ]
colnames(shrub_biom)[4] <- 'shrub_gm2'
forb_biom <- bio_type[which(bio_type$type == 'forb'), ]
colnames(forb_biom)[4] <- 'forb_gm2'
bio_type_resh <- merge(grass_biom, shrub_biom, by=c('date', 'site'),
                              all=TRUE)
bio_type_resh <- merge(bio_type_resh, forb_biom, by=c('date', 'site'),
                              all=TRUE)
bio_type_resh <- bio_type_resh[, c(1:2, 4, 6, 8)]
bio_type_resh[is.na(bio_type_resh)] <- 0
bio_type_resh$total_biomass_gm2 <- rowSums(bio_type_resh[, c(3:5)])
coords <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/monitoring_points_coordinates.csv")
site_csv <- merge(bio_type_resh, coords, by='site',
                  all=TRUE)
site_csv <- site_csv[, c(1:6, 8)]
write.csv(site_csv, "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_inputs/sites.csv",
          row.names=FALSE)

## compare simulated biomass with Boogie's data
library(ggplot2)
cmp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/zero_sd/biomass_summary.csv")
cmp_sub <- cmp_df[, c(1:3, 6, 8)]
cmp_s_r <- reshape(cmp_sub, varying=c('grass_gm2', 'total_biomass_gm2', 'sim_gm2'),
                   v.names='biomass_gm2', timevar='type', times=c('grass', 'total_emp', 'sim'),
                   idvar='site', direction='long')
p <- ggplot(cmp_s_r, aes(x=site, y=biomass_gm2, group=type))
p <- p + geom_point(aes(colour=type))
print(p)

cmp_df$grass_plus_forb <- cmp_df$grass_gm2 + cmp_df$forb_gm2
cmp_sub <- cmp_df[, c(1:2, 8:9)]
cmp_sub <- cmp_sub[order(cmp_sub$grass_plus_forb, decreasing=TRUE), ]
cmp_g_f_resh <- reshape(cmp_sub, varying=c('sim_gm2', 'grass_plus_forb'),
                        v.names='biomass_gm2', timevar='type', times=c('sim', 'grass+forb'),
                        idvar='site', direction='long')
p <- ggplot(cmp_g_f_resh, aes(x=site, y=biomass_gm2, group=type))
p <- p + geom_point(aes(colour=type))
print(p)


#### throwaway
coords <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/USFS/NCEI_climate/stations.csv")
climdat <- read.csv("C:/Users/Ginger/Documents/NatCap/GIS_local/USFS/NCEI_climate/normals_monthly_data.csv")
coords$STATION_ID <- gsub("GHCND:", "", coords$STATION_ID)
datwcoords <- merge(coords, climdat, by.x='STATION_ID', by.y='STATION', all.y=TRUE)
write.csv(datwcoords, "C:/Users/Ginger/Documents/NatCap/GIS_local/USFS/NCEI_climate/normals_with_coords.csv",
          row.names=FALSE)
