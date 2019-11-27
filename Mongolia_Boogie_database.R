## explore Boogie's Access database
# helpful resource: https://www.r-bloggers.com/getting-access-data-into-r/

library(RODBC)

# process rangeland metric variables and scores
# SCP
SCP_total <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/rangeland_metric/rangeland_metric_SCP.csv")
SCP_variables <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/rangeland_metric/rangeland_metric_variables_SCP.csv")
setdiff(SCP_total$Site, SCP_variables$Site.WCS.2017)  # 0
setdiff(SCP_variables$Site.WCS.2017, SCP_total$Site)  # 0

SCP_variables$year <- sapply(strsplit(as.character(SCP_variables$Site.WCS.2017), split="_"), `[`, 2)
SCP_variables$site_id <- sapply(strsplit(as.character(SCP_variables$Site.WCS.2017), split="_"), `[`, 1)
SCP_total$year <- sapply(strsplit(as.character(SCP_total$Site), split="_"), `[`, 2)
SCP_total$site_id <- sapply(strsplit(as.character(SCP_total$Site), split="_"), `[`, 1)
SCP_total <- SCP_total[, colnames(SCP_total)[3:7]]
SCP_variables <- SCP_variables[, colnames(SCP_variables)[4:26]]
SCP_rm <- merge(SCP_total, SCP_variables)

# CBM
cbm_list <- list()
outer_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/rangeland_metric"
for(ecosys in c('desert_steppe', 'semi_desert', 'true_desert')){
  df <- read.csv(paste(outer_dir, paste('rangeland_metric_variables_CBM_',
                                        ecosys, '.csv', sep=""), sep="/"),
                 stringsAsFactors=FALSE, header=FALSE)
  df_t <- as.data.frame(t(df), stringsAsFactors=FALSE)
  colnames(df_t) <- df[, colnames(df)[1]]
  df_t <- df_t[2:dim(df_t)[1], ]
  colnames(df_t)[1] <- 'site_id'
  df_t$site_id <- paste("V",
                        formatC(as.integer(df_t$site_id), width=2, format="d", flag="0"),
                        sep="")
  df_t$Ecosystem <- ecosys
  cbm_list[[ecosys]] <- df_t
}
intersect(cbm_list[[1]]$site_id, cbm_list[[2]]$site_id)  # 0
intersect(cbm_list[[1]]$site_id, cbm_list[[3]]$site_id)  # 0
intersect(cbm_list[[2]]$site_id, cbm_list[[3]]$site_id)  # 0; no sites duplicated btw ecosystems
setdiff(colnames(cbm_list[[1]]), colnames(cbm_list[[2]]))  # 0
setdiff(colnames(cbm_list[[1]]), colnames(cbm_list[[3]]))  # 0
setdiff(colnames(cbm_list[[2]]), colnames(cbm_list[[3]]))  # 0; all cols shared
CBM_rm <- do.call(rbind, cbm_list)
# certain sites had different locations in 2016 and 2017 - identify these as belonging to 2017
spatial_key <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/CBM_spatial_site_key.csv")
spatial_key <- spatial_key[spatial_key$year_suf == 17, ]
for(s in spatial_key$site_id){
  CBM_rm[CBM_rm$site_id == s, 'site_id'] <- paste(s, "17", sep="_")
}

# re order columns, combine SCP with CBM
CBM_rm <- CBM_rm[,  colnames(CBM_rm)[c(1:22, 24, 25)]]
colnames(CBM_rm)[23] <- "Rangeland metric score"
CBM_rm$year <- 2017
CBM_rm$source <- 'CBM'
SCP_rm <- SCP_rm[, colnames(SCP_rm)[c(2, 6:26, 3, 4, 1)]]
SCP_rm$source <- 'SCP'
colnames(SCP_rm) <- colnames(CBM_rm)
rm_df <- rbind(CBM_rm, SCP_rm)  # <<<-- rangeland metric scores for CBM and SCP
write.csv(rm_df, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/rangeland_metric_SCP_CBM_2017.csv",
          row.names=FALSE)

## database, biomass
db <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_boogie_11.15.17.accdb"
# db <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/cashmere_Rangeland_monitoring.accdb"
con2 <- odbcConnectAccess2007(db)
sqlTables(con2, tableType="TABLE")$TABLE_NAME

# non-woody biomass, Boogie's sites
biomass_SCP <- sqlFetch(con2, "Biomass", stringsAsFactors=FALSE)
spp <- sqlFetch(con2, "tblSpecies")
growth <- sqlFetch(con2, "tblSpeciesGrowthHabit")
spp <- merge(spp, growth, by.x='GrowthHabitCode', by.y="Code")
spp <- spp[, c("GenSpec", "GrowthHabitSub")]
biomass_SCP <- merge(biomass_SCP, spp, by="GenSpec")
biomass_SCP$DATE <- as.Date(biomass_SCP$DATE, format="%d/%m/%Y")
biomass_SCP$dry_calc <- biomass_SCP$dry
biomass_SCP[is.na(biomass_SCP$dry), 'dry_calc'] <- biomass_SCP[is.na(biomass_SCP$dry), 'wet'] * 0.45561
biomass_SCP[is.na(biomass_SCP$FREQUENCY), 'FREQUENCY'] <- 4
biomass_SCP$dry_calc_gm2 <- biomass_SCP$dry_calc / biomass_SCP$FREQUENCY
biomass_SCP <- biomass_SCP[biomass_SCP$GrowthHabitSub != 'shrub', ]
total_biomass_SCP <- aggregate(dry_calc_gm2 ~ `2017ID` + DATE, data=biomass_SCP,
                               FUN=sum) # dry biomass, all SCP samples
colnames(total_biomass_SCP) <- c("site_id", "Date", "biomass_g_m2")
total_biomass_SCP$source <- 'SCP'
write.csv(total_biomass_SCP,
          "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/SCP_herb_biomass.csv",
          row.names=FALSE)

# CBM biomass, from Seegii
CBM_sites <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Seegii/CBM_site_coordinates_2016_17.csv")
CBM_biomass_16 <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Seegii/CBM_biomass_data_2016.csv")
for(r in 1:NROW(CBM_sites)){
  if(length(strsplit(as.character(CBM_sites[r, 'id_GK']),
                     split="_")[[1]]) > 1){
    CBM_sites[r, 'year_suf'] <- as.numeric(strsplit(as.character(CBM_sites[r, 'id_GK']),
                                                    split="_")[[1]][2])
  }
  else{
    CBM_sites[r, 'year_suf'] <- 0
  }
}
CBM_sites <- CBM_sites[CBM_sites$year_suf != 17, ]
CBM_sites <- CBM_sites[, c('Plot_ID', 'id_GK')]  # this is the dataframe
  # that translates "Plot_ID" to "site_id" that I used for simulations, for 2016 sites
CBM_biomass <- merge(CBM_sites, CBM_biomass_16, by='Plot_ID', all.y=TRUE)
# merge with growth form df to select only non-shrub veg
CBM_spp_growth_form_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_biomass_16_species_growth_form.csv")
CBM_biomass_merge_growth_form <- merge(CBM_biomass, CBM_spp_growth_form_df)
CBM_biomass_merge_growth_form <- CBM_biomass_merge_growth_form[
                                  CBM_biomass_merge_growth_form$GrowthHabit != "Woody", ]
CBM_biomass_non_woody <- aggregate(Average_biomass~id_GK + Date, data=CBM_biomass_merge_growth_form,
                                   FUN=sum)
CBM_biomass <- aggregate(Average_biomass~id_GK + Date, data=CBM_biomass,
                         FUN=sum)
CBM_biomass_non_woody$Date <- as.Date(CBM_biomass_non_woody$Date, format="%m/%d/%Y")
CBM_biomass_non_woody <- CBM_biomass_non_woody[, c('id_GK', 'Date', 'Average_biomass')]
colnames(CBM_biomass_non_woody) <- c('site_id', 'Date', 'biomass_g_m2')
CBM_biomass_non_woody$source <- 'CBM'
write.csv(CBM_biomass_non_woody,
          "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_herb_biomass_2016.csv",
          row.names=FALSE)

# one table: herbaceous biomass in 2016 (to match with back-calc mgmt)
cbm_herb_biomass <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_herb_biomass_2016.csv")
scp_herb_biomass <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/SCP_herb_biomass.csv")
biomass_combined <- rbind(scp_herb_biomass, cbm_herb_biomass)
# restrict by date
biomass_combined$Date <- as.Date(biomass_combined$Date, format="%Y-%m-%d")
biomass_2016 <- biomass_combined[(biomass_combined$Date >= "2016-01-01") &
                                   (biomass_combined$Date <= "2016-12-31"), ]
# add "Century date"
biomass_2016$day <- format(biomass_2016$Date, format="%d")
biomass_2016$date <- 2016.67  # days in second half of August: assign to end of August
biomass_2016[biomass_2016$day <= 15, 'date'] <- 2016.58  # days in first half of August: assign to end of July
biomass_2016 <- biomass_2016[!duplicated(biomass_2016), ]
write.csv(biomass_2016,
          "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/herbaceous_biomass_2016_SCP_CBM.csv",
          row.names=FALSE)
# throwaway
p <- ggplot(biomass_2016, aes(x=source, y=biomass_g_m2))
p <- p + geom_boxplot()
print(p)

# check: compare biomass of SCP vs CBM sites in 2016
wth_match <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_SCP_points_nearest_soum_ctr.csv")
wth_match <- wth_match[, c('site_id', 'NEAR_DIST', 'name_en')]
colnames(wth_match) <- c('site_id', 'distance_to_nearest_soum_center',
                         'nearest_soum_center')
check <- biomass_combined[biomass_combined$Date < "2017-01-01", ]
plot <- merge(wth_match, check, by='site_id')
p <- ggplot(plot, aes(x=source, y=biomass_g_m2))
p <- p + geom_boxplot()
p <- p + facet_wrap(~nearest_soum_center)
p <- p + xlab("Biomass data source") + ylab("Biomass (g per square m)")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/biomass_2016_SCP_CBM.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

meta_df <- sqlFetch(con2, "Metadata")

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

#### the following code refers to old data summaries, should rework
biomass_df <- sqlFetch(con2, "Biomass")
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
