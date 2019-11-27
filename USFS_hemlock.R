# USFS hemlock decline
library(RODBC)
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=9),
                     strip.text.x=element_text(size=9),
                     axis.title.x=element_text(size=9),
                     axis.title.y=element_text(size=9),
                     axis.text=element_text(size=8),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=9),
                     legend.title=element_text(size=10)) + theme_bw()

# precip sensitivity
imgdir <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/model_runs/precip_scenarios/figs"
precip_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/model_runs/precip_scenarios/precip_scenario_summary.csv"
precip_df <- read.csv(precip_csv)
precip_df$lulc_scenario <- factor(precip_df$lulc_scenario,
                                  levels=c('pre-decline', 'post-decline'),
                                  labels=c('Pre-decline', 'Post-decline'))
# points
p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_QF))
p <- p + geom_point(aes(shape=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual quickflow")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'quickflow.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_aet))
p <- p + geom_point(aes(shape=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual evapotranspiration")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'evapotranspiration.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_B))
p <- p + geom_point(aes(shape=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual baseflow")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'baseflow.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

# lines
p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_QF))
p <- p + geom_line(aes(linetype=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual quickflow")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'quickflow_line.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_aet))
p <- p + geom_line(aes(linetype=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual evapotranspiration")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'evapotranspiration_line.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

p <- ggplot(precip_df, aes(x=precip_multiply_factor, y=sum_B))
p <- p + geom_line(aes(linetype=lulc_scenario))
p <- p + xlab("Precipitation multiplication factor") + ylab("Average annual baseflow")
p <- p + print_theme + theme(legend.title = element_blank())
pngname = paste(imgdir, 'baseflow_line.png', sep='/')
png(file=pngname, units="in", res=400, width=4.6, height=3)
print(p)
dev.off()

# distribution of L sum avail: water available for transpiration
library(raster)
outer_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/model_runs/precip_scenarios"
df_list <- list()
i <- 1
for (mult_factor in c(0.5, 0.7, 0.9, 1.1, 1.3, 1.5)) {
  raster_path <- paste(outer_dir, paste(mult_factor, 'x', sep=''),
    'L_sum_avail_post_minus_pre.tif', sep='/')
  diff_raster <- raster(raster_path)
  df <- data.frame('Lsum_avail_post_minus_pre'=c(values(diff_raster)),
    'multiplication_factor'=(rep(mult_factor, length(c(values(diff_raster))))))
  df_list[[i]] <- df
  i <- i + 1
}
diff_df <- do.call(rbind, df_list)
diff_df$multiplication_factor <- factor(diff_df$multiplication_factor)
p <- ggplot(diff_df, aes(x=multiplication_factor, y=Lsum_avail_post_minus_pre))
p <- p + geom_boxplot()
p <- p + xlab("Precipitation multiplication factor") + ylab("Delta L_sum_avail") + print_theme
pngname <- paste(outer_dir, 'figs', "l_sum_avail_diff.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

# GSMNP veg monitoring database
imgdir <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis"

# Meghan Mulroy's data
mmulroydb <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/Meghan_Mulroy/GRSM_VM_2016_17 data.accdb"
conmmdb <- odbcConnectAccess2007(mmulroydb)
mmdb_tables <- sqlTables(conmmdb, tableType="TABLE")$TABLE_NAME
mm_tbl_overstory <- sqlFetch(conmmdb, 'tbl_Overstory')
mm_events <- sqlFetch(conmmdb, 'tbl_Events')
mm_overstory <- merge(mm_tbl_overstory, mm_events, by='Event_ID')
mm_overstory$Event_Date <- as.Date(mm_overstory$Event_Date, format="%Y-%m-%d")

# NPS database, Jenkins protocol
earlydb <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/2019_databases/2003_2009_data/GRSMJenkisPlots/GRSMPlots_JENKINS_BE/GRSMPlots_JENKINS_20170525_BE.accdb"
con1 <- odbcConnectAccess2007(earlydb)
tbl_early_Events <- sqlFetch(con1, 'tbl_Events')
tbl_early_Location <- sqlFetch(con1, 'tbl_Locations')
tbl_early_overstory <- sqlFetch(con1, 'tbl_Overstory')
tbl_early_overstory <- merge(tbl_early_Events, tbl_early_overstory, by='Event_ID')

# spatial join of all early db locations with Madden landcover
earlydb_intersect_madden_overstory <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis/Madden_veg_intersect_earlydb_sites.csv")
earlydb_intersect_madden_overstory <- earlydb_intersect_madden_overstory[, c('Location_I', 'Lumped_cla')]
colnames(earlydb_intersect_madden_overstory) <- c('Location_ID', 'Lumped_class')
earlydb_intersect_madden_rhodo <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis/Madden_veg_intersect_earlydb_sites_WRHODO.csv")
colnames(earlydb_intersect_madden_rhodo)[5] <- 'Location_ID'
earlydb_intersect_madden <- merge(earlydb_intersect_madden_overstory, earlydb_intersect_madden_rhodo)

# check that coordinates are the same for Location_IDs in mm database and NPS database
# mm_tbl_locations <- sqlFetch(conmmdb, 'tbl_Locations')[, c('Location_ID', 'Loc_Name', 'X_Coord', 'Y_Coord')]
# early_tbl_locations <- sqlFetch(con1, 'tbl_Locations')[, c('Location_ID', 'Loc_Name', 'X_Coord', 'Y_Coord')]
# locations_merge <- merge(mm_tbl_locations, early_tbl_locations, all=TRUE)

# Meghan's sites sampled in 2017
mm_overstory_2017 <- mm_overstory[mm_overstory$Event_Date >= "2017-01-01",
                                  intersect(colnames(mm_overstory), colnames(tbl_early_overstory))]
# intersect Meghan's sites with Madden overstory veg classification
mm_tbl_locations <- sqlFetch(conmmdb, 'tbl_Locations')[, c('Location_ID', 'Loc_Name', 'X_Coord', 'Y_Coord')]
mm_tbl_2017_locations <- mm_tbl_locations[(mm_tbl_locations$Location_ID %in%
                                      mm_overstory_2017$Location_ID), ]
write.csv(mm_tbl_2017_locations,
          "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/Meghan_Mulroy/MM_2017_sites.csv",
          row.names=FALSE)
mm_intersect_madden <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis/Madden_veg_intersect_MM_2017_sites.csv")
mm_intersect_madden <- mm_intersect_madden[, c('Location_I', 'Lumped_cla')]
colnames(mm_intersect_madden) <- c('Location_ID', 'Lumped_class')
mm_intersect_summary <- as.data.frame(table(mm_intersect_madden$Lumped_cla))

# merge NPS early database with Meghan Mulroy data
early_overstory_mm_subset <- tbl_early_overstory[(tbl_early_overstory$Location_ID %in%
                                                    mm_overstory_2017$Location_ID),
                                                 intersect(colnames(mm_overstory), colnames(tbl_early_overstory))]
comb_df <- rbind(mm_overstory_2017, early_overstory_mm_subset)

# change in relative basal area by species, 2003 - 2017
sum_basal_area_by_spp <- aggregate(Basal_Area~SpeciesCode+Event_ID+Event_Date,
                                   data=comb_df, FUN=sum)
colnames(sum_basal_area_by_spp) <- c("SpeciesCode", "Event_ID", "Event_Date", "Basal_Area_by_spp")
sum_basal_area_by_event <- aggregate(Basal_Area~Event_ID+Event_Date,
                                     data=comb_df, FUN=sum)
colnames(sum_basal_area_by_event) <- c("Event_ID", "Event_Date", "total_Basal_Area")
perc_basal_area_df <- merge(sum_basal_area_by_event, sum_basal_area_by_spp,
                            by=c("Event_ID", "Event_Date"), all=TRUE)
perc_basal_area_df$perc_basal_area <- (perc_basal_area_df$Basal_Area_by_spp /
                                         perc_basal_area_df$total_Basal_Area) * 100
perc_basal_area_df <- perc_basal_area_df[, c('Event_ID', 'Event_Date', 'SpeciesCode', 'perc_basal_area')]
hemlock_sites <- perc_basal_area_df[(perc_basal_area_df$SpeciesCode == 'TSUGCAN' |
                                       perc_basal_area_df$SpeciesCode == 'Tsugcan'), ]

p <- ggplot(hemlock_sites, aes(x=Event_Date, y=perc_basal_area))
p <- p + geom_point()
pngname = paste(imgdir, 'hemlock_rel_basal_area_by_date.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

hemlock_sites$year <- format(hemlock_sites$Event_Date, format='%Y')
mean_perc_basal_area <- aggregate(perc_basal_area~year, data=hemlock_sites, FUN=mean)
p <- ggplot(mean_perc_basal_area, aes(x=year, y=perc_basal_area))
p <- p + geom_point()
print(p)

# relative basal area of hemlock in 2003 by Madden lumped lulc
tbl_early_Events <- sqlFetch(con1, 'tbl_Events')
tbl_early_overstory <- sqlFetch(con1, 'tbl_Overstory')
tbl_early_overstory <- merge(tbl_early_Events, tbl_early_overstory, by='Event_ID')
sum_basal_area_by_spp <- aggregate(Basal_Area~SpeciesCode+Event_ID+Event_Date,
                                   data=tbl_early_overstory, FUN=sum)
colnames(sum_basal_area_by_spp) <- c("SpeciesCode", "Event_ID", "Event_Date", "Basal_Area_by_spp")
sum_basal_area_by_event <- aggregate(Basal_Area~Event_ID+Event_Date,
                                     data=tbl_early_overstory, FUN=sum)
colnames(sum_basal_area_by_event) <- c("Event_ID", "Event_Date", "total_Basal_Area")
perc_basal_area_df <- merge(sum_basal_area_by_event, sum_basal_area_by_spp,
                            by=c("Event_ID", "Event_Date"), all=TRUE)
perc_basal_area_df$perc_basal_area <- (perc_basal_area_df$Basal_Area_by_spp /
                                         perc_basal_area_df$total_Basal_Area) * 100
perc_basal_area_df <- perc_basal_area_df[, c('Event_ID', 'Event_Date', 'SpeciesCode', 'perc_basal_area')]
hemlock_sites <- perc_basal_area_df[(perc_basal_area_df$SpeciesCode == 'TSUGCAN' |
                                       perc_basal_area_df$SpeciesCode == 'Tsugcan'), ]
early_hemlock_rel_ba <- hemlock_sites[hemlock_sites$Event_Date < "2006-01-01", ]
early_hemlock_rel_ba <- merge(early_hemlock_rel_ba, tbl_early_Events)[, c(colnames(early_hemlock_rel_ba),
                                                                   'Location_ID')]
early_hemlock_rel_ba <- merge(early_hemlock_rel_ba, earlydb_intersect_madden)
p <- ggplot(early_hemlock_rel_ba, aes(x=Lumped_class, y=perc_basal_area))
p <- p + geom_boxplot()
print(p)
p <- p + xlab("Lumped landcover class") + ylab("Relative basal area of hemlock (%)")
pngname = paste(imgdir, 'hemlock_2003_rel_basal_area_all_sites_by_lumped_lulc.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

p <- ggplot(early_hemlock_rel_ba, aes(perc_basal_area))
p <- p + geom_histogram()
p <- p + xlab("Relative basal area of hemlock (%)") + ylab("# sites")
print(p)
pngname = paste(imgdir, 'hemlock_2003_rel_basal_area_hist.png', sep='/')
png(file=pngname, units="in", res=400, width=4, height=3)
print(p)
dev.off()

# total non-hemlock basal area
non_hemlock <- sum_basal_area_by_spp[sum_basal_area_by_spp$SpeciesCode != 'TSUGCAN' &
                                       sum_basal_area_by_spp$SpeciesCode != 'Tsugcan', ]
sum_non_hemlock <- aggregate(Basal_Area_by_spp~Event_ID+Event_Date,
                             data=non_hemlock, FUN=sum)
colnames(sum_non_hemlock) <- c("Event_ID", "Event_Date", "total_Basal_Area")
p <- ggplot(sum_non_hemlock, aes(x=Event_Date, y=total_Basal_Area))
p <- p + geom_point()
print(p)
pngname = paste(imgdir, 'non_hemlock_sum_basal_area_by_date.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

sum_non_hemlock$year <- format(sum_non_hemlock$Event_Date, format='%Y')
mean_sum_basal_area <- aggregate(total_Basal_Area~year, data=sum_non_hemlock, FUN=mean)
p <- ggplot(mean_perc_basal_area, aes(x=year, y=total_Basal_Area))
p <- p + geom_point()
print(p)

## change in sapling density
mm_saplings <- sqlFetch(conmmdb, 'tbl_Saplings')
mm_saplings <- merge(mm_saplings, mm_events, by='Event_ID')
mm_saplings$Event_Date <- as.Date(mm_saplings$Event_Date, format="%Y-%m-%d")
mm_sapling_2017 <- mm_saplings[mm_saplings$Event_Date > "2017-01-01", ]
# detect # subsamples from unique values of modules within event id
n_subset_det <- aggregate(Module~Event_ID, data=mm_sapling_2017, function(x) length(unique(x)))
colnames(n_subset_det) <- c('Event_ID', 'n_modules')
mm_sapling_2017 <- merge(mm_sapling_2017, n_subset_det)

tbl_early_saplings <- sqlFetch(con1, 'tbl_Saplings')
tbl_early_saplings <- merge(tbl_early_saplings, tbl_early_Events, by='Event_ID')
early_saplings_mm_subset <- tbl_early_saplings[(tbl_early_saplings$Location_ID %in%
                                                    mm_sapling_2017$Location_ID), ]
n_subset_det <- aggregate(Module~Event_ID, data=early_saplings_mm_subset, function(x) length(unique(x)))
colnames(n_subset_det) <- c('Event_ID', 'n_modules')
early_saplings_mm_subset <- merge(early_saplings_mm_subset, n_subset_det)
early_saplings_mm_subset <- early_saplings_mm_subset[, intersect(
  colnames(early_saplings_mm_subset), colnames(mm_sapling_2017))]
mm_sapling_2017 <- mm_sapling_2017[, intersect(
  colnames(early_saplings_mm_subset), colnames(mm_sapling_2017))]
comb_sapling_df <- rbind(mm_sapling_2017, early_saplings_mm_subset)
comb_events_df <- comb_sapling_df[, c('Event_ID', 'Location_ID', 'Event_Date')]
comb_events_df <- comb_events_df[!duplicated(comb_events_df), ]

# sum up all saplings across size classes
comb_sapling_df$stem_total <- comb_sapling_df$DClass1 + comb_sapling_df$DClass2 +
  comb_sapling_df$DClass3 + comb_sapling_df$DClass4
sum_stem <- aggregate(stem_total~Event_ID+n_modules, data=comb_sapling_df, FUN=sum)
sum_stem$sapling_stem_density <- sum_stem$stem_total / sum_stem$n_modules
sapling_density <- sum_stem[, c('Event_ID', 'sapling_stem_density')]
sapling_density <- merge(sapling_density, comb_events_df)

p <- ggplot(sapling_density, aes(x=Event_Date, y=sapling_stem_density))
p <- p + geom_point()
print(p)
pngname = paste(imgdir, 'sapling_stem_density_by_date.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

# rhododendron stems only, in early db
tbl_early_saplings <- sqlFetch(con1, 'tbl_Saplings')
tbl_early_saplings <- merge(tbl_early_saplings, tbl_early_Events, by='Event_ID')
sapling_events <- tbl_early_saplings[!(duplicated(tbl_early_saplings$Event_ID)),
                                     c('Event_ID', 'Location_ID', 'Event_Date')]
n_subset_det <- aggregate(Module~Event_ID, data=tbl_early_saplings, function(x) length(unique(x)))
colnames(n_subset_det) <- c('Event_ID', 'n_modules')
tbl_early_saplings <- merge(tbl_early_saplings, n_subset_det)
early_rhodo_saplings <- tbl_early_saplings[(tbl_early_saplings$SpeciesCode == 'Rhodmax' |
                                              tbl_early_saplings$SpeciesCode == 'rhodmax' |
                                              tbl_early_saplings$SpeciesCode == 'RHODMAX'), ]
early_rhodo_saplings$stem_total <- early_rhodo_saplings$DClass1 + early_rhodo_saplings$DClass2 +
  early_rhodo_saplings$DClass3 + early_rhodo_saplings$DClass4
sum_stem <- aggregate(stem_total~Event_ID+n_modules, data=early_rhodo_saplings, FUN=sum)
sum_stem$rhodo_sapling_stems_per_module <- sum_stem$stem_total / sum_stem$n_modules
rhodo_density <- sum_stem[, c('Event_ID', 'rhodo_sapling_stems_per_module')]
rhodo_density <- merge(rhodo_density, sapling_events, all.y=TRUE)
rhodo_density[is.na(rhodo_density$rhodo_sapling_stems_per_module),
              'rhodo_sapling_stems_per_module'] <- 0
rhodo_density <- rhodo_density[!(duplicated(rhodo_density$Location_ID)), ] # one measurement per site

# density of rhodo stems by Madden understory classification
earlydb_intersect_madden[earlydb_intersect_madden$rhodo == 0, 'rhodo'] <- 'absent'
earlydb_intersect_madden[earlydb_intersect_madden$rhodo == 1, 'rhodo'] <- 'present'
earlydb_intersect_madden$rhodo <- as.factor(earlydb_intersect_madden$rhodo)
earlydb_rhodo_density <- merge(rhodo_density, earlydb_intersect_madden)

p <- ggplot(earlydb_rhodo_density, aes(x=rhodo, y=rhodo_sapling_stems_per_module))
p <- p + geom_boxplot()
print(p)
pngname = paste(imgdir, 'earlydb_rhodo_stem_density_by_Madden_understory_rhodo_presence.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

p <- ggplot(earlydb_rhodo_density, aes(x=Dens_Class, y=rhodo_sapling_stems_per_module))
p <- p + geom_boxplot()
print(p)
pngname = paste(imgdir, 'earlydb_rhodo_stem_density_by_Madden_understory_DensClass.png', sep='/')
png(file=pngname, units="in", res=400, width=10, height=3)
print(p)
dev.off()

# NPS database, post-Jenkins protocol
latedb <- "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/2019_databases/2015_2018_data/GRSMLTVegMonCertified_20181220.accdb"
conldb <- odbcConnectAccess2007(latedb)
latedb_tables <- sqlTables(conldb, tableType="TABLE")$TABLE_NAME

# all sampling dates and locations
loc_select <- "SELECT tbl_Locations.LOC_NAME, tbl_Locations.X_COORD, tbl_Locations.Y_COORD, tbl_Events.Event_Date, tbl_Overstory.*
  FROM (tbl_Locations INNER JOIN tbl_Events ON tbl_Locations.GLOBALID = tbl_Events.Global_ID) INNER JOIN tbl_Overstory ON tbl_Events.Event_ID = tbl_Overstory.Event_ID"
latedb_overstory <- sqlQuery(conldb, loc_select)
latedb_locations <- latedb_overstory[!(duplicated(latedb_overstory$LOC_NAME)), c('LOC_NAME', 'X_COORD', 'Y_COORD')]
write.csv(latedb_locations, "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/2019_databases/GK_process/2015_2018_locations.csv")
latedb_events <- latedb_overstory[!(duplicated(latedb_overstory$Event_ID)), c('LOC_NAME', 'Event_ID')]

# latedb locations joined with Madden overstory and understory
latedb_Madden_join <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis/Madden_veg_intersect_latedb_sites.csv")
colnames(latedb_Madden_join)[48] <- 'Lumped_class'
latedb_Madden_join[latedb_Madden_join$rhodo == 0, 'rhodo'] <- 'absent'
latedb_Madden_join[latedb_Madden_join$rhodo == 1, 'rhodo'] <- 'present'

# combine early and late db, count # sites in each Madden lumped class
latedb_Madden_join_subs <- latedb_Madden_join[, c('LOC_NAME', 'Lumped_class')]
colnames(latedb_Madden_join_subs)[1] <- 'Location_ID'
comb_db_madden <- rbind(earlydb_intersect_madden_overstory, latedb_Madden_join_subs)
table(comb_db_madden$Lumped_class)

# how many sites in late db in each Madden lumped class?
overstory_madden_join <- merge(latedb_overstory, latedb_Madden_join)
overstory_madden_join <- overstory_madden_join[!(duplicated(overstory_madden_join$LOC_NAME)), ]
n_sites_by_lumped_class <- as.data.frame(table(overstory_madden_join$Lumped_class))

# basal area of hemlock in overstory
sum_basal_area_by_spp <- aggregate(Basal_Area~SpeciesCode+Event_ID+Event_Date,
                                   data=latedb_overstory, FUN=sum)
colnames(sum_basal_area_by_spp) <- c("SpeciesCode", "Event_ID", "Event_Date", "Basal_Area_by_spp")
sum_basal_area_by_event <- aggregate(Basal_Area~Event_ID+Event_Date,
                                     data=latedb_overstory, FUN=sum)
colnames(sum_basal_area_by_event) <- c("Event_ID", "Event_Date", "total_Basal_Area")
perc_basal_area_df <- merge(sum_basal_area_by_event, sum_basal_area_by_spp,
                            by=c("Event_ID", "Event_Date"), all=TRUE)
perc_basal_area_df$perc_basal_area <- (perc_basal_area_df$Basal_Area_by_spp /
                                         perc_basal_area_df$total_Basal_Area) * 100
perc_basal_area_df <- perc_basal_area_df[, c('Event_ID', 'Event_Date', 'SpeciesCode', 'perc_basal_area')]
late_hemlock_rel_ba <- perc_basal_area_df[(perc_basal_area_df$SpeciesCode == 'TSUGCAN' |
                                       perc_basal_area_df$SpeciesCode == 'Tsugcan'), ]
late_hemlock_rel_ba <- merge(late_hemlock_rel_ba, latedb_events)[, c(colnames(late_hemlock_rel_ba),
                                                                   'LOC_NAME')]
late_hemlock_rel_ba <- merge(late_hemlock_rel_ba, latedb_Madden_join, by='LOC_NAME')
# distribution of late hemlock basal area in Madden lumped lulc classes
p <- ggplot(late_hemlock_rel_ba, aes(x=Lumped_class, y=perc_basal_area))
p <- p + geom_boxplot()
print(p)
p <- p + xlab("Lumped landcover class") + ylab("Relative basal area")
pngname = paste(imgdir, 'hemlock_2015-18_rel_basal_area_all_sites_by_lumped_lulc.png', sep='/')
png(file=pngname, units="in", res=400, width=5, height=3)
print(p)
dev.off()

# density of rhodo from saplings table
sapling_select <- "SELECT tbl_Locations.LOC_NAME, tbl_Events.Event_Date, tbl_Saplings.*
  FROM (tbl_Locations INNER JOIN tbl_Events ON tbl_Locations.GLOBALID = tbl_Events.Global_ID) INNER JOIN tbl_Saplings ON tbl_Events.Event_ID = tbl_Saplings.Event_ID"
latedb_saplings <- sqlQuery(conldb, sapling_select)
latedb_sapling_events <- latedb_saplings[!(duplicated(latedb_saplings$Event_ID)),
                                         c('Event_ID', 'LOC_NAME', 'Event_Date')]
n_subset_det <- aggregate(Module~Event_ID, data=latedb_saplings, function(x) length(unique(x)))
colnames(n_subset_det) <- c('Event_ID', 'n_modules')
latedb_saplings <- merge(latedb_saplings, n_subset_det)
late_rhodo_saplings <- latedb_saplings[(latedb_saplings$SpeciesCode == 'RHODARB' |
                                        latedb_saplings$SpeciesCode == 'RHODCAL' |
                                        latedb_saplings$SpeciesCode == 'RHODMAX'), ]
late_rhodo_saplings$stem_total <- late_rhodo_saplings$DClass1 + late_rhodo_saplings$DClass2 +
  late_rhodo_saplings$DClass3 + late_rhodo_saplings$DClass4
sum_stem <- aggregate(stem_total~Event_ID+n_modules, data=late_rhodo_saplings, FUN=sum)
sum_stem$rhodo_sapling_stems_per_module <- sum_stem$stem_total / sum_stem$n_modules
latedb_rhodo_density <- sum_stem[, c('Event_ID', 'rhodo_sapling_stems_per_module')]
latedb_rhodo_density <- merge(latedb_rhodo_density, latedb_sapling_events, all.y=TRUE)
latedb_rhodo_density <- latedb_rhodo_density[!(duplicated(latedb_rhodo_density$LOC_NAME)), ] # one measurement per site
latedb_rhodo_density[is.na(latedb_rhodo_density$rhodo_sapling_stems_per_module),
                     'rhodo_sapling_stems_per_module'] <- 0

# rhodo density by Madden understory classes
latedb_rhodo_density <- merge(latedb_rhodo_density, latedb_Madden_join)
latedb_rhodo_density <- latedb_rhodo_density[, intersect(colnames(latedb_rhodo_density),
                                             colnames(earlydb_rhodo_density))]
earlydb_rhodo_density <- earlydb_rhodo_density[, intersect(colnames(latedb_rhodo_density),
                                             colnames(earlydb_rhodo_density))]
combined_rhodo_density <- rbind(latedb_rhodo_density, earlydb_rhodo_density)

p <- ggplot(combined_rhodo_density, aes(rhodo_sapling_stems_per_module))
p <- p + geom_histogram(binwidth=1)
print(p)

combined_rhodo_density[combined_rhodo_density$rhodo == 0, 'rhodo'] <- 'absent'
combined_rhodo_density[combined_rhodo_density$rhodo == 1, 'rhodo'] <- 'present'
p <- ggplot(combined_rhodo_density, aes(x=rhodo, y=rhodo_sapling_stems_per_module))
p <- p + geom_boxplot()
print(p)
p <- p + xlab("Rhododendron classification") + ylab("Rhododendron stems per module")
pngname = paste(imgdir, 'combineddb_rhodo_stem_density_by_Madden_understory_rhodo_presence.png', sep='/')
png(file=pngname, units="in", res=400, width=4, height=3)
print(p)
dev.off()

mean_rhodo <- aggregate(rhodo_sapling_stems_per_module~rhodo,
                        data=combined_rhodo_density, FUN=mean)
sd_rhodo <- aggregate(rhodo_sapling_stems_per_module~rhodo,
                        data=combined_rhodo_density, FUN=sd)

p <- ggplot(combined_rhodo_density, aes(x=Dens_Class, y=rhodo_sapling_stems_per_module))
p <- p + geom_boxplot()
print(p)
pngname = paste(imgdir, 'combineddb_rhodo_stem_density_by_Madden_understory_DensClass.png', sep='/')
png(file=pngname, units="in", res=400, width=12, height=4)
print(p)
dev.off()

mean_rhodo_density_by_dens_class <- aggregate(rhodo_sapling_stems_per_module~Dens_Class,
                                 data=combined_rhodo_density, FUN=mean)
colnames(mean_rhodo_density_by_dens_class)[2] <- 'mean_rhodo_sapling_stems_per_module'
sd_rhodo_density_by_dens_class <- aggregate(rhodo_sapling_stems_per_module~Dens_Class,
                                              data=combined_rhodo_density, FUN=sd)
colnames(sd_rhodo_density_by_dens_class)[2] <- 'sd_rhodo_sapling_stems_per_module'
n_sites_by_dens_class <- as.data.frame(table(combined_rhodo_density$Dens_Class))
colnames(n_sites_by_dens_class) <- c('Dens_Class', 'n_sites')
rhodo_by_dens_class <- merge(mean_rhodo_density_by_dens_class, n_sites_by_dens_class)
rhodo_by_dens_class <- merge(rhodo_by_dens_class, sd_rhodo_density_by_dens_class)
write.csv(rhodo_by_dens_class,
          "C:/Users/ginge/Dropbox/NatCap_backup/USFS/NPS_veg_monitoring/GK_analysis/combineddb_rhodo_sapling_stems_by_Madden_density_class.csv",
          row.names=FALSE)

###########################################################################
# calculate min and max total annual precip for precip sensitivity analysis
calc_perc_ch_down <- function(values) {
  return((mean(values) - min(values)) / mean(values))
}
calc_perc_ch_up <- function(values) {
  return((max(values) - mean(values)) / mean(values))
}

annual_precip_csv <- "C:/Users/ginge/Documents/NatCap/GIS_local/USFS/NCEI_climate/GSOY_precip.csv"
ann_precip <- read.csv(annual_precip_csv)
perc_change_down_df <- aggregate(PRCP~NAME, data=ann_precip,
                                 FUN=calc_perc_ch_down)
colnames(perc_change_down_df)[2] <- 'perc_change_down'
perc_change_up_df <- aggregate(PRCP~NAME, data=ann_precip,
                                 FUN=calc_perc_ch_up)
colnames(perc_change_up_df)[2] <- 'perc_change_up'
perc_ch_df <- merge(perc_change_down_df, perc_change_up_df)

# steve norman's analysis of ForWarn NDVI
points <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/materials_from_Steve_Norman/GK_reanalysis/forwarn_points_intersect_lulc.csv")
values <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/materials_from_Steve_Norman/GK_reanalysis/forwarn_values.csv")
forwarn_dat <- merge(points, values, all.y=FALSE)

# code yearly winter dates as chosen by Steve Norman
period_df <- data.frame('period'=colnames(values)[2:length(colnames(values))],
                        stringsAsFactors=FALSE)
period_df$year <- gsub("A", "", period_df$period)
out <- strsplit(period_df$year, "_")
period_df <- data.frame(period_df, do.call(rbind, out))
colnames(period_df) <- c('period', 'erase', 'year', 'window')
period_df <- period_df[, c('period', 'year', 'window')]

winter_cur <- 1:11
winter_prev <- 44:46
for(r in 1:NROW(period_df)){
  if(as.numeric(as.character(period_df[r, 'window'])) %in% winter_cur){
    period_df[r, 'winter_year'] <- as.numeric(as.character(period_df[r, 'year']))
  }
  if(as.numeric(as.character(period_df[r, 'window'])) %in% winter_prev){
    period_df[r, 'winter_year'] <- as.numeric(as.character(period_df[r, 'year'])) + 1
  }
}

# make time series and compare: (visualize mean within groups, and some measure
# of variability within group around the mean)
# whole watershed (all rows in forwarn dat)
# hemlock decline
# hemlock decline with rhodo, without rhodo

# select only winter records, code as winter year
row.names(values) <- values$id
values <- values[, c(2:dim(values)[2])]
values_t <- data.frame(t(values))
values_t$period <- row.names(values_t)
per_subs <- period_df[, c('period', 'winter_year')]
values_t <- merge(values_t, per_subs)
values_t <- values_t[, colnames(values_t)[2:length(colnames(values_t))]]
winter_vals <- values_t[!is.na(values_t$winter_year) &
                          values_t$winter_year >= 2001, ]  # select only winter values starting in 2001
winter_max <- aggregate(winter_vals, by=list(winter_vals$winter_year),
                        FUN=max)  # max NDVI within each winter for each pixel
row.names(winter_max) <- winter_max$Group.1
winter_max <- winter_max[, colnames(winter_max)[2:length(colnames(winter_max))]]
winter_maxt <- data.frame(t(winter_max))
winter_maxt <- winter_maxt[1:(dim(winter_maxt)[1]-1), ]  # get rid of winter year row
winter_maxt$id <- row.names(winter_maxt)
winter_maxt$id <- as.numeric(as.character(gsub("X", "", winter_maxt$id)))
points <- points[, c("id", "lulc_inter")]
winter_maxt <- merge(winter_maxt, points)

# summarize
# all points, data only
year <- as.numeric(as.character(gsub("X", "", colnames(winter_maxt)[2:17])))
all_points <- winter_maxt[, colnames(winter_maxt)[2:17]]
q25_watershed <- sapply(all_points, quantile, probs=c(0.25),
                        na.rm=TRUE, names=FALSE)
q75_watershed <- sapply(all_points, quantile, probs=c(0.75),
                        na.rm=TRUE, names=FALSE)
mean_watershed <- sapply(all_points, mean,
                         na.rm=TRUE, names=FALSE)
watershed_df <- data.frame('year'=year, 'q25'=q25_watershed,
                           'q75'=q75_watershed, 'NDVI'=mean_watershed,
                          'lulc_group'='watershed')

hemlock <- winter_maxt[winter_maxt$lulc_inter == 'decline_no_rhodo' &
                         winter_maxt$lulc_inter == 'decline_with_rhodo',
                       colnames(winter_maxt)[2:17]]  # hemlock decline
q25_hemlock <- sapply(hemlock, quantile, probs=c(0.25),
                        na.rm=TRUE, names=FALSE)
q75_hemlock <- sapply(hemlock, quantile, probs=c(0.75),
                        na.rm=TRUE, names=FALSE)
mean_hemlock <- sapply(hemlock, mean,
                         na.rm=TRUE, names=FALSE)
hemlock_df <- data.frame('year'=year, 'q25'=q25_hemlock,
                           'q75'=q75_hemlock, 'NDVI'=mean_hemlock,
                           'lulc_group'='hemlock_decline')
# decline without rhodo
decline_no_rhodo <- winter_maxt[winter_maxt$lulc_inter == 'decline_no_rhodo',
                                colnames(winter_maxt)[2:17]]
q25_no_rh <- sapply(decline_no_rhodo, quantile, probs=c(0.25),
                      na.rm=TRUE, names=FALSE)
q75_no_rh <- sapply(decline_no_rhodo, quantile, probs=c(0.75),
                      na.rm=TRUE, names=FALSE)
mean_no_rh <- sapply(decline_no_rhodo, mean,
                       na.rm=TRUE, names=FALSE)
decl_no_df <- data.frame('year'=year, 'q25'=q25_no_rh,
                         'q75'=q75_no_rh, 'NDVI'=mean_no_rh,
                         'lulc_group'='decline_no_rhodo')
# decline with rhodo
decline_with_rhodo <- winter_maxt[winter_maxt$lulc_inter == 'decline_with_rhodo',
                                  colnames(winter_maxt)[2:17]]
q25_w_rh <- sapply(decline_with_rhodo, quantile, probs=c(0.25),
                    na.rm=TRUE, names=FALSE)
q75_w_rh <- sapply(decline_with_rhodo, quantile, probs=c(0.75),
                    na.rm=TRUE, names=FALSE)
mean_w_rh <- sapply(decline_with_rhodo, mean,
                     na.rm=TRUE, names=FALSE)
decl_rh_df <- data.frame('year'=year, 'q25'=q25_w_rh,
                         'q75'=q75_w_rh, 'NDVI'=mean_w_rh,
                         'lulc_group'='decline_with_rhodo')
NDVI_df <- rbind(watershed_df, decl_no_df, decl_rh_df)
NDVI_df$lulc_group <- factor(NDVI_df$lulc_group, levels=levels(NDVI_df$lulc_group),
                             labels=c("Entire watershed", "Hemlock decline with rhododendron",
                                      "Hemlock decline without rhododendron"))

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/materials_from_Steve_Norman/GK_reanalysis/figs"
p <- ggplot(NDVI_df, aes(x=year, y=NDVI))
p <- p + geom_line(aes(colour=lulc_group))
pngname <- paste(imgdir, "winter_NDVI_by_lulc.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

p <- ggplot(NDVI_df, aes(x=year, y=NDVI))
p <- p + geom_line(aes(colour=lulc_group))
p <- p + geom_ribbon(aes(ymin=q25, ymax=q75, fill=lulc_group), alpha=0.2)
p <- p + xlab("Year") + ylab("Winter NDVI")
p <- p + theme(legend.title=element_blank(), legend.position="bottom")
pngname <- paste(imgdir, "winter_NDVI_by_lulc_25-75_perc.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

p <- ggplot(NDVI_df, aes(x=year, y=NDVI))
p <- p + geom_line()
p <- p + geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.2)
p <- p + facet_wrap(~lulc_group)
pngname <- paste(imgdir, "winter_NDVI_by_lulc_facet.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()


# results: quickflow (fourth draft run)
imgdir <- "C:/Users/ginge/Documents/NatCap/GIS_local/USFS/replicate_4th_draft_12.4.18/summary/figs"
  # "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/model_runs/fourth_draft/summary_of_results/figs"
qf_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/USFS/replicate_4th_draft_12.4.18/summary/monthly_quickflow.csv")
  # read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/model_runs/fourth_draft/summary_of_results/monthly_quickflow.csv")
# raw <- qf_df[qf_df$scenario != 'difference', ]
raw <- qf_df
raw$Scenario <- factor(raw$scenario, levels=c("pre-decline", "post-decline"),
                       labels=c("Pre-decline", "Post-decline"))
p <- ggplot(raw, aes(x=month, y=sum_quickflow, group=Scenario))
p <- p + geom_point()
p <- p + geom_line(aes(linetype=Scenario))
p <- p + scale_x_continuous(breaks=seq(0, 12, by=2))
p <- p + xlab("Month") + ylab("Quickflow (mm)") + print_theme
pngname <- paste(imgdir, "monthly_quickflow_by_scenario.png", sep="/")
png(file=pngname, units="in", res=300, width=6, height=3)
print(p)
dev.off()

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
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/understory_with_rhodo_Palmer_creek_1km_buf.csv"
write.csv(mad_un, save_as, row.names=FALSE)
