# USFS hemlock decline

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
qf_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/USFS/model_runs/fourth_draft/summary_of_results/monthly_quickflow.csv")
raw <- qf_df[qf_df$scenario != 'difference', ]
p <- ggplot(raw, aes(x=month, y=sum_quickflow, group=scenario))
p <- p + geom_point(aes(colour=scenario))
p <- p + geom_line(aes(colour=scenario))
print(p)

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
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/USFS/input_data/Madden_landcover_summary/understory_with_rhodo.csv"
write.csv(mad_un, save_as, row.names=FALSE)
