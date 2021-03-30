# Moore-Amazon InVEST results for Chaglla dam watershed
library(ggplot2)
library(raster)

relative_to_baseline <- function(df) {
  df$relative_value <- 0
  for (y in unique(df$erosivity)) {
    for (id in unique(df$response)) {
      id_val <- df[(df$response == id) & (df$erosivity == y), 'value']  #  
      baseline_val <- df[(df$response == id) & (df$erosivity == y) & (df$year == 2015), 'value']
      df[(df$response == id) & (df$erosivity == y), 'relative_value'] <- (id_val - baseline_val) / baseline_val * 100  # 
    }
  }
  return(df)
}

# plot histograms of difference values?
diff_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/SDR/sed_export_diff_from_baseline"
eg_path <- paste(diff_dir, "diff_2070_2.6.tif", sep='/')
raster_vals <- raster(eg_path)
histvals <- hist(raster_vals, maxpixels=100000000000, plot=FALSE)

# examine precip that was used to run SWY
precip_summary <- read.csv("C:/SWY_workspace/precip_summary.csv")
precip_summary$scenario <- factor(precip_summary$scenario, levels=c('current', 2.6, 6, 8.5),
                                  labels=c('current', 'RCP2.6-SSP1', 'RCP6.0-SSP4', 'RCP8.5-SSP5'))
p <- ggplot(precip_summary, aes(x=month_p, y=precip, color=scenario))
p <- p + geom_line() + facet_wrap(~year) + ylab("Monthly precip sum")
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/summary_figs/monthly_precip_summary.png"
png(file=pngname, units="in", res=300, width=6, height=2.5)
print(p)
dev.off()

# calculate future rain events tables, assuming same amount of precip per event as current
precip_summary <- read.csv("C:/SWY_workspace/precip_summary.csv")
current_rain_events <- read.csv('C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_data_inputs/rain_events_Chaglla_centroid_iwmi_water_atlas.csv')
precip_per_event <- merge(current_rain_events,
                          precip_summary[(precip_summary$scenario == 'current') & (precip_summary$year == 2050),
                                         c('month_p', 'precip')],
                          by.x='month', by.y='month_p')
precip_per_event$pr_per_ev <- precip_per_event$precip / precip_per_event$events
precip_per_event <- precip_per_event[, c('month', 'pr_per_ev')]
rain_events_outer_dir <- "F:/Moore_Amazon_backups/rain_events"
# dir.create(rain_events_outer_dir)
for (year in c(2050, 2070)){
  for (rcp in c('2.6', '6', '8.5')){
    precip_subs <- 
    events_table <- merge(precip_per_event, precip_summary[(precip_summary$scenario == rcp) & (precip_summary$year == year),
                                                           c('month_p', 'precip')],
                          by.x='month', by.y='month_p')
    events_table$events <- events_table$precip / events_table$pr_per_ev
    events_table <- events_table[, c('month', 'events')]
    out_path <- paste(rain_events_outer_dir, paste('year_', year, '_rcp_', rcp, '.csv', sep=''), sep='/')
    write.csv(events_table, out_path, row.names=FALSE)
  }
}

# SWY: streamflow. Annual baseflow was distributed across months according to relative precip in the prior month
swy_summary <- read.csv("C:/SWY_workspace/seasonal_streamflow_summary.csv")
swy_summary$scenario <- factor(swy_summary$scenario, levels=c('current', '2.6', '6', '8.5'),
                               labels=c('current', 'RCP2.6-SSP1', 'RCP6.0-SSP4', 'RCP8.5-SSP5'))

swy_subs <- swy_summary[swy_summary$CN_option == 'CN-III', 
                        c('month', 'baseflow', 'quickflow', 'scenario', 'year')]  # TODO keep both CN-II and CN-III?
swy_res1 <- reshape(swy_subs, varying=c('baseflow', 'quickflow'),
                    v.names='value', idvar=c('month', 'scenario', 'year'),
                    times=c('baseflow', 'quickflow'),
                    timevar='response', direction='long')
swy_res1$fraction <- factor(swy_res1$response, levels=c('quickflow', 'baseflow'))
swy_res1$scenario_numeric <- 0
swy_res1[swy_res1$scenario == 'current', 'scenario_numeric'] <- 1
swy_res1[swy_res1$scenario == 'RCP2.6-SSP1', 'scenario_numeric'] <- 2
swy_res1[swy_res1$scenario == 'RCP6.0-SSP4', 'scenario_numeric'] <- 3
swy_res1[swy_res1$scenario == 'RCP8.5-SSP5', 'scenario_numeric'] <- 4
p <- ggplot(swy_res1, aes(x=scenario, y=value, fill=fraction))
p <- p + geom_bar(position='stack', stat='identity')
p <- p + facet_grid(year~month) + xlab("") + ylab("flow (m3/sec)")
p <- p + theme(panel.spacing.x=unit(0, 'lines'), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/summary_figs/swy_flow_summary_stack_bar.png"
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

swy_resh <- reshape(swy_summary, varying=c('baseflow', 'quickflow', 'streamflow'),
                    v.names='value', idvar=c('month', 'scenario', 'year'),
                    times=c('baseflow', 'quickflow', 'streamflow'),
                    timevar='response', direction='long')
p <- ggplot(swy_resh, aes(x=month, y=value, color=scenario))
p <- p + geom_line() + facet_grid(response~year, scales='free')
p <- p + scale_x_continuous(breaks=seq(2, 12, 2))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/summary_figs/swy_baseflow_quickflow_summary.png"
png(file=pngname, units="in", res=300, width=6, height=5)
print(p)
dev.off()

# SDR: 3 scenarios, 3 time periods
# using erosivity calculated via Riquetti (annual precip * 2.2)
sdr_summary <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/SDR/watershed_results_summary.csv")
sdr_res <- reshape(sdr_summary, varying=c('usle_tot', 'sed_export', 'sed_retent', 'sed_dep'),
                   v.names='value', idvar=c('WS_ID', 'year', 'scenario', 'erosivity'),
                   times=c('usle_tot', 'sed_export', 'sed_retent', 'sed_dep'),
                   timevar='response', direction='long')
sdr_rel <- relative_to_baseline(sdr_res)

sdr_rel$scenario <- factor(sdr_rel$scenario, levels=c(2.6, 6, 8.5),
                           labels=c('RCP2.6-SSP1', 'RCP6.0-SSP4', 'RCP8.5-SSP5'))
sdr_rel$erosivity <- factor(sdr_rel$erosivity, levels=c('Riquetti_current', 'Riquetti_future_precip'),
                            labels=c('Erosivity: current', 'Erosivity: future precip'))
sdr_rel$response <- factor(sdr_rel$response, levels=c('usle_tot', 'sed_export', 'sed_retent', 'sed_dep'))

p <- ggplot(sdr_rel, aes(x=year, y=relative_value))
p <- p + geom_line(aes(colour=scenario)) + facet_grid(response~erosivity, scales='free')
p <- p + ylab("Percent change from current")
p <- p + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/summary_figs/sdr_future_summary_with_without_erosivity_change.png"
png(file=pngname, units="in", res=300, width=6, height=7)
print(p)
dev.off()

sdr_restr <- sdr_rel[sdr_rel$erosivity == 'Erosivity: future precip', ]
p <- ggplot(sdr_restr, aes(x=year, y=relative_value))
p <- p + geom_line(aes(colour=scenario)) + facet_wrap(~response, scales='free')
p <- p + ylab("Percent change from current")
p <- p + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/SDR_SWY_results/summary_figs/sdr_future_summary.png"
png(file=pngname, units="in", res=300, width=6, height=5)
print(p)
dev.off()

# investigate querying Worldclim data directly
library(raster)
?getData()

# process GRAND dams data
grand_dbf <- "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/GRanD_Version_1_3/GRanD_dams_v1_3.csv"
grand_df <- read.csv(grand_dbf)
brazil_ecuador_peru_df <- grand_df[(grand_df$COUNTRY.C.50 == 'Brazil') |
                                     (grand_df$COUNTRY.C.50 == 'Ecuador') |
                                     (grand_df$COUNTRY.C.50 == 'Peru'), ]
write.csv(brazil_ecuador_peru_df, "C:/Users/ginge/Dropbox/NatCap_backup/Moore_Amazon/GRanD_Brazil_Ecuador_Peru.csv",
          row.names=FALSE)
dams_df <- read.csv("G:/Shared drives/Moore Amazon Hydro/dam_subset_black_orange.csv")

# summarize farm size distribution in watersheds
ws_full_df <- read.csv("G:/Shared drives/Moore Amazon Hydro/1_base_data/Other/watershed_characteristics/watershed_characteristics_combined.csv",
                       sep=';')
ws_full_df <- ws_full_df[, c('fid', 'area_ha')]
size_intersect_df <- read.csv("C:/Users/ginge/Desktop/Farm_size_Samberg_etal_2016/watershed_farm_size_intersect.csv")
total_area_intersect <- aggregate(area_ha~fid, data=size_intersect_df, FUN=sum)
colnames(total_area_intersect) <- c('fid', 'total_area_intersect')
total_area_df <- merge(ws_full_df, total_area_intersect)
total_area_df$perc_area_in_size_dataset <- total_area_df$total_area_intersect / total_area_df$area_ha
total_area_df <- total_area_df[, c('fid', 'total_area_intersect', 'perc_area_in_size_dataset')]
sum_area_by_size <- aggregate(area_ha~fid + Jun7_cat, data=size_intersect_df, FUN=sum)
colnames(sum_area_by_size) <- c('fid', 'Jun7_cat', 'area_ha_size_category')
area_merge <- merge(total_area_df, sum_area_by_size, all=TRUE)
area_merge$perc_area_size_category <- area_merge$area_ha_size_category / area_merge$total_area_intersect
long_df <- area_merge[, c('fid', 'Jun7_cat', 'perc_area_size_category')]
wide_df <- reshape(long_df, idvar='fid', timevar='Jun7_cat', v.names='perc_area_size_category', direction='wide')
total_area_df <- total_area_df[, c('fid', 'perc_area_in_size_dataset')]
summary_df <- merge(total_area_df, wide_df)
summary_df[is.na(summary_df)] <- 0
write.csv(summary_df, "G:/Shared drives/Moore Amazon Hydro/1_base_data/Other/watershed_characteristics/farm_size_summary.csv",
          row.names=FALSE)
