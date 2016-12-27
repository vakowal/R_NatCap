# summarize dung data for Kenya Ticks
library(ggplot2)

count <- function(values){
  return(length(values))
}

metadata_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_metadata.csv"
PDM_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_PDM.csv"

meta_df <- read.csv(metadata_csv)
dung_df <- read.csv(PDM_csv)

dung_df[, 9:32][is.na(dung_df[, 9:32])] <- 0

dung_ids <- unique(dung_df[, c("Date", "Site")])

dung_df$transect <- paste(dung_df$Date, dung_df$Site, sep="_")
# make sure there are 11 samples in each transect
sample_count <- aggregate(dung_df$PDM, by=list(dung_df$transect), FUN=count)
trouble <- sample_count[which(sample_count$PDM != 11), ]  # should be 0 row

dung_sum <- aggregate(dung_df[, 9:32], by=list(dung_df$transect), FUN=sum)
colnames(dung_sum)[1] <- 'transect'

meta_df$transect <- paste(meta_df$Date, meta_df$Site, sep="_")
meta_subs <- meta_df[, c('transect', 'Lat', 'Long')]
date_only <- dung_df[, c('transect', 'Date')]
date_only <- date_only[!duplicated(date_only[, 'transect']), ]

comb_df <- merge(dung_sum, meta_subs, by='transect')
comb_df <- merge(comb_df, date_only, by='transect')
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/OPC_dung_analysis/dung_summary.csv"
write.csv(comb_df, file=save_as, row.names=TRUE)

# dung around weather stations summarized in python 
outerdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/OPC_weather_stations"
points_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/OPC_weather_stations_coordinates.csv"
group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)

points_df <- read.csv(points_file)
df_list <- list()
for(stn in points_df$Name){
  dung_file <- paste(outerdir, paste("dung_2.0km_", stn, ".csv", sep=""), sep="/")
  df <- read.csv(dung_file)
  df_sub <- df[which(df$Year == 2015), c(3:26)]
  df_means <- as.data.frame.list(colMeans(df_sub))
  df_means$site <- stn
  df_means$n_transect <- dim(df_sub)[1]
  df_list[[stn]] <- df_means
}
combined <- do.call(rbind, df_list)

means_t <- as.data.frame(t(combined[, c(1:24)]))
means_t$Abbrev <- rownames(means_t)

gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group2')]
comb <- merge(means_t, gr_subs, by='Abbrev')

gr1_means <- aggregate(comb[, 2:11], by=list(comb$Group1), FUN=mean)
gr2_means <- aggregate(comb[, 2:11], by=list(comb$Group2), FUN=mean)
colnames(gr1_means)[1] <- "group"
colnames(gr2_means)[1] <- "group"

# reshape for plotting
gr1_res <- reshape(gr1_means, idvar="group", varying=list(2:11), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:11], direction="long",
                   new.row.names=1:1000)
gr2_res <- reshape(gr2_means, idvar="group", varying=list(2:11), timevar='site',
                   v.names="mean_dung", times=colnames(gr1_means)[2:11], direction="long",
                   new.row.names=1:1000)

gr1_plot <- gr1_res[which(gr1_res$group == 'non-cattle'), ]
gr1_plot <- gr1_plot[which(gr1_plot$site %in% c('Kamok', 'Loidien',
                                                'Loirugurugu', 'Research',
                                                'Rongai gate', ' Serat gate')), ]
p <- ggplot(gr1_plot, aes(x=site, y=mean_dung))
p <- p + geom_point()
print(p)

gr2_plot <- gr2_res[which(gr2_res$group != 'cattle'), ]
gr2_plot <- gr2_plot[which(gr2_plot$site %in% c('Kamok', 'Loidien',
                                                'Loirugurugu', 'Research',
                                                'Rongai gate', ' Serat gate')), ]
p <- ggplot(gr2_plot, aes(x=site, y=mean_dung, group=group))
p <- p + geom_point(aes(colour=group))
print(p)


# summarize OPC dung data for calculating correlation with GPS-derived stocking density
dung_subs <- dung_sum[, c('transect', 'Buf', 'Cow')]

meta_df$transect <- paste(meta_df$Date, meta_df$Site, sep="_")
meta_subs <- meta_df[, c('transect', 'Lat', 'Long')]
date_only <- dung_df[, c('transect', 'Date')]
date_only <- date_only[!duplicated(date_only[, 'transect']), ]

comb_df <- merge(dung_subs, meta_subs, by='transect')
comb_df <- merge(comb_df, date_only, by='transect')
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_bovid_dung_sum.csv"
write.csv(comb_df, file=save_as, row.names=TRUE)

comb_df <- read.csv(save_as)
comb_df$bovid <- rowSums(comb_df[, c(3, 4)])

# cattle density calculated from GPS
density_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/OPC_dung_analysis/correlation_with_GPS_records"

# summarize comparison between dung and GPS-derived density
summary_df <- data.frame('distance'=numeric(6), 'days_lag'=numeric(6),
                         'num_obs_densities'=numeric(6), 'rho_cow'=numeric(6),
                         'rho_bovid'=numeric(6))
i <- 1
for(distance in c(0.1, 0.3, 0.5)){
  for(days_lag in c(28, 35)){
    density_file <- paste(density_dir, paste("GPS_density_", distance,
                                             "km_", days_lag, "days.csv", sep=""),
                          sep="/")
    dens_df <- read.csv(density_file)
    dens_df$total_anim <- rowSums(dens_df[, c(1:6, 8)])
    cor_df <- merge(comb_df, dens_df, by='row_id', all=TRUE)
    num_non_na <- dim(cor_df[!is.na(cor_df$total_anim), ])[1]
    cor_df[, 9:17][is.na(cor_df[, 9:17])] <- 0
    if(num_non_na > 1){
      rho_cow = cor.test(cor_df$Cow, cor_df$total_anim, method='spearman')[[4]]
      p <- ggplot(cor_df, aes(x=Cow, y=total_anim))
      p <- p + geom_point()
      p <- p + xlab("Cattle dung") + ylab("GPS-derived animal density")
      p <- p + ggtitle(paste(distance, " km, ", days_lag, " days (cow dung only)", sep=""))
      pngname <- paste(density_dir, 'figs',
                       paste(distance, "km_", days_lag, "days.png", sep=""), sep="/")
      png(file=pngname, units="in", res=300, width=8, height=5)
      print(p)
      dev.off()
      
      rho_bovid = cor.test(cor_df$bovid, cor_df$total_anim, method='spearman')[[4]]
    }
    else{
      rho_cow = NA
      rho_bovid = NA
    }
    summary_df[i, 'distance'] <- distance
    summary_df[i, 'days_lag'] <- days_lag
    summary_df[i, 'num_obs_densities'] <- num_non_na
    summary_df[i, 'rho_cow'] <- rho_cow
    summary_df[i, 'rho_bovid'] <- rho_bovid
    i <- i + 1
  }
}
save_as <- paste(density_dir, "correlation_summary.csv", sep='/')
write.csv(summary_df, save_as)

distance <- 0.5
days_lag <- 35
density_file <- paste(density_dir, paste("GPS_density_", distance,
                                         "km_", days_lag, "days.csv", sep=""),
                      sep="/")
dens_df <- read.csv(density_file)
dens_df$total_anim <- rowSums(dens_df[, c(1:6, 8)])
cor_df <- merge(comb_df, dens_df, by='row_id', all=TRUE)
num_non_na <- dim(cor_df[!is.na(cor_df$total_anim), ])[1]
cor_df[, 9:17][is.na(cor_df[, 9:17])] <- 0

p <- ggplot(cor_df, aes(x=total_anim, y=Buf))
p <- p + geom_point()
p <- p + xlab("GPS-derived cattle density") + ylab("Buffalo dung")
print(p)

p <- ggplot(cor_df, aes(x=Buf, y=total_anim))
p <- p + geom_point()
p <- p + ylab("GPS-derived cattle density") + xlab("Buffalo dung")
pngname <- paste(density_dir, 'figs', 'buf_dung_vs_GPS_density_0.5_km_35_days.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()