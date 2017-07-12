# OPC ticks pathogen data

pathogen_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Brian/Kenyan Tick Pathogen Report 20170706.csv",
                        stringsAsFactors=FALSE)
pathogen_df$infected <- 0
for(r in 1:NROW(pathogen_df)){
  if(pathogen_df[r, 'fluidigm.'] == 'ND'){
    pathogen_df[r, 'infected'] <- 'NA'
    next
  }
  if(pathogen_df[r, "Pathogen"] != ""){
    pathogen_df[r, 'infected'] <- 1
  }
}
pathogen_df <- pathogen_df[pathogen_df$Site == 'Ol Pejeta  ' |
                             pathogen_df$Site == 'Ol Pejeta' |
                             pathogen_df$Site == 'Ol Pejeta_extra', ]
pathogen_df$transect <- paste(pathogen_df$Date, pathogen_df$Code, sep="_")
pathogen_df$transect[pathogen_df$transect == "12-Nov-14_LMD1"] <- "11-Nov-14_LM1"
pathogen_df$transect[pathogen_df$transect == "12-Nov-14_MH3"] <- "13-Nov-14_MH3"
write.csv(pathogen_df, "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_pathogen.csv",
          row.names=FALSE)

tick_metadata <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/KWS_samples for export_finalv2.csv")
tick_metadata <- tick_metadata[tick_metadata$Site == 'Ol Pejeta  ' |
                                 tick_metadata$Site == 'Ol Pejeta' |
                                 tick_metadata$Site == 'Ol Pejeta_extra', ]
tick_metadata$transect <- paste(tick_metadata$Date, tick_metadata$Code, sep="_")
write.csv(tick_metadata, "C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_transect_metadata.csv",
          row.names=FALSE)

# combine, aggregate, calculate average by sampling period / management zone
pathogen_df <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_pathogen.csv")
pathogen_metadata <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_transect_metadata.csv")
pathogen_metadata <- pathogen_metadata[!duplicated(pathogen_metadata$transect), ]
pathogen_metadata <- pathogen_metadata[, c(1, 3, 16, 17)]
pathogen_df <- pathogen_df[, c(43:44)]
pathogen_df <- merge(pathogen_df, pathogen_metadata, by="transect",
                        all.x=TRUE, all.y=FALSE)

samp_per <- pathogen_df[, c("Date", "Code")]
samp_per$transect <- paste(samp_per$Date, samp_per$Code, sep="_")
samp_per$Date <- as.Date(samp_per$Date, format="%d-%b-%y")
samp_per$year_month <- format(samp_per$Date, "%Y-%m")
samp_per <- samp_per[!duplicated(samp_per$transect), ]
year_mo_per <- as.data.frame(unique(samp_per$year_month))
colnames(year_mo_per)[1] <- 'year_month'
year_mo_per$sampling_period <- c(0, 1, 2, 3, 3, 4, 5, 6, 7, 8, 9)
samp_per <- merge(samp_per, year_mo_per, by='year_month')
samp_per <- samp_per[, c('transect', 'sampling_period')]
pathogen_df <- merge(pathogen_df, samp_per, by='transect', all.x=TRUE)
pathogen_df$zone_period <- paste(pathogen_df$mgmt_zone, as.character(pathogen_df$sampling_period),
                                    sep="-")
mean_by_zone_period <- aggregate(infected~zone_period, data=pathogen_df, FUN=mean)
