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
                             pathogen_df$Site == 'OPC extra', ]
pathogen_df$transect <- paste(pathogen_df$Date, pathogen_df$Code, sep="_")


ticks_meta_df <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_metadata.csv")
veg_meta_df <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_veg_data_9.30.16_metadata.csv")
tick_transects <- read.csv("C:/Users/Ginger/Box Sync/Kenya Fame and Fortune Starts Here Data Portal/Ol_Pej/Project files_Ol Pejeta/OPC_tick_transects.csv")
ticks_meta_df$site_transect <- paste(ticks_meta_df$Date, ticks_meta_df$Site, sep="_")
ticks_meta_df$way_transect <- paste(ticks_meta_df$Date, ticks_meta_df$waypoint, sep="_")
tick_transects$transect <- paste(tick_transects$Date, tick_transects$Site,
                                 sep="_")

# try to deduce how to match pathogen sites with metadata sites
in_a_not_b <- function(a_ids, b_ids){
  mismatch <- c()
  for (id in a_ids){
    if (!is.element(id, b_ids)){
      mismatch <- c(mismatch, id)
    }
  }
  return(mismatch)
}

pathogen_ids <- unique(pathogen_df$transect)
meta_way_ids <- ticks_meta_df$way_transect
meta_site_ids <- ticks_meta_df$site_transect
transect_ids <- tick_transects$transect

miss_1 <- in_a_not_b(pathogen_ids, meta_way_ids)
miss_2 <- in_a_not_b(pathogen_ids, meta_site_ids)
miss_3 <- in_a_not_b(pathogen_ids, transect_ids)
miss_4 <- in_a_not_b(transect_ids, meta_way_ids)
miss_5 <- in_a_not_b(transect_ids, meta_site_ids)
miss_6 <- in_a_not_b(meta_site_ids, transect_ids)
miss_7 <- in_a_not_b(meta_way_ids, transect_ids)


# reconcile tick transect metadata with veg transect metadata corrected??