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

work_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Brian/pathogen_data_metadata_mismatch"
sample_ids <- read.csv(paste(work_dir, "KWS_samples for export_finalv2.csv", sep="/"))
sample_ids$transect <- paste(sample_ids$Date, sample_ids$Code, sep="_")
write.csv(sample_ids, paste(work_dir, "sample_ids_with_transect.csv", sep="/"),
          row.names=FALSE)

sample_ids <- sample_ids[sample_ids$Site == 'Ol Pejeta  ' |
                           sample_ids$Site == 'Ol Pejeta' |
                           sample_ids$Site == 'Ol Pejeta_extra', ]
sample_ids <- sample_ids[!is.na(sample_ids$Code), ]
sample_id_transects <- sample_ids$transect
missing_from_meta <- in_a_not_b(pathogen_ids, sample_id_transects)
missing_from_path <- in_a_not_b(sample_id_transects, pathogen_ids)

meta_orig <- read.csv(paste(work_dir, "opc_metadata_orig.csv", sep="/"))
meta_orig$transect <- paste(meta_orig$Date, meta_orig$waypoint, sep="_")

miss_1 <- in_a_not_b(pathogen_ids, meta_orig$transect)

pathogen_df$missing_from_meta_orig_waypoint <- 0
for(r in 1:NROW(pathogen_df)){
  if(pathogen_df[r, 'transect'] %in% miss_1){
    pathogen_df[r, 'missing_from_meta_orig_waypoint'] <- 1
  }
}
pathogen_df <- pathogen_df[, c(4:6, 44:45)]
pathogen_df <- pathogen_df[!duplicated(pathogen_df$transect), ]

missing_codes <- in_a_not_b(unique(pathogen_df$Code), meta_orig$waypoint)
write.csv(missing_codes, paste(work_dir, "missing_waypoints.csv", sep="/"),
          row.names=FALSE)

# find missing records not due to missing codes
missing_transects <- data.frame('missing_from_metadata'=miss_1)
missing_transects$missing_due_to_code <- 0
missing_transects$missing_due_to_date <- 0
for(r in 1:NROW(missing_transects)){
  code <- strsplit(as.character(missing_transects[r, 'missing_from_metadata']),
                   split="_")[[1]][2]
  if(code %in% missing_codes){
    missing_transects[r, 'missing_due_to_code'] <- 1
  }
  else{
    missing_transects[r, 'missing_due_to_date'] <- 1
  }
}
pathogen_df <- merge(pathogen_df, missing_transects, by.x='transect',
                     by.y='missing_from_metadata', all=TRUE)
pathogen_df[is.na(pathogen_df$missing_due_to_code), 'missing_due_to_code'] <- 0
pathogen_df[is.na(pathogen_df$missing_due_to_date), 'missing_due_to_date'] <- 0
write.csv(pathogen_df, paste(work_dir, "pathogen_with_missing_records.csv", sep="/"),
          row.names=FALSE)
