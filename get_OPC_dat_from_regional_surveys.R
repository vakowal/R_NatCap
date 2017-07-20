# get OPC data collected during regional survey to combine with OPC survey data

data_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17"
metadata_2014 <- read.csv(paste(data_dir, 'rs_metadata_2014.csv', sep='/'))
metadata_2014 <- metadata_2014[metadata_2014$Property == "Ol_Pejeta",
                               c("Date.sampled", 'Transect', 'Lat', "Long")]
metadata_2015 <- read.csv(paste(data_dir, 'rs_metadata_2015.csv', sep='/'))
metadata_2015 <- metadata_2015[metadata_2015$Property == "Ol_Pejeta",
                               c("Date.sampled", 'Transect', 'Lat', "Long")]
metadata <- rbind(metadata_2014, metadata_2015)
write.csv(metadata, paste(data_dir, 'summarized/metadata.csv', sep='/'), row.names=FALSE)

# summarize pdm
pdm_2014 <- read.csv(paste(data_dir, 'rs_pdm_2014.csv', sep='/'))
pdm_2014 <- pdm_2014[pdm_2014$Property == 'Ol_Pejeta',
                     c("Date", "Transect", "Position_m", "PDM")]
pdm_2015 <- read.csv(paste(data_dir, 'rs_pdm_2015.csv', sep='/'))
pdm_2015 <- pdm_2015[pdm_2015$Property == 'Ol_Pejeta',
                     c("Date", "Transect", "Position_m", "PDM")]
pdm_df <- rbind(pdm_2014, pdm_2015)
pdm_df <- aggregate(PDM~Date + Transect, data=pdm_df, FUN=mean)
pdm_df$biomass_kgha <- pdm_df$PDM * 332.35 + 15.857
pdm_df <- pdm_df[, c('Date', 'Transect', 'biomass_kgha')]
write.csv(pdm_df, paste(data_dir, 'summarized/pdm_biomass.csv', sep='/'), row.names=FALSE)

# summarize pinframe
find_green <- function(val){
  letters <- strsplit(val, split="")
  if(length(letters[[1]]) > 3){
    test_letter <- letters[[1]][4]
  }
  else{
    test_letter <- letters[[1]][3]
  }
  if(test_letter == 'G'){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
pinframe_2014 <- read.csv(paste(data_dir, 'rs_pinframe_2014.csv', sep="/"))
pinframe_2014 <- pinframe_2014[pinframe_2014$Property == 'Ol_Pejeta', ]
pinframe_2014 <- pinframe_2014[, c(1, 3:4, 6:29)]
pinframe_2014$trans_pos <- paste(pinframe_2014$Date, pinframe_2014$Transect,
                                 pinframe_2014$Position_m, sep="_")
pin_pos_sum <- aggregate(pinframe_2014[, 4:27], by=list(pinframe_2014$trans_pos),
                         FUN=sum)
pin_pos_sum <- data.frame(pin_pos_sum,
                          do.call(rbind, strsplit(
                          as.character(pin_pos_sum$Group.1),'_')))
pin_pos_sum$transect <- paste(pin_pos_sum$X1, pin_pos_sum$X2, sep="_")
pin_mean <- aggregate(pin_pos_sum[, 2:25], by=list(pin_pos_sum$transect), FUN=mean)
colnames(pin_mean)[1] <- 'transect'
green_col_idx <- sapply(colnames(pin_mean), find_green, USE.NAMES=FALSE)
brown_cols <- pin_mean[, (!green_col_idx)]
green_brown_summary <- data.frame(pin_mean[, 1])
colnames(green_brown_summary) <- 'transect'
green_brown_summary$green_sum <- rowSums(pin_mean[, (green_col_idx)])
green_brown_summary$brown_sum <- rowSums(brown_cols[, -1], na.rm=TRUE)
green_brown_summary$perc_green <- green_brown_summary$green_sum / (green_brown_summary$green_sum + green_brown_summary$brown_sum)
g_b_summary_2014 <- green_brown_summary

pinframe_2015 <- read.csv(paste(data_dir, 'rs_pinframe_2015.csv', sep='/'))
pinframe_2015 <- pinframe_2015[pinframe_2015$Property == 'Ol_Pejeta', ]
pinframe_2015 <- pinframe_2015[, c(1, 3:4, 6:81)]
pinframe_2015$trans_pos <- paste(pinframe_2015$Date, pinframe_2015$Transect,
                                 pinframe_2015$Position_m, sep="_")
pin_pos_sum <- aggregate(pinframe_2015[, 4:79], by=list(pinframe_2015$trans_pos),
                         FUN=sum)
pin_pos_sum <- data.frame(pin_pos_sum,
                          do.call(rbind, strsplit(
                          as.character(pin_pos_sum$Group.1),'_')))
pin_pos_sum$transect <- paste(pin_pos_sum$X1, pin_pos_sum$X2, sep="_")
pin_mean <- aggregate(pin_pos_sum[, 2:77], by=list(pin_pos_sum$transect), FUN=mean)
colnames(pin_mean)[1] <- 'transect'
green_col_idx <- sapply(colnames(pin_mean), find_green, USE.NAMES=FALSE)
brown_cols <- pin_mean[, (!green_col_idx)]
green_brown_summary <- data.frame(pin_mean[, 1])
colnames(green_brown_summary) <- 'transect'
green_brown_summary$green_sum <- rowSums(pin_mean[, (green_col_idx)])
green_brown_summary$brown_sum <- rowSums(brown_cols[, -1], na.rm=TRUE)
green_brown_summary$perc_green <- green_brown_summary$green_sum / (green_brown_summary$green_sum + green_brown_summary$brown_sum)
g_b_summary_2015 <- green_brown_summary

green_brown_summary <- rbind(g_b_summary_2014, g_b_summary_2015)
write.csv(green_brown_summary, paste(data_dir, 'summarized/pinframe_summary.csv', sep='/'), row.names=FALSE)

# summarize dung
dung_df <- read.csv(paste(data_dir, 'rs_dung_2015.csv', sep="/"))
dung_df <- dung_df[dung_df$Property == 'Ol_Pejeta', ]
dung_df <- dung_df[which(dung_df$Position_m > 0), ]
dung_df[, 6:29][is.na(dung_df[, 6:29])] <- 0
dung_df$transect <- paste(dung_df$Date, dung_df$Transect, sep="_")
dung_sum <- aggregate(dung_df[, 6:29], by=list(dung_df$transect), FUN=sum)
colnames(dung_sum)[1] <- 'transect'

# make sure all abbrevs in dung_sum are in group key
atest <- setdiff(colnames(dung_sum)[2:25], gr_key_df$Abbrev) # cool

group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)
means_t <- as.data.frame(t(dung_sum[, c(2:25)]))
colnames(means_t) <- dung_sum$transect
means_t$Abbrev <- rownames(means_t)
gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group5', 'Group6', 'Group7')]
comb <- merge(means_t, gr_subs, by='Abbrev')
gr1_means <- aggregate(comb[, 2:7], by=list(comb$Group1), FUN=sum)
gr5_means <- aggregate(comb[, 2:7], by=list(comb$Group5), FUN=sum)
gr6_means <- aggregate(comb[, 2:7], by=list(comb$Group6), FUN=sum)
gr7_means <- aggregate(comb[, 2:7], by=list(comb$Group7), FUN=sum)
colnames(gr1_means)[1] <- "group"
colnames(gr5_means)[1] <- "group"
colnames(gr6_means)[1] <- "group"
colnames(gr7_means)[1] <- "group"
gr1_res <- as.data.frame(t(gr1_means[2:7]))
colnames(gr1_res) <- gr1_means$group
gr1_res$transect <- rownames(gr1_res)
gr5_res <- as.data.frame(t(gr5_means[2:7]))
colnames(gr5_res) <- gr5_means$group
gr5_res$transect <- rownames(gr5_res)
gr6_res <- as.data.frame(t(gr6_means[2:7]))
colnames(gr6_res) <- gr6_means$group
gr6_res$transect <- rownames(gr6_res)
gr7_res <- as.data.frame(t(gr7_means[2:7]))
colnames(gr7_res) <- gr7_means$group
gr7_res$transect <- rownames(gr7_res)
grouped_dung <- merge(gr1_res, gr6_res, all=TRUE)
gr5_res <- gr5_res[, c('transect', setdiff(colnames(gr5_res), colnames(gr1_res)))]
grouped_dung <- merge(grouped_dung, gr5_res, all=TRUE)
gr7_res <- gr7_res[, c('transect', setdiff(colnames(gr7_res), colnames(grouped_dung)))]
grouped_dung <- merge(grouped_dung, gr7_res, all=TRUE)
write.csv(grouped_dung, paste(data_dir, 'summarized/grouped_dung.csv', sep='/'), row.names=FALSE)

# added management zone field to metadata in Arc
metadata <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/downloaded_from_box_7.18.17/summarized/metadata_mgmt_zone.csv")
