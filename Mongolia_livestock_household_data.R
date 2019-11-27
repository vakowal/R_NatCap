# Making sense of household livestock data for Mongolia from Oggie

livestock_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/MALCHIN_URKH.csv"
livestock_df <- read.csv(livestock_csv)

# translation of livestock types surmised via google searches
livestock_df$camels <- livestock_df$temee
livestock_df$horses <- livestock_df$aduu
livestock_df$cattle <- livestock_df$uher
livestock_df$sheep <- livestock_df$honi
livestock_df$goats <- livestock_df$yamaa

# sheep units conversion rates from Gao et al 2015,
# "Is Overgrazing A Pervasive Problem Across Mongolia? An Examination of Livestock Forage Demand and Forage Availability from 2000 to 2014"
camelConv <- 5
horseConv <- 7
cattleConv <- 6
goatConv <- 0.9

livestock_df <- livestock_df[, c('AimagCode', 'SoumCode', 'BagCode',
                                 'camels', 'horses', 'cattle', 'sheep', 'goats')]
livestock_df$sfu_sum <- livestock_df$camels * camelConv + livestock_df$horses * horseConv +
  livestock_df$cattle * cattleConv + livestock_df$sheep + livestock_df$goats * goatConv
soum_sum <- aggregate(livestock_df$sfu_sum ~ livestock_df$AimagCode + livestock_df$SoumCode,
                      FUN=sum)
colnames(soum_sum) <- c('AimagCode', 'SoumCode', 'SFU_sum')
write.csv(soum_sum, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/processed_GK/sfu_sum_per_soum.csv",
          row.names=FALSE)

bagh_sum <- aggregate(livestock_df$sfu_sum ~ livestock_df$AimagCode + livestock_df$SoumCode + 
                        livestock_df$BagCode, FUN=sum)
colnames(bagh_sum) <- c('AimagCode', 'SoumCode', 'BagCode', 'SFU_sum')
write.csv(bagh_sum, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/processed_GK/sfu_sum_per_bagh.csv",
          row.names=FALSE)

# match with shapefile ids
soum_sum <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/processed_GK/sfu_sum_per_soum.csv")
shp_id_match <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/processed_GK/soum_aimag_match_table.csv")
soum_sum$aimag_soum <- paste(soum_sum$AimagCode, "_", soum_sum$SoumCode, sep="")
soum_sum_match_fid <- merge(soum_sum, shp_id_match)
write.csv(soum_sum_match_fid, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/from_Oggie/processed_GK/sfu_sum_per_soum_fid_match.csv",
          row.names=FALSE)
