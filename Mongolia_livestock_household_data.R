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

## correct % liverstock in each soum by area of desert ecosystem, which is not modeled
sfu_tes_gobi_csv <- "E:/GIS_local/Mongolia/Livestock_household_data/masked_by_tes_gobi_ecosystem/sfu_per_soum_intersect_tes_gobi.csv"
sfu_df <- read.csv(sfu_tes_gobi_csv)
soum_total_area <- aggregate(AREA_M2~aimag_soum, data=sfu_df, FUN=sum)
colnames(soum_total_area)[2] <- 'area_total'
soum_area_desert <- aggregate(AREA_M2~aimag_soum, data=sfu_df[sfu_df$tes_gobi==13, ], FUN=sum)
colnames(soum_area_desert)[2] <- 'area_tes_gobi=13'
area_df <- merge(soum_total_area, soum_area_desert, all=TRUE)
area_df[is.na(area_df$`area_tes_gobi=13`), 'area_tes_gobi=13'] <- 0
area_df$proportion_area_non_desert <- (area_df$area_total - area_df$`area_tes_gobi=13`) / area_df$area_total
write.csv(area_df, "E:/GIS_local/Mongolia/Livestock_household_data/masked_by_tes_gobi_ecosystem/percent_area_non_desert.csv",
          row.names=FALSE)
