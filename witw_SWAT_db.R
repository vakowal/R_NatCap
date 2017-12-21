## get info from SWAT database

library(RODBC)

cont_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/SWAT_Ucross/SWAT_databases/continuous/SWATmodelthirdIteration.mdb"
cont_db <- odbcConnectAccess2007(cont_file)

mgt2_c <- sqlFetch(cont_db, 'mgt2')
grz_c <- mgt2_c[mgt2_c$MGT_OP == 9, ]
mgt1_c <- sqlFetch(cont_db, 'mgt1')
unique(mgt1_c$BIO_MIN)

grz_c$UNIQUECOMB <- paste(grz_c$SUBBASIN, grz_c$LANDUSE, grz_c$SOIL, grz_c$SLOPE_CD, sep="_")
grz_c <- grz_c[, colnames(grz_c[c(8:10, 30, 32, 68)])]
hru_areac <- sqlQuery(cont_db, "select UNIQUECOMB, AREA from FullHRU")  # the AREA field is in ha
setdiff(unique(grz_c$UNIQUECOMB), unique(hru_areac$UNIQUECOMB)) # empty
grz_c <- merge(grz_c, hru_areac, all.x=TRUE)
grz_c$kg_consumed <- grz_c$GRZ_DAYS * grz_c$BIO_EAT * grz_c$AREA
sum_consumed_by_hru_c <- aggregate(kg_consumed~UNIQUECOMB, data=grz_c, FUN=sum)  # kg consumed in each HRU across 5 years

one_month <- grz_c[grz_c$YEAR == 1 & grz_c$MONTH == 5, ]  # 626 HRUs
total_area <- sum(one_month$AREA)  # 3767 total ha

# assume I should be looking at the 5-year rotational scenario
rot5_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/SWAT_Ucross/SWAT_databases/Rotational/5 year rotational/SWATmodelthirdIteration.mdb"
rot5_db <- odbcConnectAccess2007(rot5_file)
mgt2_r <- sqlFetch(rot5_db, 'mgt2')
grz_r <- mgt2_r[mgt2_r$MGT_OP == 9, ]
mgt1_r <- sqlFetch(rot5_db, 'mgt1')
unique(mgt1_r$BIO_MIN)

grz_r$UNIQUECOMB <- paste(grz_r$SUBBASIN, grz_r$LANDUSE, grz_r$SOIL, grz_r$SLOPE_CD, sep="_")
grz_r <- grz_r[, colnames(grz_r)[c(8:10, 30, 32, 68)]]
# find area of each hru
hru_arear <- sqlQuery(rot5_db, "select UNIQUECOMB, AREA from FullHRU")  # the AREA field is in ha
setdiff(unique(hru_areac), unique(hru_arear)) # empty
setdiff(unique(hru_arear), unique(hru_areac)) # empty
setdiff(unique(grz_r$UNIQUECOMB), unique(hru_arear$UNIQUECOMB)) # empty
grz_r <- merge(grz_r, hru_arear, all.x=TRUE)
# calculate total kg eaten
grz_r$kg_consumed <- grz_r$GRZ_DAYS * grz_r$BIO_EAT * grz_r$AREA
sum_consumed_by_hru_r <- aggregate(kg_consumed~UNIQUECOMB, data=grz_r, FUN=sum)  # kg consumed in each HRU across 5 years

setdiff(unique(sum_consumed_by_hru_c$UNIQUECOMB), unique(sum_consumed_by_hru_r$UNIQUECOMB))
setdiff(unique(sum_consumed_by_hru_r$UNIQUECOMB), unique(sum_consumed_by_hru_c$UNIQUECOMB))  # empty

consumed_total <- merge(sum_consumed_by_hru_c, sum_consumed_by_hru_r,
                        by='UNIQUECOMB', all=TRUE)
colnames(consumed_total) <- c('UNIQUECOMB', 'kg_consumed_continuous', 'kg_consumed_rotation')
consumed_total$rot_minus_cont <- consumed_total$kg_consumed_rotation - consumed_total$kg_consumed_continuous

write.csv(consumed_total, "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/SWAT_Ucross/consumed_total.csv",
          row.names=FALSE)

sum(consumed_total$rot_minus_cont, na.rm=TRUE) / (3767*5) # average yearly difference per ha
