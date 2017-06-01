## explore Boogie's Access database
# helpful resource: https://www.r-bloggers.com/getting-access-data-into-r/

library(RODBC)

db <- "C:/Users/Ginger/Documents/NatCap/GIS_local/Mongolia/From_Boogie/cashmere_Rangeland_monitoring.accdb"
con2 <- odbcConnectAccess2007(db)

table_names <- sqlTables(con2, tableType="TABLE")$TABLE_NAME
meta_df <- sqlFetch(con2, "Metadata")
str(meta_df)  # summarize structure of the table
livestock_df <- sqlFetch(con2, "bagh_livestock")
str(livestock_df)

columns_by_table <- list()
for(table_n in sqlTables(con2, tableType="TABLE")$TABLE_NAME){
  cols <- sqlColumns(con2, table_n)$COLUMN_NAME
  columns_by_table[[table_n]] <- cols
}
columns_by_table

table_list <- list()
for(table_n in table_names){
  table_list[[table_n]] <- sqlFetch(con2, table_n)
}

biomass_df <- table_list[['Biomass']]
