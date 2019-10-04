# data prep for NCI Nitrate ~ NDR for World Bank

library(ggplot2)

# NOXN training data from World bank
training_data_csv <- "F:/NCI_NDR/Data worldbank/training_data/NOXN/noxn_04_03_19.csv"
training_data <- read.csv(training_data_csv)

# collect min and max values from training data by column
min_by_column <- as.data.frame(apply(training_data, 2, min))
min_by_column$variable <- row.names(min_by_column)
colnames(min_by_column) <- c('min_value', 'variable')
max_by_column <- as.data.frame(apply(training_data, 2, max))
max_by_column$variable <- row.names(max_by_column)
colnames(max_by_column) <- c('max_value', 'variable')
col_summary <- merge(min_by_column, max_by_column)
write.csv(col_summary, "F:/NCI_NDR/Data worldbank/training_data/NOXN/min_max_values.csv")

# station data from World Bank
parameter_metadata_csv <- "F:/NCI_NDR/Data worldbank/station_data/parameter_metadata.csv"
parameter_metadata_df <- read.csv(parameter_metadata_csv)
N_files_dir <- "F:/NCI_NDR/Data worldbank/station_data/Nitrogen_sheets_exported"
N_files_list <- list.files(N_files_dir)
N_df_list <- list()
for (file in N_files_list) {
  df <- read.csv(paste(N_files_dir, file, sep='/'))
  N_df_list[[file]] <- df
}
N_measurement_df <- do.call(rbind, N_df_list)
N_freq_table <- as.data.frame(table(N_measurement_df$Parameter.Code))
N_freq_table <- merge(N_freq_table, parameter_metadata_df,
                           by.x="Var1", by.y="Parameter.Code",
                           all.x=TRUE)
write.csv(N_freq_table, "F:/NCI_NDR/Data worldbank/station_data/N_parameter_frequency_count.csv")

# station metadata
station_metadata_csv <- "F:/NCI_NDR/Data worldbank/station_data/station_metadata.csv"
stn_df <- read.csv(station_metadata_csv)

# how many stations of each type (groundwater vs surface water) have noxn measurements?
N_meas_cols <- colnames(N_measurement_df)[c(1:2, 8:10)]
noxn_subset <- N_measurement_df[N_measurement_df$Parameter.Code == 'NOxN', N_meas_cols]
stn_cols <- colnames(stn_df)[c(1, 4, 5, 15:16)]
stn_subset <- stn_df[, stn_cols]
stn_subset[stn_subset$Water.Type == 'Groundwater station', 'ground_v_surface'] <- 'groundwater'
stn_subset[stn_subset$Water.Type == 'Lake station', 'ground_v_surface'] <- 'surface'
stn_subset[stn_subset$Water.Type == 'River station', 'ground_v_surface'] <- 'surface'
NOxN_by_stn <- merge(noxn_subset, stn_subset, all.x=TRUE)
NOxN_by_stn$Sample.Date <- as.Date(NOxN_by_stn$Sample.Date, format="%Y-%m-%d")

# groundwater stations with NOxN measurements
NOxN_groundwater <- NOxN_by_stn[NOxN_by_stn$ground_v_surface == 'groundwater', ]
groundwater_freq_table <- as.data.frame(table(NOxN_groundwater$GEMS.Station.Number))
groundwater_freq_table <- groundwater_freq_table[groundwater_freq_table$Freq > 0, ]
colnames(groundwater_freq_table) <- c('GEMS.Station.Number', 'Num_obs_NOxN')
stn_coords <- stn_subset[stn_subset$GEMS.Station.Number %in% groundwater_freq_table$GEMS.Station.Number,
                         c("GEMS.Station.Number", "Country.Name", "Latitude", "Longitude")]
groundwater_freq_table <- merge(groundwater_freq_table, stn_coords)
write.csv(groundwater_freq_table, "F:/NCI_NDR/Data worldbank/station_data/groundwater_noxn_obs.csv",
          row.names=FALSE)
