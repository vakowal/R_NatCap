## Calculate summary climate data from IDEAM

ProcessDataToDF <- function(data_dir, ideam_cols, summary, indices, widths, skip_cols = FALSE) {
  list_of_data <- list()
  for (index in indices) {
    file <- read.fwf(paste(data_dir, "/", index, ".txt", sep = ""), widths = widths, skip = 5, fill = TRUE)
    if (skip_cols) {
      file <- file[, c(1, seq(2, 24, by = 2))]
    }
    colnames(file) <- ideam_cols
    file$year <- summary[index, "year"]
    file$coords <- summary[index, "coords"]
    list_of_data[[index]] <- file
  }
  df <- do.call(rbind, list_of_data)
  return(df)
}

data_dir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Colombia/IDEAM_parsed'
variables <- read.table(paste(data_dir, 'variables.txt', sep = "/"), header = FALSE)
coordinates <- read.table(paste(data_dir, 'coordinates.txt', sep = "/"), header = TRUE)
years <- read.table(paste(data_dir, 'years.txt', sep = "/"), header = FALSE)
colnames(years) <- c('index', 'year')

years$variable = do.call(paste, as.data.frame(variables[, 2:7], stringsAsFactors=FALSE))
years$coords = do.call(paste, c(as.data.frame(coordinates[, c("lat", "long")]), sep = ", "))

summary <- years
write.csv(summary, paste(data_dir, "summary.txt", sep = '/'), row.names = FALSE)

summary = read.csv(paste(data_dir, '/summary.txt', sep = ""), header = TRUE)
variables <- unique(summary[, 'variable'])

# variables[1] "VALORES MEDIOS DIARIOS DE TEMPERATURA (oC)"     median temperature (deg C)
# variables[2] "VALORES TOTALES DIARIOS DE PRECIPITACION (mms)" median precipitation (mm)
# variables[3] "VALORES TOTALES DIARIOS DE EVAPORACION (mms)"   median evaporation (mm)
# variables[4] "VALORES MEDIOS DIARIOS DE CAUDALES (M3/Seg)"    median flow (m3/sec)

ideam_cols <- c('day', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sept', 'oct', 'nov', 'dec')

temp_indices <- summary[which(summary$variable == variables[1]), 'index']
precip_indices <- summary[which(summary$variable == variables[2]), 'index']
temp_widths <- c(21, rep(9, 12))
precip_widths <- c(18, rep(c(7,2), 12))

temp_df <- ProcessDataToDF(data_dir, ideam_cols, summary, temp_indices, temp_widths)
precip_df <- ProcessDataToDF(data_dir, ideam_cols, summary, precip_indices, precip_widths, skip_cols = TRUE)

  

