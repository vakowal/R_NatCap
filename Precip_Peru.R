# process weather data for CGIAR Peru

weather_file <- "C:/Users/Ginger/Documents/NatCap/GIS_local/CGIAR/Peru/climate_and_soil/SWAT_precip.csv"
dat <- read.csv(weather_file)

outdir <- "C:/Users/Ginger/Desktop/precip_working"

sites <- levels(dat$station)
for(site in sites){
  missing_val_y <- c()
  missing_val_m <- c()
  subs <- subset(dat, station == site)
  copy <- subset(dat, station == site)
  min_y <- min(subs$year)
  max_y <- max(subs$year)
  y <- min_y
  while(y <= max_y){
    for(m in 1:12){
      val <- subs[which(subs$year == y & subs$month == m), 'PCP']
      if(length(val) == 0){
        missing_val_y <- append(missing_val_y, y)
        missing_val_m <- append(missing_val_m, m)
        non_missing <- subs[which(subs$month == m), 'PCP']
        new_row <- c(mean(non_missing), y, m, site)
        copy <- rbind(copy, new_row)
      }
    }
    y <- y + 1
  }
  # record what dates were missing for this site
  missing_df <- data.frame(year=missing_val_y, month=missing_val_m)
  missing_csv <- paste(outdir, '/', site, '_missing_values.csv', sep="")
  write.table(missing_df, missing_csv, row.names=FALSE, sep=",")
  
  # format site for input to CENTURY
  nrow <- max_y - min_y + 1
  formatted_df <- data.frame(year=numeric(nrow),
                             "1"=numeric(nrow), "2"=numeric(nrow), "3"=numeric(nrow),
                             "4"=numeric(nrow), "5"=numeric(nrow), "6"=numeric(nrow),
                             '7'=numeric(nrow), '8'=numeric(nrow), '9'=numeric(nrow), 
                             '10'=numeric(nrow), '11'=numeric(nrow), '12'=numeric(nrow))
  i <- 1
  for(y in min_y:max_y){
    row <- c(y)
    for(m in 1:12){
      row <- append(row, copy[copy$year==y & copy$month==m, 'PCP'])
    }
    formatted_df[i, ] <- row
    i <- i + 1
  }
  df_csv <- paste(outdir, '/', site, '_formatted.csv', sep="")
  write.table(formatted_df, df_csv, row.names=FALSE, sep=",")
}