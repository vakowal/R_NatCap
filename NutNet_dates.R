# standardize NutNet dates

rawdate_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/distinct_biomass_sampling_dates_blocksplots.csv"
rawdate <- read.csv(rawdate_csv, stringsAsFactors=FALSE)

# replace missing values with NA
rawdate$processed <- trimws(rawdate$date, which="both")
rawdate$processed[rawdate$processed == "NULL"] <- NA
rawdate$processed[nchar(rawdate$processed) == 0] <- NA

# convert all separating characters to '/'
rawdate$processed <- as.character(rawdate$processed)
rawdate$processed <- gsub("-", "/", rawdate$processed, fixed=TRUE)
rawdate$processed <- gsub(".", "/", rawdate$processed, fixed=TRUE)
rawdate$processed <- gsub("_", "/", rawdate$processed, fixed=TRUE)
rawdate$index <- seq(1, dim(rawdate)[1])
rawdate$format <- NA
done <- rawdate[is.na(rawdate$processed), ]
rem <- rawdate[!rawdate$index %in% done$index, ]

# manual fixes
rem[rem$processed == "5/5, 5/11, 5/12/17", 'processed'] <- '5/9/17'
rem[rem$processed == "21 Jun 2011 and 30 Aug 20", 'processed'] <- '2011'
rem[rem$processed == "2011/5/25 & 2011/11/12", 'processed'] <- '2011'
rem[rem$processed == "6/20 & 9/15/2016", 'processed'] <- '2016'
rem[rem$processed == "2011231", 'processed'] <- '1/23/2011'
rem[rem$processed == "2011237", 'processed'] <- '7/23/2011'
rem[rem$processed == "2011238", 'processed'] <- '8/23/2011'
rem[rem$processed == "2013/8/26", 'processed'] <- '8/26/2013'
rem[rem$processed == "june/30/2015", 'processed'] <- '6/30/2015'
done <- rbind(done, rem[sapply(rem[, 'processed'],
                               function(y) length(strsplit(y, split='/')
                                                  [[1]]) == 1), ])  # year only
rem <- rem[!rem$index %in% done$index, ]
rem$date1 <- sapply(rem[, 'processed'],
                    function(y) strsplit(y, split='/')[[1]][1])
rem$date2 <- sapply(rem[, 'processed'],
                    function(y) strsplit(y, split='/')[[1]][2])
rem$date3 <- sapply(rem[, 'processed'],
                    function(y) strsplit(y, split='/')[[1]][3])
for(r in 1:nrow(rem)){
  if(is.na(rem[r, 'date3'])){
    rem[r, 'date3'] <- rem[r, 'year']
    rem[r, 'processed'] <- paste(rem[r, 'date1'], rem[r, 'date2'], rem[r, 'date3'], sep='/')
  }
}
for(r in 1:nrow(rem)){
  if(nchar(rem[r, 'date3']) == 2){
    if(rem[r, 'date3'] > 17){
      rem[r, 'date3'] <- rem[r, 'year']
      rem[r, 'processed'] <- paste(rem[r, 'date1'], rem[r, 'date2'], rem[r, 'year'], sep='/')
    }
    else {
      rem[r, 'date3'] <- paste('20', rem[r, 'date3'], sep="")
      rem[r, 'processed'] <- paste(rem[r, 'date1'], rem[r, 'date2'], rem[r, 'date3'], sep='/')
    }
  }
}
done$date1 <- NA
done$date2 <- NA
done$date3 <- NA

rem[is.na(as.numeric(rem$date2)), 'format'] <- '%d/%b/%Y'
done <- rbind(done, rem[!is.na(rem$format), ])
rem <- rem[!rem$index %in% done$index, ]

sub1 <- rem[sapply(rem[, 'processed'],
                   function(y) as.numeric(strsplit(y, split='/')[[1]][2]) > 12), ]  # definitely m/d/yyyy
sub1$format <- '%m/%d/%Y'
sub2 <- rem[sapply(rem[, 'processed'],
                   function(y) as.numeric(strsplit(y, split='/')[[1]][1]) > 12), ]  # definitely d/m/yyyy
sub2$format <- '%d/%m/%Y'
done <- rbind(done, sub1)
done <- rbind(done, sub2)
rem <- rem[!rem$index %in% done$index, ]
done$date_certain <- 1

# assume date format is consistent within site
rem[rem$site %in% sub1$site_code, 'format'] <- sub1[1, 'format']
rem[rem$site %in% sub2$site_code, 'format'] <- sub1[1, 'format']

problems <- intersect(sub1$site_code, sub2$site_code)
rem[rem$site == problems, 'format'] <- '%m/%d/%Y'  # nearly wild guess
rem[is.na(rem$format), 'format'] <- '%m/%d/%Y'  # nearly wild guess
rem$date_certain <- 0
done <- rbind(done, rem)
date_unknown <- done[is.na(done$format), ]
rem <- done[!done$index %in% date_unknown$index, ]
rem$date_formatted <- as.Date('01/31/1900', format='%m/%d/%Y')
for(r in 1:NROW(rem)){
  rem[r, 'date_formatted'] <- as.Date(rem[r, 'processed'], format=rem[r, 'format'])
}
library(ggplot2)
p <- ggplot(rem, aes(x=date_formatted, y=date_certain))
p <- p + geom_point()
print(p)  # sweet

date_unknown$date_formatted <- as.Date(NA)
dates_corrected <- rbind(rem, date_unknown)
dates_corrected <- dates_corrected[, c('date', 'year', 'site_code',
                                       'block', 'plot', 'subplot',
                                       'date_certain', 'date_formatted')]
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/dates_corrected.csv"
write.csv(dates_corrected, save_as, row.names=FALSE)

