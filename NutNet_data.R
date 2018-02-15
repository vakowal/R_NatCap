# summarize NutNet data for model validation

########################
dat_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/NutNet_data_subset_Feb_2018.csv"
dat <- read.csv(dat_csv, stringsAsFactors=FALSE)
site_cols <- c("site_code", "site_name", "country",
               "continent", "habitat", "latitude",
               "longitude")
site_chars <- unique(dat[, site_cols])

# data diagnostics
colnames(dat[which(unlist(lapply(colnames(dat), FUN=endsWith, suffix=".x")))])
colnames(dat[which(unlist(lapply(colnames(dat), FUN=endsWith, suffix=".y")))])

biomass <- dat[!is.na(dat$mass), c('year', 'site_name',
                                   'site_code', 'block', 'plot', 'subplot',
                                   'mass', 'live', 'category')]
test1 <- biomass[biomass$subplot.x != biomass$subplot.y, ]  # problematic
test1 <- biomass[(biomass$live.x == 1 & biomass$live.y == 0), ]
test1 <- biomass[(biomass$live.y == 1 & biomass$live.x == 0), ] # pay attention to live.x

biomass <- dat[!is.na(dat$mass), c('year', 'site_code', 'block', 'plot',
                                   'subplot', 'mass', 'live','category')]
n_years <- aggregate(year~site_code, data=unique(biomass[, c('year', 'site_code')]),
                     FUN=length)
colnames(n_years) <- c('site_code', 'n_years_biomass_collection')
site_summary <- merge(n_years, site_chars, by='site_code')
write.csv(site_summary, "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/GK_summaries/site_summary.csv",
          row.names=FALSE)
