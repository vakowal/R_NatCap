# summarize NutNet data for model validation

dat_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/NutNet_data_subset_Feb_2018.csv"
dat <- read.csv(dat_csv, stringsAsFactors=FALSE)
date_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/dates_corrected.csv"
dates <- read.csv(date_csv, stringsAsFactors=FALSE)
dat <- merge(dat, dates)
site_cols <- c("site_code", "site_name", "country",
               "continent", "habitat", "latitude",
               "longitude", 'managed', 'grazed',
               'anthropogenic')
site_chars <- unique(dat[, site_cols])

# valid candidates for validation
# assert no ambiguity in "managed" field within site
biomass <- dat[!is.na(dat$mass), c('year', 'site_name',
                                   'site_code', 'block', 'plot', 'subplot',
                                   'mass', 'live', 'category',
                                   'date_certain', 'date_formatted')]
biomass <- biomass[!is.na(biomass$date_formatted), ]
biomass_sites <- as.data.frame(unique(biomass[, 'site_code']))
colnames(biomass_sites) <- 'site_code'
biomass_sites$valid_biomass <- 1
validation_sites <- merge(site_chars, biomass_sites, by='site_code', all.x=TRUE)
validation_sites <- validation_sites[!is.na(validation_sites$valid_biomass), ]
validation_sites <- validation_sites[(validation_sites$managed==0 |
                                        is.na(validation_sites$managed)), ]
validation_sites <- validation_sites[validation_sites$managed!=1, ]
validation_sites <- validation_sites[, c("site_code", "latitude", "longitude")]
write.csv(validation_sites, "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/GK_summaries/validation_sites.csv",
          row.names=FALSE)

# data summaries
colnames(dat[which(unlist(lapply(colnames(dat), FUN=endsWith, suffix=".x")))])
colnames(dat[which(unlist(lapply(colnames(dat), FUN=endsWith, suffix=".y")))])

biomass <- dat[!is.na(dat$mass), c('year', 'site_name',
                                   'site_code', 'block', 'plot', 'subplot',
                                   'mass', 'live', 'category',
                                   'date_certain', 'date_formatted')]
biomass_no_date <- biomass[is.na(biomass$date_formatted), ]
biomass <- biomass[!is.na(biomass$date_formatted), ]
# number of years of biomass measurement per site
n_years <- aggregate(year~site_code, data=unique(biomass_no_date[, c('year', 'site_code')]),
                     FUN=length)
colnames(n_years) <- c('site_code', 'n_years_biomass_collection')
n_years_w_date <- aggregate(year~site_code, data=unique(biomass[, c('year', 'site_code')]),
                            FUN=length)
colnames(n_years_w_date) <- c('site_code', 'n_years_biomass_collection_with_date')
n_years <- merge(n_years, n_years_w_date)
site_summary <- merge(n_years, site_chars, by='site_code')
write.csv(site_summary, "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/GK_summaries/site_summary.csv",
          row.names=FALSE)

# does live mean live?
live_categories <- data.frame('site_code'=unique(biomass$site_code),
                              'live_categories'=NA, 'dead_categories'=NA)
for(site in unique(biomass$site_code)){
  subs <- biomass[biomass$site_code == site, ]
  unique_live <- unique(subs[subs$live == 1, 'category'])
  unique_dead <- unique(subs[subs$live == 0, 'category'])
  live_categories[live_categories$site_code == site, 'live_categories'] <- paste(unique_live, collapse=', ')
  live_categories[live_categories$site_code == site, 'dead_categories'] <- paste(unique_dead, collapse=', ')
}

# means across replicates within site, treatment, date, functional group
# exclude managed sites
unique(biomass$category)


