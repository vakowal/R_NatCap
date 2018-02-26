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
biomass <- dat[!is.na(dat$mass), c('site_code', 'trt',
                                   'mass', 'NotLitter', 'category',
                                   'date_certain', 'date_formatted')]  # has biomass dataa
biomass <- biomass[!is.na(biomass$date_formatted), ]  # has valid date
biomass_sites <- as.data.frame(unique(biomass[, 'site_code']))
colnames(biomass_sites) <- 'site_code'
biomass_sites$valid_biomass <- 1
validation_sites <- merge(site_chars, biomass_sites, by='site_code', all.x=TRUE)
validation_sites <- validation_sites[!is.na(validation_sites$valid_biomass), ]
# assert no ambiguity in "managed" field within site
testc <- c()
for(site in unique(validation_sites$site_code)){
  subs <- validation_sites[validation_sites$site_code == site, ]
  vals <- unique(subs$managed)
  testc <- c(testc, length(vals))
}
validation_sites <- validation_sites[(validation_sites$managed==0 |
                                        is.na(validation_sites$managed)), ]
# validation_sites <- validation_sites[validation_sites$managed!=1, ]
validation_sites <- validation_sites[, c("site_code", "latitude", "longitude")]
write.csv(validation_sites, "C:/Users/Ginger/Dropbox/NatCap_backup/NutNet/GK_summaries/validation_sites.csv",
          row.names=FALSE)

# data summaries
biomass <- biomass[biomass$site_code %in% unique(validation_sites$site_code), ]
# aggregate to plot
biomass_plot <- aggregate(mass~site_code+trt+date_formatted+category,
                          data=biomass, FUN=mean)
biomass_res <- reshape(biomass_plot, idvar=c('site_code', 'trt', 'date_formatted'),
                       timevar='category', direction="wide")
# additional biomass categories
biomass_res$herbaceous <- rowSums(biomass_res[, c("mass.GRAMINOID", "mass.FORB")],
                                  na.rm=TRUE)
biomass_res$herbaceous_legume <- rowSums(biomass_res[, c("mass.GRAMINOID", "mass.FORB", "mass.LEGUME")],
                                         na.rm=TRUE)
# coarsen dates: samples taken within 30 days of each other
# within site/treatment combination should receive median date
# since I can't find out way to do this explicitly, just group by calendar month
# and check for adjacent months
biomass_res$samp_mon <- as.character(as.Date(biomass_res$date_formatted), format="%m-%Y")
median_date <- aggregate(date_formatted~site_code+trt+samp_mon, data=biomass_res, 
                                 FUN=median, na.rm=TRUE)
avg_date <- aggregate(date_formatted~site_code+trt+samp_mon, data=biomass_res, 
                      FUN=mean, na.rm=TRUE)

# summary statistics across plots within site/date/trt
biomass_mean <- aggregate(biomass_res[, 4:21], by=biomass_res[, c(1:3)], FUN=mean)


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

# means across replicates within site, treatment, date, functional group
# exclude managed sites
unique(biomass$category)


