# process biomass NetCDF data from ORCHIDEE
library(ncdf4)
orchidee_path <- "F:/NCI_NDR/Data meat production/ORCHIDEE/ORCHIDEE-GMv3.1_ANPP_halfdeg_GI_0000_N000.nc"
ncin <- nc_open(orchidee_path)
print(ncin)

LON <- ncvar_get(ncin, "LON")

lon <- ncvar_get(ncin, "LON")[, 1]  # don't know if this is correct ....
nlon <- length(lon)
head(lon)

lat <- ncvar_get(ncin, "LAT")[1, ]
nlat <- length(lat)
head(lat)

na_val <- ncatt_get(ncin, 'ANPP_NATURAL', "missing_value")$value
npp_array <- ncvar_get(ncin, 'ANPP_NATURAL')
npp_brick <- brick(npp_array, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), transpose=TRUE)
# npp_brick <- brick(npp_array, xmn=-180, xmx=180, ymn=-90, ymx=90, transpose=TRUE)
npp_brick[npp_brick == na_val] <- NA

mean_anpp <- calc(npp_brick, fun=mean, na.rm=TRUE)  # dimensions: 720 x 360 x 1
tif_filename <- "F:/NCI_NDR/Data meat production/ORCHIDEE/ORCHIDEE-GMv3.1_ANPP_halfdeg_GI_0000_N000_annual_average.tif"
writeRaster(mean_anpp, tif_filename, format="GTiff", overwrite=TRUE)

# plot npp from Century vs orchidee
compare_df <- read.csv("F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Mongolia_ANPP_ORCHIDEE_Century.csv")
library(ggplot2)
p <- ggplot(compare_df, aes(x=orchidee_anpp, y=century_anpp))
p <- p + geom_abline(intercept=0, slope=1, linetype='dashed') + ylim(c(0, 200))
p <- p + geom_point() + xlab("ORCHIDEE ANPP (g/m2)") + ylab("Century ANPP (g/m2)")
print(p)
pngname <- "F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Mongolia_ANPP_ORCHIDEE_Century.png"
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

compare_no_outlier <- compare_df[compare_df$orchidee_anpp < 200, ]
cor.test(compare_no_outlier$century_anpp, compare_no_outlier$orchidee_anpp, method='pearson')
lmod <- lm(orchidee_anpp~century_anpp, data=compare_no_outlier)

# compare to peak biomass at WCS's grazing exclosures
site_to_id <- read.csv("F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/exclosure_site_to_1degree_site.csv")
colnames(site_to_id) <- c('site_id', 'grid_id')
emp_raw_dat <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/exclosure_data_2017_2018_2019.csv")
emp_raw_dat$cover_herb <- emp_raw_dat$Cover.of.all.vegetation - emp_raw_dat$Cover.of.all.shrubs
emp_cols <- c('year', 'site_id', 'cover_herb', 'Rangeland.metric.score')
emp_raw_dat <- emp_raw_dat[emp_raw_dat$treatment == 'inside', emp_cols]
cover_agg <- aggregate(emp_raw_dat$cover_herb~emp_raw_dat$year + emp_raw_dat$site_id, FUN=mean)
colnames(cover_agg) <- c('year', 'site_id', 'empirical_herbaceous_cover')
rmetric_agg <- aggregate(emp_raw_dat$Rangeland.metric.score~emp_raw_dat$year + emp_raw_dat$site_id, FUN=mean)
colnames(rmetric_agg) <- c('year', 'site_id', 'empirical_rangeland_metric')
exclosure_df <- merge(cover_agg, rmetric_agg)
exclosure_df <- merge(site_to_id, exclosure_df, by='site_id')
exclosure_anpp_df <- merge(exclosure_df, compare_df, by.x='grid_id', by.y='id')  # mostly nodata

# compare to biomass at Julian's site centroids
anpp_df <- read.csv("F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Ahlborn_sites_ANPP_ORCHIDEE_Century.csv")
colnames(anpp_df)[2] <- 'site'

data_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/Julian_Ahlborn"
processed_dir <- paste(data_dir, 'summaries_GK')
veg_df <- read.csv(paste(processed_dir, 'biomass_cover_plot_mean.csv', sep='/'))
veg_2014 <- veg_df[veg_df$year == 2014, ]
site_mean_biomass <- aggregate(biomass_gm2~site, data=veg_2014, FUN=mean)
site_mean_biomass[, 'fraction'] <- 'Mean empirical biomass'
site_max_biomass <- aggregate(biomass_gm2~site, data=veg_2014, FUN=max)
site_max_biomass[, 'fraction'] <- 'Max empirical biomass'
emp_biomass <- rbind(site_mean_biomass, site_max_biomass)

plot_df <- merge(emp_biomass, anpp_df)
p <- ggplot(plot_df, aes(x=orchidee_anpp, y=biomass_gm2))
p <- p + geom_point() + facet_wrap(~fraction, scales='free')
p <- p + xlab("ORCHIDEE ANPP (g/m2)") + ylab("Biomass (g/m2)")
print(p)
pngname <- "F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Julian_biomass_ORCHIDEE_ANPP.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

max_mod <- lm(biomass_gm2~orchidee_anpp, data=plot_df, subset=(plot_df$fraction == 'Max empirical biomass'))
summary(max_mod)
mean_mod <- lm(biomass_gm2~orchidee_anpp, data=plot_df, subset=(plot_df$fraction == 'Mean empirical biomass'))
summary(mean_mod)

p <- ggplot(plot_df, aes(x=century_anpp, y=biomass_gm2))
p <- p + geom_point() + facet_wrap(~fraction, scales='free')
p <- p + xlab("Century ANPP (g/m2)") + ylab("Biomass (g/m2)")
print(p)
pngname <- "F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Julian_biomass_Century_ANPP.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

max_mod <- lm(biomass_gm2~century_anpp, data=plot_df, subset=(plot_df$fraction == 'Max empirical biomass'))
summary(max_mod)
mean_mod <- lm(biomass_gm2~century_anpp, data=plot_df, subset=(plot_df$fraction == 'Mean empirical biomass'))
summary(mean_mod)

p <- ggplot(plot_df, aes(x=orchidee_anpp, y=century_anpp))
p <- p + geom_point() + xlab("ORCHIDEE ANPP (g/m2)") + ylab("Century ANPP (g/m2)")
print(p)
pngname <- "F:/NCI_NDR/Data meat production/ORCHIDEE/comparisons_with_Century_RPM/Ahlborn_sites_ANPP_ORCHIDEE_Century.png"
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()
