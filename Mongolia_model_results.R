# examine Mongolia simulations

library(ggplot2)

match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/soum_ctr_match_table.csv")

# make map of CHIRPS pixel-based model results
ch_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/biomass_summary_zero_sd_chirps_GCD_G.csv")
aug_df <- ch_df[ch_df$month == 9, colnames(ch_df)[c(2:5, 9)]]
aug_wide <- reshape(aug_df, idvar='site_id', timevar='year', direction="wide")
write.csv(aug_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/CHIRPS_pixels/biomass_August_2016_2017_2_soums.csv",
          row.names=FALSE)

# compare results from NAMEM and CHIRPS
comp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
comp_df <- comp_df[comp_df$month == 9, ]  # August only
match_df <- match_df[, c('name_en', 'site_id')]
comp_df <- merge(comp_df, match_df)
p <- ggplot(comp_df, aes(x=name_en, y=total_biomass_gm2))
p <- p + geom_point(aes(shape=climate_source))
p <- p + facet_wrap(~year)
p <- p + xlab("Soum") + ylab("August biomass (g/m2)")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/figs/soum_ctr_August_biomass_2016_2017.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

# make csv to join to soum centers, to make map
comp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
comp_subs <- comp_df[comp_df$month == 9, colnames(comp_df)[c(2:5, 7, 9)]]  # August only
comp_nm <- comp_df[comp_df$month == 9 &
                       comp_df$climate_source == 'namem_clim',
                     colnames(comp_df)[c(2:5, 9)]]
comp_wide <- reshape(comp_nm, idvar='site_id',
                     timevar='year',
                     direction="wide")
write.csv(comp_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_August_2016_2017_NAMEM.csv",
          row.names=FALSE)
comp_ch <- comp_df[comp_df$month == 9 &
                     comp_df$climate_source == 'chirps_prec',
                   colnames(comp_df)[c(2:5, 9)]]
comp_wide <- reshape(comp_ch, idvar='site_id',
                     timevar='year',
                     direction="wide")
write.csv(comp_wide,
          "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/soum_centers/biomass_August_2016_2017_CHIRPS.csv",
          row.names=FALSE)

# compare NAMEM and CHIRPS climate data at soum centers
match_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/NAMEM/soum_ctr_match_table.csv")
namem_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_inputs/soum_centers/namem_clim"
chirps_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_inputs/soum_centers/chirps_prec"
figdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/climate/source_comparisons"

widths <- c(6, 4, rep(7, 12))
df_list <- list()
for(r in 1:NROW(match_df)){
  chirps_wth <- paste(chirps_dir, "/", match_df[r, 'site_id'], ".wth", sep="")
  ch_df <- read.fwf(chirps_wth, widths=widths)
  ch_df <- ch_df[which(ch_df$V2 >= 2008), ]
  ch_df$source <- 'CHIRPS'
  nm_wth <- paste(namem_dir, "/", match_df[r, 'name_en'], ".wth", sep="")
  nm_df <- read.fwf(nm_wth, widths=widths)
  nm_df$source <- 'NAMEM'
  site_df <- rbind(ch_df, nm_df)
  site_df$site_id <- match_df[r, 'name_en']
  colnames(site_df) <- c('variable', 'Year', seq(1, 12), 'source', 'site_id')
  df_list[[r]] <- site_df
}
clim_df <- do.call(rbind, df_list)
clim_res <- reshape(clim_df, idvar=c('variable', 'Year', 'source', "site_id"),
                    varying=colnames(clim_df)[3:14], v.names='amount',
                    direction="long", timevar='month')
clim_res$date <- as.Date(paste(clim_res$Year, clim_res$month, '01', sep="-"),
                         format='%Y-%m-%d')
p <- ggplot(clim_res, aes(x=date, y=amount))
p <- p + geom_line(aes(linetype=source))
p <- p + facet_grid(variable~site_id, scales="free")
print(p)
pngname <- paste(figdir, "CHIRPS_vs_NAMEM.png", sep="/")
png(file=pngname, units="in", res=300, width=15, height=10)
print(p)
dev.off()

clim_sub <- clim_res[clim_res$variable == "prec  ", ]
p <- ggplot(clim_sub, aes(x=date, y=amount))
p <- p + geom_line(aes(linetype=source))
p <- p + facet_wrap(~site_id, scales="free",
                    nrow=3)
print(p)
pngname <- paste(figdir, "CHIRPS_vs_NAMEM_prec.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=8)
print(p)
dev.off()

ch_subs <- clim_res[clim_res$source == 'CHIRPS', ]
nm_subs <- clim_res[clim_res$source == 'NAMEM', ]
sub_res <- merge(ch_subs, nm_subs, by=c('date', "Year", 'month', 'variable', 'site_id'))
sub_res$CHIRPS_minus_NAMEM <- sub_res$amount.x - sub_res$amount.y
sub_prec <- sub_res[sub_res$variable == unique(sub_res$variable)[1], ]

p <- ggplot(sub_prec, aes(x=date, y=CHIRPS_minus_NAMEM))
p <- p + geom_line()
p <- p + facet_wrap(~site_id, nrow=1)  #, scales="free")
p <- p + ylab("CHIRPS - NAMEM (cm)")
print(p)
pngname <- paste(figdir, "CHIRPS_minus_NAMEM_prec.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# empirical summary of biomass: SCP and CBM
emp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/biomass_SCP_CBM.csv")
source_id <- emp_df[, c('site_id', 'source')]
colnames(source_id) <- c('site_id', 'emp_source')

sim_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/biomass_summary_zero_sd_namem_chirps_GCD_G.csv")
# sim_df <- merge(sim_df, source_id, by='site_id')

wth_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/data/summaries_GK/CBM_SCP_points_nearest_soum_ctr.csv")
wth_df <- wth_df[, c("site_id", "NEAR_DIST", "name_en")]
sim_df <- merge(sim_df, wth_df)

# mismatch (sim - empirical) by precip source, for SCP sites only
emp_df$year <- format(as.Date(emp_df$Date), '%Y')
emp_df <- emp_df[emp_df$source == 'SCP', c('site_id', 'biomass_g_m2', 'year')]
colnames(emp_df) <- c('site_id', 'empirical_biomass', 'year')
aug_df <- sim_df[sim_df$month == 9, ]
simemp_df <- merge(emp_df, aug_df, by=c('site_id', 'year'), all=FALSE)
simemp_df$sim_minus_emp_gm2 <- simemp_df$total_biomass_gm2 - simemp_df$empirical_biomass
p <- ggplot(simemp_df, aes(x=climate_source, y=sim_minus_emp_gm2))
p <- p + geom_boxplot()
p <- p + facet_wrap(~year)
print(p)
figdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Mongolia/model_results/prec_source_comparison"
pngname <- paste(figdir, "sim-emp_chirps_namem_worldclim.png", sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(simemp_df, aes(x=empirical_biomass, y=total_biomass_gm2))
p <- p + geom_point()
p <- p + facet_wrap(~climate_source, nrow=3, scales="free")
p <- p + xlab('empirical biomass') + ylab('simulated biomass')
print(p)
pngname <- paste(figdir, "sim_vs_emp_chirps_namem_worldclim.png", sep="/")
png(file=pngname, units="in", res=300, width=3, height=8)
print(p)
dev.off()

for(precip_source in unique(simemp_df$climate_source)){
  subs <- simemp_df[simemp_df$climate_source == precip_source, ]
  c <- cor.test(subs$total_biomass_gm2, subs$empirical_biomass,
                  method='pearson')
  print(precip_source)
  print(c)
}

