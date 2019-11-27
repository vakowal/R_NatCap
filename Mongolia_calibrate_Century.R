# Examine Century results in comparison with empirical biomass, to calibrate plant parameterization
library(ggplot2)

sim_output_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec_no_grazing/biomass_summary.csv"
match_csv <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec_no_grazing/match_summary.csv"

sim_output_df <- read.csv(sim_output_csv)
match_df <- read.csv(match_csv)

site_list <- unique(match_df$site)

# time series of total biomass
for (i in 1:10) {
  rand_site <- site_list[sample(1:length(site_list), 1)]
  time_subset <- sim_output_df[sim_output_df$site == rand_site, ]
  time_subset$month <- c(1:12, 12)
  p <- ggplot(time_subset, aes(x=month, y=live))
  p <- p + geom_line()
  p <- p + ggtitle(rand_site)
  p <- p + scale_x_continuous(breaks=seq(2, 12, by=2))
  print(p)
}

# examine trends in simulated vs empirical biomass at each site
match_df$diff <- match_df$Simulated_biomass - match_df$Empirical_biomass
hist(match_df$diff, breaks=100)
summary(match_df$diff)
length(match_df[match_df$diff > 0, 'diff'])  # ideally, this value will be >0 for almost all sites

# examine peak biomass across sites
peak_live_agg <- aggregate(live ~ site, sim_output_df, max)
peak_live_df <- merge(peak_live_agg, sim_output_df)
hist(peak_live_df$live, breaks=100)
summary(peak_live_df$live)  # this should be around 100 g/m2

peak_subs <- peak_live_df[, c('site', 'live')]
colnames(peak_subs)[2] <- 'peak_live'
match_subs <- match_df[, c('site', 'Empirical_biomass', 'Simulated_biomass', 'diff')]
sum_df <- merge(peak_subs, match_subs)
write.csv(sum_df, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/monitoring_sites/chirps_prec_no_grazing/site_summary.csv",
          row.names=FALSE)
