# Describe future climate scenarios for Mongolia
library(ggplot2)

relative_to_baseline <- function(df) {
  df$percent_change <- 0
  df$absolute_change <- 0
  for (out in unique(df$output)) {
    for (aoi in unique(df$aoi)) {
      for (method in unique(df$summary_method)) {
        subs <- ((df$output == out) & (df$aoi == aoi) & (df$summary_method == method))
        baseline_val <- df[(df$output == out) & (df$aoi == aoi) & (df$summary_method == method) &
                             (df$model == 'current'), 'mean_value']
        df[subs, 'percent_change'] <- (df[subs, 'mean_value'] - baseline_val) / abs(baseline_val) * 100
        df[subs, 'absolute_change'] <- df[subs, 'mean_value'] - baseline_val
      }
    }
  }
  return(df)
}

# winter temperature summary
summary_df_path <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_current+future_winter_temp_summary.csv"
summary_df <- read.csv(summary_df_path)
relative_df <- relative_to_baseline(summary_df)

# annual average summary
summary_df_path <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_current+future_scenario_summary.csv"
summary_df <- read.csv(summary_df_path)
summary_df$model_label <- factor(summary_df$model,
                                  levels=c("current", "BCC-CSM2-MR", "CNRM-ESM2-1", "GFDL-ESM4",
                                           "MIROC-ES2L", "MRI-ESM2-0", "MIROC6", "CanESM5",
                                           "IPSL-CM6A-LR", "CNRM-CM6-1"),
                                  labels=c('current', '1', '2', '3', '4', '5', '6', '7', '8', '9'))
relative_df <- relative_to_baseline(summary_df)

relative_df <- relative_df[relative_df$model != 'current', ]

summary(relative_df[(relative_df$aoi == 'Mongolia') &
                      (relative_df$summary_method == 'Monthly average across pixels') &
                      (relative_df$output == 'tmin'), 'absolute_change'])
summary(relative_df[(relative_df$aoi == 'Mongolia') &
                      (relative_df$summary_method == 'Monthly average across pixels') &
                      (relative_df$output == 'tmax'), 'absolute_change'])
summary(relative_df[(relative_df$aoi == 'Mongolia') &
                      (relative_df$summary_method == 'Monthly average across pixels') &
                      (relative_df$output == 'prec'), 'percent_change'])

tmin_df <- relative_df[(relative_df$aoi == 'Mongolia') &
                            (relative_df$summary_method == 'Monthly average across pixels') &
                            (relative_df$output == 'tmin'), c('model', 'absolute_change')]
colnames(tmin_df) <- c('model', 'absolute_change_tmin')
tmax_df <- relative_df[(relative_df$aoi == 'Mongolia') &
                         (relative_df$summary_method == 'Monthly average across pixels') &
                         (relative_df$output == 'tmax'), c('model', 'absolute_change')]
colnames(tmax_df) <- c('model', 'absolute_change_tmax')
prec_df <- relative_df[(relative_df$aoi == 'Mongolia') &
                         (relative_df$summary_method == 'Monthly average across pixels') &
                         (relative_df$output == 'prec'), c('model', 'percent_change')]
colnames(prec_df) <- c('model', 'percent_change_precip')
summary_df <- merge(tmin_df, tmax_df)
summary_df <- merge(summary_df, prec_df)
write.csv(summary_df, "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_ssp370_2061-2080_Mongolia_change_summary.csv",
          row.names=FALSE)


hist(relative_df[(relative_df$aoi == 'Eastern_steppe') &
                   (relative_df$output == 'tmin'), 'relative_value'], breaks=10)
hist(relative_df[(relative_df$aoi == 'Eastern_steppe') &
                   (relative_df$output == 'tmax'), 'relative_value'], breaks=10)
hist(relative_df[(relative_df$aoi == 'Eastern_steppe') &
                   (relative_df$output == 'prec'), 'relative_value'], breaks=10)


rel_resh <- reshape(relative_df, idvar=c('aoi', 'model', 'summary_method'),
                    timevar='output', direction="wide", drop=c('mean_value', 'model_label'))

p <- ggplot(rel_resh, aes(x=relative_value.prec, y=relative_value.tmin))
p <- p + geom_point(aes(color=aoi))
print(p)

p <- ggplot(rel_resh, aes(x=relative_value.prec, y=relative_value.tmax))
p <- p + geom_point(aes(color=aoi))
print(p)

p <- ggplot(rel_resh, aes(x=relative_value.tmin, y=relative_value.tmax))
p <- p + geom_point(aes(color=aoi))
print(p)

# take out the CanESM5 model and calculate correlation
nocanesm5 <- rel_resh[(rel_resh$model != 'CanESM5') & (rel_resh$aoi == 'Mongolia'), ]
tmin_prec_cor <- cor.test(nocanesm5$relative_value.tmin, nocanesm5$relative_value.prec)
tmin_prec_cor
tmax_prec_cor <- cor.test(nocanesm5$relative_value.tmax, nocanesm5$relative_value.prec)
tmax_prec_cor

# average across months, across pixels
subs_df <- summary_df[summary_df$summary_method == 'Monthly average across pixels', ]
p <- ggplot(subs_df, aes(x=model_label, y=mean_value))
p <- p + geom_point() + facet_grid(aoi~output, scales='free')
print(p)

# precip both ways
subs_df <- summary_df[summary_df$output == 'prec', ]
p <- ggplot(subs_df, aes(x=model_label, y=mean_value))
p <- p + geom_point() + facet_grid(summary_method~aoi, scales='free')
print(p)

# annual, eastern steppe only
subs_df <- summary_df[(summary_df$output == 'prec') &
                        (summary_df$aoi == 'Eastern_steppe') &
                        (summary_df$summary_method == 'Average annual sum across pixels'), ]
# order by decreasing average annual precip
modeled_df <- subs_df[subs_df$model != 'current', ]
model_order <- c('current', modeled_df[order(modeled_df$mean_value, decreasing=TRUE), 'model'])
subs_df$model <- factor(subs_df$model, levels=model_order)
p <- ggplot(subs_df, aes(x=model, y=mean_value))
p <- p + geom_abline(slope=0, intercept=subs_df[subs_df$model == 'current', 'mean_value'],
                     linetype='dashed')
p <- p + geom_point() + print_theme + xlab("") + ylab("Average annual precipitation (mm)")
p <- p + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_ssp370_2061-2080_eastern_steppe_annual_precip.png"
png(file=pngname, units="in", res = 150, width = 4, height=4)
print(p)
dev.off()

# time series: precip from current and CanESM5 at selected points in the eastern steppe aoi
time_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_time_series_points.csv")
p <- ggplot(time_df, aes(x=month, y=precip))
p <- p + geom_line(aes(linetype=time_period)) + ylab("Precipitation (cm)")
p <- p + facet_wrap(~id) + scale_x_continuous(breaks=seq(2, 12, by=2))
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/climate/Worldclim_ssp370_2061-2080_point_time_series.png"
png(file=pngname, units="in", res = 150, width = 7, height=5)
print(p)
dev.off()
