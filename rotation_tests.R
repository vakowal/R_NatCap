# what is effect of rotation on the forage model?

library(ggplot2)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/comparison_8.9.16.csv"
sum_df <- read.csv(sum_csv)

metrics <- c('avg_biomass', "total_biomass", "avg_monthly_gain")

rotated <- subset(sum_df, duration < 24)
rotated$avg_monthly_gain <- rotated$gain_kg / 12

# precipitation
precip_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/avg_precip"
precip_df <- data.frame("subbasin"=numeric(14), "mean_precip"=numeric(14),
                        "sum_precip"=numeric(14), "std_precip"=numeric(14))
for(sbasin in 1:14){
  precip_csv <- paste(precip_dir, "/", sbasin, ".csv", sep="")
  df <- read.csv(precip_csv, header=FALSE)
  precip_df[sbasin, "subbasin"] <- sbasin
  precip_df[sbasin, "mean_precip"] <- mean(df$V1)
  precip_df[sbasin, "sum_precip"] <- sum(df$V1)
  precip_df[sbasin, "std_precip"] <- sd(df$V1)
}

# what is the difference in each metric between rotation schedules of the same duration and subbasin?
ids <- unique(rotated[, c('subbasin', 'duration')])
diff <- data.frame(abs(diff(as.matrix(rotated[, metrics]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
colnames(odd_diff) <- paste("diff", colnames(odd_diff), sep="_")
diff_df <- cbind(odd_diff, ids)
diff_df <- merge(diff_df, precip_df, by="subbasin")
summary_df <- diff_df
for(m in metrics){
  summary_df[, paste("mean", m, sep="_")] <- diff_df[, paste("diff", m, sep="_")]/2
}

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/figs"
precip_descrips <- colnames(precip_df)[c(2, 3, 4)]
  
for(metric in metrics){
  dif_metric <- paste("diff", metric, sep="_")
  for(pr in precip_descrips){
    p <- ggplot(summary_df, aes_string(x=pr, y=dif_metric,
                                    group="duration"))
    p <- p + geom_point(aes(colour=as.factor(duration)))
    p <- p + print_theme
    pngname <- paste(img_dir, "/diff_", metric, "~", pr, ".png", sep="")
    png(file=pngname, units="in", res=150, width=7, height=3)
    print(p)
    dev.off()
  }
}

# mean of metrics within rotated schedules, vs non-rotated (full-year) schedule
constant <- subset(sum_df, duration == 24)
constant$avg_monthly_gain <- constant$gain_kg / 24
mean_df <- aggregate(rotated[, metrics], by=list(
                     rotated$subbasin, rotated$duration),
                     mean)
colnames(mean_df)[1] <- "subbasin"
colnames(mean_df)[2] <- "duration"
mean_df <- merge(mean_df, precip_df, by="subbasin")
constant_df <- merge(constant, precip_df, by="subbasin")
for(metric in metrics){
  for(pr in precip_descrips){
    p <- ggplot(mean_df, aes_string(x=pr, y=metric,
                                       group="duration"))
    p <- p + geom_jitter(aes(colour=as.factor(duration)))
    p <- p + geom_point(data=constant_df, aes_string(
                        x=pr, y=metric))
    p <- p + print_theme
    pngname <- paste(img_dir, "/", metric, "~", pr,
                     "_mean_rotation_vs_full_year.png", sep="")
    png(file=pngname, units="in", res=150, width=7, height=3)
    print(p)
    dev.off()
  }
}

# means by subbasin instead of precip descriptors
for(metric in metrics){
  p <- ggplot(mean_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  p <- p + geom_jitter(aes(colour=as.factor(duration)),
                       width=0.3)
  p <- p + geom_point(data=constant_df, aes_string(
                      x="subbasin", y=metric))
  p <- p + print_theme
  pngname <- paste(img_dir, "/", metric, "~subbasin",
                   "_mean_rotation_vs_full_year.png", sep="")
  png(file=pngname, units="in", res=150, width=7, height=3)
  print(p)
  dev.off()
}

# raw values: rotational schedules vs non-rotated (full-year) schedule
rotated_df <- merge(rotated, precip_df, by="subbasin")
for(metric in metrics){
  p <- ggplot(rotated_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  p <- p + geom_jitter(aes(colour=as.factor(duration)),
                       width=0.3)
  p <- p + geom_point(data=constant_df, aes_string(
    x="subbasin", y=metric))
  p <- p + print_theme
  pngname <- paste(img_dir, "/", metric, "~subbasin",
                   "_rotation_vs_full_year.png", sep="")
  png(file=pngname, units="in", res=150, width=7, height=3)
  print(p)
  dev.off()
}

###### differences in rotated schedules vs differences between interventions
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/figs"
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/rotation_high_sd/comparison_8.22.16.csv"
sum_df <- read.csv(sum_csv)

ids <- unique(sum_df[, c('subbasin', 'duration', 'sd_level', 'animal_type')])
sum_df$id <- paste(sum_df$subbasin, sum_df$duration, sum_df$sd_level, sum_df$animal_type, sep=".")
biomass_means <- aggregate(sum_df$avg_biomass, by=list(sum_df$id), FUN=mean)
colnames(biomass_means) <- c('id', 'avg_biomass')
sum_df_sub <- unique(sum_df[, colnames(sum_df)[c(1, 5, 8, 9, 10)]])
biomass_means <- merge(sum_df_sub, biomass_means, by='id')
biomass_means[(biomass_means$duration == 204 & 
                 biomass_means$sd_level == 'rechigh'), 'level'] <- 'high'
biomass_means[(biomass_means$duration == 4 & 
                 biomass_means$sd_level == 'rechigh'), 'level'] <- 'high-rot'
biomass_means[(biomass_means$duration == 204 & 
                 biomass_means$sd_level == 'low'), 'level'] <- 'low'
biomass_means[(biomass_means$duration == 4 & 
                 biomass_means$sd_level == 'low'), 'level'] <- 'low-rot'
biomass_means$intervention <- paste(biomass_means$animal_type, biomass_means$level, sep="-")

## calculate all pairwise differences between interventions within subbasin
diff_list <- list()
sb <- unique(biomass_means$subbasin)
for(sbasin in sb){
  subs <- biomass_means[which(biomass_means$subbasin == sbasin), 'avg_biomass']
  diffs <- c(dist(subs))
  df <- data.frame('subbasin'=rep(sbasin, length(diffs)), 'diff'=diffs)
  diff_list[[sbasin]] <- df
}
diff_df <- do.call(rbind, diff_list)
diff_df$difference_type <- 'intervention'

# calculate all differences between schedules of the same intervention type
sbasins <- unique(sum_df$subbasin)
rotated <- subset(sum_df, duration < 204)
diff <- data.frame(abs(diff(as.matrix(rotated[, c('avg_yearly_gain', 'avg_biomass')]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
odd_diff <- cbind(odd_diff, sbasins)
odd_diff <- odd_diff[, c('sbasins', 'avg_biomass')]
odd_diff$difference_type <- 'schedule'

colnames(odd_diff) <- colnames(diff_df)
plot_df <- rbind(diff_df, odd_diff)

p <- ggplot(plot_df, aes(x=subbasin, y=diff, colour=difference_type))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + ylab('diff: avg monthly biomass (kg/ha)')
pngname <- paste(imgpath, "diff_avg_biomass_intervention_v_schedule.png", sep="/")
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

# how does difference between schedules vary by intervention?
rotated <- subset(sum_df, duration < 204)
ids <- unique(rotated[, c('animal_type', 'subbasin', 'sd_level')])
diff <- data.frame(abs(diff(as.matrix(rotated[, c('avg_yearly_gain', 'avg_biomass')]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
odd_diff <- cbind(odd_diff, ids)
odd_diff$intervention <- paste(odd_diff$animal_type, odd_diff$sd_level, sep="-")
odd_diff$subbasin <- as.factor(odd_diff$subbasin)

p <- ggplot(odd_diff, aes(x=intervention, y=avg_biomass))
p <- p + geom_point(aes(colour=as.factor(subbasin)))
p <- p + ylab("diff avg monthly biomass between schedules")
pngname <- "C:/Users/Ginger/Desktop/diff_avg_biomass_by_intervention.png"
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

p <- ggplot(odd_diff, aes(x=intervention, y=avg_yearly_gain))
p <- p + geom_point(aes(colour=as.factor(subbasin)))
p <- p + ylab("diff avg yearly gain between schedules")
pngname <- "C:/Users/Ginger/Desktop/diff_avg_yearly_gain_by_intervention.png"
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

## how does average monthly gain differ between rotated and non-rotated schedules?
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/rotation_high_sd/comparison_8.25.16.csv"
sum_df <- read.csv(sum_csv)

sum_df$id <- paste(sum_df$subbasin, sum_df$sd_level, sum_df$animal_type, sep=".")
rotated <- subset(sum_df, duration < 204)
full <- subset(sum_df, duration == 204)
monthly_gain_means <- aggregate(rotated$avg_gain_kg, by=list(rotated$id), FUN=mean)
colnames(monthly_gain_means) <- c('id', 'avg_monthly_gain')
sum_df_sub <- unique(rotated[, colnames(rotated)[c(1, 8, 9, 10)]])
monthly_gain_rotated <- merge(sum_df_sub, monthly_gain_means, by='id')
monthly_gain_rotated$schedule <- 'rotation'
full_sub <- full[, colnames(full)[c(10, 1, 8, 9, 3)]]
colnames(full_sub)[5] <- 'avg_monthly_gain'
full_sub$schedule <- 'full'
plot_df <- rbind(monthly_gain_rotated, full_sub)
levels(plot_df$sd_level) <- c("high density", "low density")

p <- ggplot(plot_df, aes(x=subbasin, y=avg_monthly_gain, colour=schedule))
p <- p + geom_point()
p <- p + facet_grid(animal_type~sd_level, scales='free')
pngname <- paste(imgpath, "avg_monthly_gain_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()

## avg monthly biomass
ids <- unique(sum_df[, c('subbasin', 'duration', 'sd_level', 'animal_type')])
sum_df$id <- paste(sum_df$subbasin, sum_df$duration, sum_df$sd_level, sum_df$animal_type, sep=".")
biomass_means <- aggregate(sum_df$avg_biomass, by=list(sum_df$id), FUN=mean)
colnames(biomass_means) <- c('id', 'avg_biomass')
sum_df_sub <- unique(sum_df[, colnames(sum_df)[c(1, 5, 8, 9, 10)]])
biomass_means <- merge(sum_df_sub, biomass_means, by='id')
biomass_means[which(biomass_means$duration == 4), 'schedule'] <- "rotation"
biomass_means[which(biomass_means$duration == 204), 'schedule'] <- "full"
biomass_means[which(biomass_means$sd_level == 'rechigh'), 'stocking_density'] <- "high"
biomass_means[which(biomass_means$sd_level == 'reclow'), 'stocking_density'] <- "low"

p <- ggplot(biomass_means, aes(x=subbasin, y=avg_biomass, colour=schedule))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + facet_grid(animal_type~stocking_density, scales='free')
p <- p + ylab("avg monthly biomass (kg/ha)")
pngname <- paste(imgpath, "avg_biomass_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()

## total yearly gain
marginal_table <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_table_8.25.16.csv"
marg_df <- read.csv(marginal_table)
marg_df$schedule <- "full"
marg_df[which(marg_df$density == 'ighrot'), 'schedule'] <- "rotation"
marg_df[which(marg_df$density == 'lowrot'), 'schedule'] <- "rotation"
marg_df$stocking_density <- marg_df$density
marg_df[which(marg_df$density == 'ighrot'), 'stocking_density'] <- "high"
marg_df[which(marg_df$density == 'lowrot'), 'stocking_density'] <- "low"

p <- ggplot(marg_df, aes(x=subbasin, y=total_delta_weight_kg, colour=schedule))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + facet_grid(animal~stocking_density, scales='free')
p <- p + ylab("total yearly gain (kg)")
pngname <- paste(imgpath, "total_yearly_gain_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()