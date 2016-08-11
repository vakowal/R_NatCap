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

sum_df$avg_monthly_gain <- sum_df$gain_kg / sum_df$duration
metrics <- c('avg_biomass', "gain_kg", "total_biomass", "avg_monthly_gain")

rotated <- subset(sum_df, duration < 24)

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
    pngname <- paste(img_dir, "/", metric, "~", pr, ".png", sep="")
    png(file=pngname, units="in", res=150, width=7, height=3)
    print(p)
    dev.off()
  }
}

# mean of rotated schedules vs non-rotated schedule
constant <- subset(sum_df, duration == 24)
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
    # p <- p + geom_point()
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

for(metric in metrics){
  p <- ggplot(mean_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  # p <- p + geom_point()
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

rotated_df <- merge(rotated, precip_df, by="subbasin")
metric_subset <- c("avg_biomass", "avg_monthly_gain")
for(metric in metric_subset){
  p <- ggplot(rotated_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  # p <- p + geom_point()
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