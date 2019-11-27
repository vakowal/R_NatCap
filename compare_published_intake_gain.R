# compare published and modeled values: intake and gain

# the goal: replicate all inputs given in publication, then present published result on
# x-axis and modeled on y-axis, for various feeds.

library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()
print_theme_l <- theme_bw() + theme(axis.title=element_text(size=22),
                       axis.text=element_text(size=22))

data_dir <- 'C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations'
  
# published values
pub_dat <- read.csv(paste(data_dir, "published_values.csv", sep="/"), header=TRUE,
                    stringsAsFactors=FALSE)
# simulated values
sim_l <- list()
for(study in c('Panjaitan_et_al_2010', 'Rubanza_et_al_2005', 'Shem_et_al_1995')){
  sim_dat <- read.csv(paste(data_dir, study, "summary.csv", sep="/"), header=TRUE,
                       stringsAsFactors=FALSE)
  sim_l[[study]] <- sim_dat
}
df <- do.call(rbind, sim_l)
df$grass_label <- as.factor(df$grass_label)
df$intake_forage <- as.numeric(df$intake_forage)
df$daily_gain <- as.numeric(df$daily_gain)

sim_mean_intake <- aggregate(df$intake_forage~df$grass_label, FUN=mean)
colnames(sim_mean_intake) <- c('grass_label', 'sim_intake_forage')
sim_mean_gain <- aggregate(df$daily_gain~df$grass_label, FUN=mean)
colnames(sim_mean_gain) <- c('grass_label', 'sim_daily_gain')
pub_dat$grass_label = factor(pub_dat$type)  #, levels=levels(sim_mean_gain$grass_label))
combined = merge(sim_mean_intake, sim_mean_gain, by='grass_label')
combined = merge(combined, pub_dat, by='grass_label')

study <- 'Panjaitan_et_al'
subset <- combined[which(combined$Ref == study), ]
figdir <- paste(data_dir, study, sep='/')

p <- ggplot(subset, aes(x=intake_forage, y=sim_intake_forage))
p <- p + geom_point() + print_theme
p <- p + xlab('Empirical daily intake (kg)') + ylab('Simulated daily intake (kg)')
min_val <- min(c(subset$intake_forage, subset$sim_intake_forage))
max_val <- max(c(subset$intake_forage, subset$sim_intake_forage))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(figdir, "intake.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

p <- ggplot(subset, aes(x=daily_gain, y=sim_daily_gain))
p <- p + geom_point() + print_theme
p <- p + xlab('Empirical daily gain (kg)') + ylab('Simulated daily gain (kg)')
min_val <- min(c(subset$daily_gain, subset$sim_daily_gain))
max_val <- max(c(subset$daily_gain, subset$sim_daily_gain))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(figdir, "gain.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

summary_list <- list()
for(ref in unique(combined$Ref)){
  subset <- combined[which(combined$Ref == ref), ]
  in_test <- cor.test(subset$sim_intake_forage, subset$intake_forage, method="spearman")
  in_est <- in_test[['estimate']]
  in_pval <- in_test[['p.value']]
  gain_test <- cor.test(subset$sim_intake_forage, subset$intake_forage, method="spearman")
  gain_est <- gain_test[['estimate']]
  gain_pval <- gain_test[['p.value']]
  summary_list[[ref]] <- data.frame(ref, gain_est, in_est, gain_pval, in_pval)
}
summary_df <- do.call(rbind, summary_list)

############ Shem et al only
# published values
pub_dat <- read.csv(paste(data_dir, "published_values.csv", sep="/"), header=TRUE,
                    stringsAsFactors=FALSE)
pub_dat <- pub_dat[pub_dat$Ref == "Shem_et_al", ]
# simulated values
study <- 'Shem_et_al_1995'
sim_l <- list()
sim_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/Shem_et_al_1995/revisions_10_12/summary_unsupplemented"
sim_dat <- read.csv(paste(sim_dir, "summary.csv",  sep="/"), header=TRUE,
                      stringsAsFactors=FALSE)  # "summary_unsupplemented_CK13x2_CG2=1_CM2div10_CM12div10_unreduced.csv",
sim_l[[study]] <- sim_dat
df <- do.call(rbind, sim_l)
df$grass_label <- as.factor(df$grass_label)
df$intake_forage <- as.numeric(df$intake_forage)
df$daily_gain <- as.numeric(df$daily_gain)

sim_mean_intake <- aggregate(df$intake_forage~df$grass_label + df$SRW, FUN=mean)
colnames(sim_mean_intake) <- c('grass_label', 'SRW', 'sim_intake_forage')
sim_mean_gain <- aggregate(df$daily_gain~df$grass_label + df$SRW, FUN=mean)
colnames(sim_mean_gain) <- c('grass_label', 'SRW', 'sim_daily_gain')
pub_dat$grass_label = factor(pub_dat$type)  #, levels=levels(sim_mean_gain$grass_label))
combined = merge(sim_mean_intake, sim_mean_gain)
combined = merge(combined, pub_dat, by='grass_label', all=TRUE)
combined$sim_gain_per_intake <- combined$sim_daily_gain / combined$sim_intake_forage
combined$gain_per_intake <- combined$daily_gain / combined$intake_forage
# bias in intake: observed - predicted
combined$diff_intake <- combined$intake_forage - combined$sim_intake_forage
combined$diff_gpi <- combined$sim_gain_per_intake - combined$gain_per_intake

# mean bias in predicting intake, for each SRW
mean_bias_intake_by_SRW <- aggregate(diff_intake~SRW, data=combined, FUN=mean)
diff_gpi_SRW <- aggregate(diff_gpi~SRW, data=combined, FUN=mean)
mean_intake_SRW <- aggregate(intake_forage~SRW, data=combined, FUN=mean)
mean_gpi_SRW <- aggregate(gain_per_intake~SRW, data=combined, FUN=mean)

imgdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/Shem_et_al_1995/revisions_10_19/summary_unsupplemented"

numr <- length(unique(combined$SRW))
cor_df <- data.frame("SRW"=numeric(numr),
                     'pearson_cor_intake'=numeric(numr),
                     'pearson_p_intake'=numeric(numr),
                     'spearman_cor_intake'=numeric(numr),
                     'spearman_p_intake'=numeric(numr),
                     'pearson_cor_gpi'=numeric(numr),
                     'pearson_p_gpi'= numeric(numr),
                     'spearman_cor_gpi'=numeric(numr),
                     'spearman_p_gpi'=numeric(numr))
for(i in 1:length(unique(combined$SRW))){
  SRW <- unique(combined$SRW)[i]
  cor_df[i, 'SRW'] <- SRW
  subs <- combined[combined$SRW == SRW, ]
  in_test_p <- cor.test(subs$sim_intake_forage, subs$intake_forage, method="pearson")
  cor_df[i, 'pearson_cor_intake'] <- in_test_p[['estimate']]
  cor_df[i, 'pearson_p_intake'] <- in_test_p[['p.value']]
  gpi_test_p <- cor.test(subs$sim_gain_per_intake, subs$gain_per_intake, method="pearson")
  cor_df[i, 'pearson_cor_gpi'] <- gpi_test_p[['estimate']]
  cor_df[i, 'pearson_p_gpi'] <- gpi_test_p[['p.value']]
  in_test_s <- cor.test(subs$sim_intake_forage, subs$intake_forage, method="spearman")
  cor_df[i, 'spearman_cor_intake'] <- in_test_s[['estimate']]
  cor_df[i, 'spearman_p_intake'] <- in_test_s[['p.value']]
  gpi_test_s <- cor.test(subs$sim_gain_per_intake, subs$gain_per_intake, method="spearman")
  cor_df[i, 'spearman_cor_gpi'] <- gpi_test_s[['estimate']]
  cor_df[i, 'spearman_p_gpi'] <- gpi_test_s[['p.value']]
}
write.csv(cor_df, paste(imgdir, "cor_summary.csv", sep="/"))

for(SRW in unique(combined$SRW)){
  subs <- combined[combined$SRW == SRW, ]
  p <- ggplot(subs, aes(x=intake_forage, y=sim_intake_forage))
  p <- p + geom_point()
  min_val <- min(c(subs$intake_forage, subs$sim_intake_forage) - 0.1)
  max_val <- max(c(subs$intake_forage, subs$sim_intake_forage) + 0.1)
  p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
  p <- p + geom_abline(slope=1, intercept=0, linetype=2)
  p <- p + ggtitle(paste("Intake: SRW", SRW))
  pngname <- paste(imgdir, paste("intake_SRW_", SRW, ".png", sep=""),
                   sep="/")
  png(file=pngname, units="in", res=300, width=3.5, height=3.5)
  print(p)
  dev.off()
}

for(SRW in unique(combined$SRW)){
  subs <- combined[combined$SRW == SRW, ]
  p <- ggplot(subs, aes(x=gain_per_intake, y=sim_gain_per_intake))
  p <- p + geom_point()
  min_val <- min(c(subs$gain_per_intake, subs$sim_gain_per_intake) - 0.001)
  max_val <- max(c(subs$gain_per_intake, subs$sim_gain_per_intake) + 0.001)
  p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
  p <- p + geom_abline(slope=1, intercept=0, linetype=2)
  p <- p + ggtitle(paste("Gain per unit Intake: SRW", SRW))
  pngname <- paste(imgdir, paste("gain_per_intake_SRW_", SRW, ".png", sep=""),
                   sep="/")
  png(file=pngname, units="in", res=300, width=3.5, height=3.5)
  print(p)
  dev.off()
}

# plots for MS: best matched SRW
subs <- combined[combined$SRW == 160, ]
p <- ggplot(subs, aes(x=sim_intake_forage, y=intake_forage))
p <- p + geom_point() + print_theme
p <- p + ylab('Empirical intake (kg/day)') + xlab('Simulated intake (kg/day)')
min_val <- min(c(subs$intake_forage, subs$sim_intake_forage) - 0.1)
max_val <- max(c(subs$intake_forage, subs$sim_intake_forage) + 0.1)
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(imgdir, "intake_SRW160_MS.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

subs <- combined[combined$SRW == 600, ]
p <- ggplot(subs, aes(x=gain_per_intake, y=sim_gain_per_intake))
p <- p + geom_point() + print_theme
p <- p + xlab('Empirical daily gain per kg intake (kg)') + ylab('Simulated daily gain per kg intake (kg)')
min_val <- min(c(subs$gain_per_intake, subs$sim_gain_per_intake))
max_val <- max(c(subs$gain_per_intake, subs$sim_gain_per_intake))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(imgdir, "gain_per_kg_intake_SRW600_MS.png", sep="/")
png(file=pngname, units="in", res=300, width=3.66, height=3.5)
print(p)
dev.off()

### Rubanza et al simple test
study <- 'Rubanza_et_al_2005'
sim_dat <- read.csv(paste(data_dir, study, "summary_force_supp_force_intake.csv", sep="/"), header=TRUE,
                    stringsAsFactors=FALSE)
df <- sim_dat
sim_mean_intake <- aggregate(df$intake_forage~df$supp_level, FUN=mean)
sim_CPI <- aggregate(df$CPI_forage~df$supp_level, FUN=mean)
sim_MEI <- aggregate(df$ME_intake_total~df$supp_level, FUN=mean)
sim_gain <- aggregate(df$daily_gain~df$supp_level, FUN=mean)
gain_t1 <- aggregate(df$daily_gain_t1~df$supp_level, FUN=mean)
gain_t1
sim_mean_intake
sim_CPI
sim_MEI
sim_gain
