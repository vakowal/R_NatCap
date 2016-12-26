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

data_dir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations'
  
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
# simulated values
studies <- c('Shem_et_al_1995')
sim_l <- list()
for(study in studies){
  sim_dat <- read.csv(paste(data_dir, study, "summary_unsupplemented_CK13x2_CG2=1_CM2div10_CM12div10_unreduced.csv", sep="/"), header=TRUE,
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
combined$sim_gain_per_intake <- combined$sim_daily_gain / combined$sim_intake_forage
combined$gain_per_intake <- combined$daily_gain / combined$intake_forage

in_test <- cor.test(combined$sim_intake_forage, combined$intake_forage, method="spearman")
in_test[['estimate']]
in_test[['p.value']]
gain_test <- cor.test(combined$sim_daily_gain, combined$daily_gain, method="spearman")
gain_test[['estimate']]
gain_test[['p.value']]
gpi_test <- cor.test(combined$sim_gain_per_intake, combined$gain_per_intake, method="spearman")
gpi_test[['estimate']]
gpi_test[['p.value']]

figdir <- paste(data_dir, study, 'unsupplemented', sep='/')

p <- ggplot(combined, aes(x=intake_forage, y=sim_intake_forage))
p <- p + geom_point() + print_theme
p <- p + xlab('Empirical daily intake (kg)') + ylab('Simulated daily intake (kg)')
min_val <- min(c(combined$intake_forage, combined$sim_intake_forage))
max_val <- max(c(combined$intake_forage, combined$sim_intake_forage))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(figdir, "intake_CK13x2_CG2=1_CM2div10_CM12div10_no_reduce.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
print(p)
dev.off()

p <- ggplot(combined, aes(x=daily_gain, y=sim_daily_gain))
p <- p + geom_point(aes(size=5)) + print_theme_l
p <- p + xlab('Empirical daily gain (kg)') + ylab('Simulated daily gain (kg)')
min_val <- min(c(combined$daily_gain, combined$sim_daily_gain))
max_val <- max(c(combined$daily_gain, combined$sim_daily_gain))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(figdir, "gain_CK13x2_CG2=1_CM2div10_CM12div10_no_reduce_large.png", sep="/")
png(file=pngname, units="in", res=150, width=9, height=8)
print(p)
dev.off()

p <- ggplot(combined, aes(x=gain_per_intake, y=sim_gain_per_intake))
p <- p + geom_point() + print_theme
p <- p + xlab('Empirical daily gain per kg intake (kg/kg)') + ylab('Simulated daily gain per kg intake (kg/kg)')
min_val <- min(c(combined$gain_per_intake, combined$sim_gain_per_intake))
max_val <- max(c(combined$gain_per_intake, combined$sim_gain_per_intake))
p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
p <- p + geom_abline(slope=1, intercept=0, linetype=2)
pngname <- paste(figdir, "gain_per_kg_intake_CK13x2_CG2=1_CM2div10_CM12div10_no_reduce.png", sep="/")
png(file=pngname, units="in", res=300, width=3.5, height=3.5)
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
