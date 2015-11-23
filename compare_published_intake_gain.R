# compare published and modeled values: intake and gain

# the goal: replicate all inputs given in publication, then present published result on
# x-axis and modeled on y-axis, for various feeds.

data_dir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations'
  
# published values
pub_dat <- read.csv(paste(data_dir, "published_values.csv", sep="/"), header=TRUE,
                    stringsAsFactors=FALSE)
# simulated values
sim_l <- list()
for(study in c('Panjaitan_et_al_2010', 'Rubanza_et_al_2005', 'Shem_et_al_1995')){
  sim_dat <- read.csv(paste(data_dir, study, "summary_supplemented.csv", sep="/"), header=TRUE,
                       stringsAsFactors=FALSE)
  sim_l[[study]] <- sim_dat
}
df <- do.call(rbind, sim_l)
df$grass_label <- as.factor(df$grass_label)

sim_mean_intake <- aggregate(df$intake_forage~df$grass_label, FUN=mean)
colnames(sim_mean_intake) <- c('grass_label', 'sim_intake_forage')
sim_mean_gain <- aggregate(df$daily_gain~df$grass_label, FUN=mean)
colnames(sim_mean_gain) <- c('grass_label', 'sim_daily_gain')
pub_dat$grass_label = factor(pub_dat$type, levels=levels(sim_mean_gain$grass_label))
combined = merge(sim_mean_intake, sim_mean_gain, by='grass_label')
combined = merge(combined, pub_dat, by='grass_label')

p <- ggplot(combined, aes(x=intake_forage, y=sim_intake_forage, group=Ref))
p <- p + geom_point(aes(colour=Ref)) + print_theme
p <- p + xlab('Empirical daily intake (kg)') + ylab('Simulated daily intake (kg)')
p <- p + xlim(c(0,10))
print(p)

p <- ggplot(combined, aes(x=daily_gain, y=sim_daily_gain, group=Ref))
p <- p + geom_point(aes(colour=Ref)) + print_theme
p <- p + xlab('Empirical daily gain (kg)') + ylab('Simulated daily gain (kg)')
# p <- p + ylim(c(0,0.75))
print(p)

