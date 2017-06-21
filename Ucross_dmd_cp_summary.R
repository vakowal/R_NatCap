library(ggplot2)

kam_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Western_US/Ucross/grass_cp_dmd/kamstra_1973_CP_DMD.csv")

# values in kamstra 1973 already sampled same date

kam_df$grass <- factor(kam_df$grass)
p <- ggplot(kam_df, aes(x=protein_., y=cellulose_digestion_., group=grass))
p <- p + geom_point(aes(shape=grass))
print(p)

mod <- lm(cellulose_digestion_.~protein_., data=kam_df)
summary(mod)

mean_by_grass <- aggregate(protein_.~grass, data=kam_df, FUN=mean)

cp_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/Data/Ucross/grass_cp_dmd/protein_summary.csv")
mean_by_grass <- aggregate(protein~grass, data=cp_summary, FUN=mean)
cp_summary$grass <- factor(cp_summary$grass,
                           levels=c("Idaho fescue", "Blue grama", 
                                    "Green needlegrass", "Western wheatgrass"))

p <- ggplot(cp_summary, aes(x=grass, y=protein))
p <- p + geom_boxplot()
p <- p + geom_point()
p <- p + geom_point(data=mean_by_grass, aes(x=grass, y=protein),
                    shape=3, size=5)
p <- p + xlab("") + ylab("crude protein (%)")
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Western_US/Ucross/grass_cp_live_dead_combined_summary.png"
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

bedell_dat <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Western_US/Ucross/grass_cp_dmd/Bedell_1980_Idaho_fescue_CP_DMD.csv")
bed_cp <- bedell_dat[which(bedell_dat$label == "crude protein"), ]
bed_dmd <- bedell_dat[which(bedell_dat$label == 'digestibility'), ]
month_out <- seq(min(bed_cp$month), max(bed_dmd$month), length.out=20)

cp_interp <- approx(x=bed_cp$month, y=bed_cp$measture, xout=month_out)
dmd_interp <- approx(x=bed_dmd$month, y=bed_dmd$measture, xout=month_out)

interp_df <- rbind(data.frame('month'=month_out, 'measture'=cp_interp$y,
                              'label'='cp_interp'),
                   data.frame('month'=month_out, 'measture'=dmd_interp$y,
                              'label'='dmd_interp'))

plot_df <- rbind(interp_df, bedell_dat)
p <- ggplot(plot_df, aes(x=month, y=measture, group=label))
p <- p + geom_point(aes(shape=label))
print(p)

p <- ggplot(interp_df, aes(x=month, y=measture, group=label))
p <- p + geom_point(aes(shape=label))
print(p)

lm_df <- data.frame('month'=month_out, 'crude_protein'=cp_interp$y,
                    'dmd'=dmd_interp$y)
p <- ggplot(lm_df, aes(x=crude_protein, y=dmd))
p <- p + geom_point()
print(p)

mod <- lm(dmd_interp$y~cp_interp$y)
summary(mod)
