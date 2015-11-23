# Sensitivity analysis of forage model: forage quality

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=7, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

data_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/Sensitivity/Forage_quality/"
fig_dir <- paste(data_dir, 'figs', sep = "")

raw_dat <- read.csv(paste(data_dir, "summary.csv", sep=""), header=TRUE)
pub_dat <- read.csv(paste(data_dir, "published_values.csv", sep=""), header=TRUE,
                    stringsAsFactors=FALSE)
DMD_med <- 0.45354  # median(raw_dat$DMD)
CP_med <- 0.15354  # median(raw_dat$CP)
DMD_quan <- c(0.1, 0.27677, DMD_med, 0.62323, 0.8)
CP_quan <- c(0.01, 0.08323, CP_med, 0.22677, 0.3)

step0 <- raw_dat[which(raw_dat$step == 0), ]
step0_DMD_quan <- step0[which(step0$DMD %in% DMD_quan), ]
step0_DMD_quan$DMD <- factor(step0_DMD_quan$DMD, levels=rev(DMD_quan))
step0_CP_quan <- step0[which(step0$CP %in% CP_quan), ]
step0_CP_quan$CP <- factor(step0_CP_quan$CP, levels=rev(CP_quan))

# scatterplot: visualize
p <- ggplot(step0_CP_quan, aes(x=DMD, y=intake_forage, group=CP))
p <- p + geom_point(aes(color=CP))
# p <- p + geom_point(data=pub_dat, aes(x=DMD, y=intake_forage))
p <- p + print_theme + xlab('Dry matter digestibility (%)') + ylab('Daily forage intake (kg)')
pngname <- paste(fig_dir, "Intake~DMD.png", sep = "/")
png(file = pngname, units="in", res = 150, width=5.5, height=4)
print(p)
dev.off()

p <- ggplot(step0_CP_quan, aes(x=DMD, y=daily_gain, group=CP))
p <- p + geom_point(aes(color=CP))
# p <- p + geom_point(data=pub_dat, aes(x=DMD, y=daily_gain))
p <- p + print_theme + xlab('Dry matter digestibility (%)') + ylab('Daily liveweight gain (kg)')
pngname <- paste(fig_dir, "Gain~DMD.png", sep = "/")
png(file = pngname, units="in", res = 150, width=5.5, height=4)
print(p)
dev.off()

p <- ggplot(step0_DMD_quan, aes(x=CP, y=intake_forage, group=DMD))
p<- p + geom_point(aes(color=DMD))
# p <- p + geom_point(data=pub_dat, aes(x=CP, y=intake_forage))
p <- p + print_theme + xlab('Crude protein content (%)') + ylab('Daily forage intake (kg)')
pngname <- paste(fig_dir, "Intake~CP.png", sep = "/")
png(file = pngname, units="in", res = 150, width=5.5, height=4)
print(p)
dev.off()

p <- ggplot(step0_DMD_quan, aes(x=CP, y=daily_gain, group=DMD))
p <- p + geom_point(aes(color=DMD))
# p <- p + geom_point(data=pub_dat, aes(x=CP, y=daily_gain))
p <- p + print_theme + xlab('Crude protein content (%)') + ylab('Daily liveweight gain (kg)')
pngname <- paste(fig_dir, "Gain~CP.png", sep = "/")
png(file = pngname, units="in", res = 150, width=5.5, height=4)
print(p)
dev.off()
