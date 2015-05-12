# Investigate results of sensitivity analysis of SRW and birth weight
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

data_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/Sensitivity/SRW/"
fig_dir <- paste(data_dir, 'figs', sep = "")

raw_dat <- read.csv(paste(data_dir, "summary.csv", sep = ""), header = TRUE)
step0 <- raw_dat[which(raw_dat$step == 0), ]
step0[which(step0$Wbirth == 37.5), 'vary'] <- 'SRW'
step0[which(step0$SRW == 550), 'vary'] <- 'Birth weight'

med_step0 <- step0[!is.na(step0$vary), ]

# scatterplot: visualize
med_step0_SRW <- med_step0[med_step0$Wbirth == 37.5, ]
p <- ggplot(med_step0_SRW, aes(x = SRW, y = intake_forage)) + geom_point()
p <- p + print_theme + xlab('SRW') + ylab('Daily forage intake (kg)')
srw_intake <- p
print(p)
p <- ggplot(med_step0_SRW, aes(x = SRW, y = daily_gain)) + geom_point()
p <- p + print_theme + xlab('SRW') + ylab('Daily liveweight gain (kg)')
print(p)

med_step0_wbirth <- med_step0[med_step0$SRW == 550, ]
p <- ggplot(med_step0_wbirth, aes(x = Wbirth, y = intake_forage)) + geom_point()
p <- p + print_theme + xlab('Birth weight') + ylab('Daily forage intake (kg)')
wbirth_intake <- p
print(p)
p <- ggplot(med_step0_wbirth, aes(x = Wbirth, y = daily_gain)) + geom_point()
p <- p + print_theme + xlab('Birth weight') + ylab('Daily liveweight gain (kg)')
print(p)

pngname <- paste(fig_dir, "Intake_scatterplot.png", sep = "/")
png(pngname, width = 6, height=2.75, units = "in", res = 600)
grid.arrange(arrangeGrob(wbirth_intake, srw_intake, nrow=1))
dev.off()

## boxplots
p <- ggplot(med_step0, aes(x = vary, y = max_intake)) + geom_boxplot()
p <- p + print_theme + xlab('Varied quantity') + ylab('Maximum daily intake (kg)')
#p <- p + ggtitle("Effect of SRW and birth weight on maximum intake")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
print(p)

pngname <- paste(fig_dir, "Max_intake.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(p)
dev.off()

p <- ggplot(med_step0, aes(x = vary, y = intake_forage)) + geom_boxplot()
p <- p + print_theme + xlab('Varied quantity') + ylab('Daily forage intake (kg)')
#p <- p + ggtitle("Effect of SRW and birth weight on maximum intake")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
print(p)

pngname <- paste(fig_dir, "Intake.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(p)
dev.off()

p <- ggplot(med_step0, aes(x = vary, y = daily_gain)) + geom_boxplot()
p <- p + print_theme + xlab('') + ylab('Daily liveweight gain (kg)')
#p <- p + ggtitle("Effect of SRW and birth weight on maximum intake")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
print(p)

pngname <- paste(fig_dir, "Daily_gain.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(p)
dev.off()

### intake forced to be same in all steps, all treatments ###
gain_dat <- read.csv(paste(data_dir, "summary_forced_intake.csv", sep = ""), header = TRUE)
step0g <- gain_dat[which(gain_dat$step == 0), ]
step0g[which(step0g$Wbirth == 37.5), 'vary'] <- 'SRW'
step0g[which(step0g$SRW == 550), 'vary'] <- 'Birth weight'
med_step0g <- step0g[!is.na(step0g$vary), ]

# scatterplot: visualize
med_step0_SRW <- med_step0g[med_step0g$Wbirth == 37.5, ]
p <- ggplot(med_step0_SRW, aes(x = SRW, y = daily_gain)) + geom_point()
p <- p + print_theme + xlab('SRW') + ylab('Daily liveweight gain (kg)')
srw_gain <- p
print(p)

med_step0_wbirth <- med_step0g[med_step0g$SRW == 550, ]
p <- ggplot(med_step0_wbirth, aes(x = Wbirth, y = daily_gain)) + geom_point()
p <- p + print_theme + xlab('Birth weight') + ylab('Daily liveweight gain (kg)')
wbirth_gain <- p
print(p)

pngname <- paste(fig_dir, "Gain_scatterplot_forced_intake.png", sep = "/")
png(pngname, width = 6, height=2.75, units = "in", res = 600)
grid.arrange(arrangeGrob(wbirth_gain, srw_gain, nrow=1))
dev.off()

p <- ggplot(med_step0g, aes(x = SRW, y = daily_gain)) + geom_point()
print(p)
p <- ggplot(med_step0g, aes(x = Wbirth, y = daily_gain)) + geom_point()
print(p)

p <- ggplot(med_step0g, aes(x = vary, y = daily_gain)) + geom_boxplot()
p <- p + print_theme + xlab('') + ylab('Daily liveweight gain (kg)')
print(p)

pngname <- paste(fig_dir, "Daily_gain_forced_intake.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(p)
dev.off()