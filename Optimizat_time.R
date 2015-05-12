# visualize problem size vs run time for Peter's optimizat tools
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

data_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Optimizat_timing/"
fig_dir <- paste(data_dir, 'figs', sep = "")

int_dat <- read.csv(paste(data_dir, "time_integer_solver.csv", sep = ""), header = TRUE)
int_dat$product <- int_dat$nopts * int_dat$nparcels
lin_dat <- read.csv(paste(data_dir, "time_linear_solver.csv", sep = ""), header = TRUE)
lin_dat$elapsed <- as.numeric(lin_dat$elapsed)
quad_dat <- read.csv(paste(data_dir, "time_quadratic_solver.csv", sep = ""), header = TRUE)

###### integer plot #######
restr <- int_dat[which(int_dat$nopts == 20 & int_dat$n_factors == 20), ]
p <- ggplot(restr, aes(x = n_IU, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('# implementation units') + ylab('Run time (sec)')
p <- p + ggtitle("Constant # objectives, # interventions, mip gap")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
IU_plot <- p
print(IU_plot)

pngname <- paste(fig_dir, "integer_N_IU.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(IU_plot)
dev.off()

restr <- int_dat[which(int_dat$nopts == 20 & int_dat$n_IU == 500), ]
p <- ggplot(restr, aes(x = n_factors, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('# objectives') + ylab('Run time (sec)')
p <- p + ggtitle("Constant # implementation units, # interventions, mip gap")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
factors_plot <- p
print(factors_plot)

pngname <- paste(fig_dir, "integer_N_objectives.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(factors_plot)
dev.off()

restr <- int_dat[which(int_dat$n_factors == 20 & int_dat$n_IU == 500), ]
p <- ggplot(restr, aes(x = nopts, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('# interventions') + ylab('Run time (sec)')
p <- p + ggtitle("Constant # implementation units, # objectives, mip gap")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
opts_plot <- p
print(opts_plot)

pngname <- paste(fig_dir, "integer_N_interventions.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(opts_plot)
dev.off()


restr <- int_dat[which(int_dat$mip_gap < 0.011 & int_dat$nopts == 3 & int_dat$target > 0 & int_dat$nparcels = 500), ]
p <- ggplot(restr, aes(x = target, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('Target') + ylab('Run time (sec)')
p <- p + ggtitle("Constant N parcels, N BMPs, mip gap")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
target_plot <- p

pngname <- paste(fig_dir, "integer_target.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(target_plot)
dev.off()

restr <- int_dat[which(int_dat$mip_gap < 0.011 & int_dat$target < 0.5 & int_dat$nparcels = 500), ]
p <- ggplot(restr, aes(x = nopts, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('BMPs') + ylab('Run time (sec)')
p <- p + ggtitle("Constant N parcels, mip gap, target")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
nopts_plot <- p

pngname <- paste(fig_dir, "integer_BMPs.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(nopts_plot)
dev.off()

restr <- int_dat[which(int_dat$mip_gap < 0.011 & int_dat$nopts == 12), ]
p <- ggplot(restr, aes(x = nparcels, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('Parcels') + ylab('Run time (sec)')
p <- p + ggtitle("Constant BMPs, mip gap, target")
p <- p + theme(plot.title=element_text(size=7, face=NULL))
nparcels_plot <- p

pngname <- paste(fig_dir, "integer_parcels.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(nparcels_plot)
dev.off()

####### linear plot #######
p <- ggplot(lin_dat, aes(x = nparcels, y = elapsed))
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('Parcels') + ylab('Run time (sec)')
lin_plot <- p

pngname <- paste(fig_dir, "linear.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(lin_plot)
dev.off()

######## Quadratic plots #########
quad_dat_r <- quad_dat[quad_dat$tac == 10, ]
quad_dat_r <- quad_dat_r[quad_dat_r$dusize == 150, ]
p <- ggplot(quad_dat_r, aes(x = ndus, y = elapsed_optim)) # dusize, ndus, product
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('Number decision units') + ylab('Run time (sec)')
quad_plot1 <- p

pngname <- paste(fig_dir, "quad_ndus.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(quad_plot1)
dev.off()

p <- ggplot(quad_dat_r, aes(x = dusize, y = elapsed_optim)) # dusize, ndus, product
p <- p + geom_point()
p <- p + print_theme
p <- p + xlab('Decision unit size (pixels)') + ylab('Run time (sec)')
quad_plot2 <- p
pngname <- paste(fig_dir, "quad_dusize.png", sep = "/")
png(file = pngname, units="in", res = 150, width = 4, height=4)
print(quad_plot2)
dev.off()
