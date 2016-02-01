## plot Unilever results for Becky's MS
## Ginger Kowal 2.12.15

## some lines that are commented out are aesthetic options I played with
## but decided against, you may wish to re-implement

library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

color <- FALSE # use color instead of line type to distinguish factors?

# I stole this from stackoverflow: get legend from one plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# general ggplot theme for print figs
print_theme <- theme(strip.text.y=element_text(size=9), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=9), 
                     axis.title.y=element_text(size=9),
                     axis.text=element_text(size=9),
                     plot.title=element_text(size=9, face="bold"),
                     legend.text=element_text(size=9),
                     legend.title=element_text(size=9)) + theme_bw()

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Unilever/Manuscript materials' 
figdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Unilever/Manuscript materials/figs'
all_data <- read.table(paste(datadir, 'fig_data.txt', sep = "/"), header = TRUE, sep="\t")

# define grouping factors
all_data$actual_baseline <- factor(all_data$actual.baseline, levels = c("baseline", "actual"))
all_data$site <- factor(all_data$site, levels = c("Iowa", "Heilongjiang", "Jiangxi", "Mato Grosso", "synthetic")) 
all_data$pattern <- factor(all_data$pattern, levels = c("To stream", "From stream", "Buffer", "From cropland"),
                           labels = c("To stream", "From stream", "From stream + buffer", "From cropland"))
all_data$k <- factor(all_data$k, levels = c(1, 1.5, 2, 2.5))
all_data$ic <- factor(all_data$ic, levels = c(0.25, 0.5, 0.75))
all_data$extent <- factor(all_data$extent, levels = c("regular", "double"))
all_data$slope <- factor(all_data$slope, levels = c("constant", "broken"),
                         labels = c("constant", "variable"))

# these line types or colors will map to a factor level in the defined order
lines <- c("solid", "longdash", "dotted", "twodash")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00", "#000000")

# make fig 2 first
fig2_data <- all_data[(all_data$percent.converted >= 0.1 & 
                  all_data$percent.converted <= 0.95 &
                  all_data$site %in% c("Iowa", "Heilongjiang", "Jiangxi", "Mato Grosso")), ]

# build the ggplot figure
# fig2_data$perc <- fig2_data$percent.converted * 100
p <- ggplot(fig2_data, aes(x = percent.converted, y = export, group = pattern))
# p <- ggplot(fig2_data, aes(x = perc, y = export, group = pattern))
# map line types or colors defined above to factor levels
if (color){
  p <- p + geom_line(aes(colour = pattern), size = 0.8) + print_theme
  p <- p + scale_colour_manual(values = cbPalette, name = "", guide = guide_legend(nrow = 2))
} else {
  p <- p + geom_line(aes(linetype = pattern), size = 0.8) + print_theme
  p <- p + scale_linetype_manual(values = lines, name = "", guide = guide_legend(nrow = 2))
}
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3.7, "line"))
p <- p + theme(legend.margin=unit(-0.7,"cm")) 
p <- p + theme(legend.position="bottom")
p <- p + theme(plot.margin=unit(c(0.01, 0.01, 0.01, 0.01), "cm"))
p <- p + scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), labels = percent)
p <- p + theme(axis.text.y=element_text(size=9), axis.text.x=element_text(size=9))

# this is the part that builds the grid
p <- p + facet_grid(site ~ actual_baseline, scales = "free")

# text annotation
len <- 8

vars <- data.frame(expand.grid(levels(fig2_data$site), levels(fig2_data$actual_baseline)))
vars <- vars[c(1:4, 6:9), ]
colnames(vars) <- c("site", "actual_baseline")
y_pos = c(38, 35, 120, 9.3, 38, 35, 120, 9.3)
dat <- data.frame(x = rep(0.15, len), y = y_pos, vars, labs=letters[1:len])

p <- p + geom_text(aes(x, y, label=labs, group=NULL),data=dat)

p <- p + xlab("Percent converted") + ylab("Marginal change in sediment export (T / ha / yr)")
# p <- p + theme(axis.ticks = element_blank())
fig2 <- p

# take a look
print(fig2)

# export the figure to file
if(color){
  jpgname <- paste(figdir, "Fig2_color.jpg", sep = "/")
} else {
  jpgname <- paste(figdir, "Fig2.jpg", sep = "/")
}
ggsave(filename = jpgname, plot = fig2, width = 5, height=9, units = "in", dpi = 600)

# new figure (USLE) 1.15.16, fig 3
fig3_dat <- read.table(paste(datadir, 'fig_data_1.15.16.txt', sep="/"), header=TRUE, sep="\t")
fig3_dat$site <- factor(fig3_dat$site, levels = c("Iowa", "Heilongjiang", "Jiangxi", "Mato Grosso")) 
fig3_dat$pattern <- factor(fig3_dat$pattern, levels=c("To stream", "From stream", "Buffer", "From cropland"),
                           labels = c("To stream", "From stream", "From stream + buffer", "From cropland"))
lines <- c("solid", "longdash", "dotted", "twodash")
fig3_dat$eros_div = fig3_dat$erosion.potential / 100000000
p <- ggplot(fig3_dat, aes(x=percent.converted, y=eros_div, group=pattern))
p <- p + geom_line(aes(linetype=pattern), size=0.4) + print_theme
p <- p + scale_linetype_manual(values = lines, name = "", guide = guide_legend(nrow = 2))
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.height=unit(0.5, "line"))
p <- p + theme(legend.key.width=unit(3.7, "line"))
p <- p + theme(legend.position="bottom")
# for a-d: p <- p + theme(plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  
p <- p + theme(plot.margin=unit(c(0.05, 0.1, -0.1, 0.1), "cm")) # for a-h: 
p <- p + theme(legend.margin=unit(-0.2, "cm"))
p <- p + theme(legend.text=element_text(size=6))
p <- p + theme(axis.title.x=element_text(size=6))
p <- p + scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), labels = percent)
p <- p + theme(axis.text.y=element_text(size=6), axis.text.x=element_text(size=6))
p <- p + theme(axis.title.y=element_text(size=7))
p <- p + ylab(expression(atop("Erosion potential", paste("(100,000,000 T / yr)"))))
p <- p + facet_wrap(~ site, nrow=1, scales = "free") + theme(strip.background = element_blank(),
                                                   strip.text.x = element_blank())
fig3a <- p
print(fig3a)

# bottom part
fig3_dat$change_div <- fig3_dat$marginal.change / 100000000
p <- ggplot(fig3_dat, aes(x=percent.converted, y=marginal.change, group=pattern))
p <- p + geom_line(aes(linetype=pattern), size=0.4) + print_theme
p <- p + scale_linetype_manual(values = lines, name = "", guide = guide_legend(nrow = 2))
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.height=unit(0.5, "line"))
p <- p + theme(legend.key.width=unit(3.7, "line"))
p <- p + theme(legend.position="bottom")
p <- p + theme(plot.margin=unit(c(-0.1, 0.1, 0.1, 0.1), "cm"))
p <- p + theme(legend.margin=unit(-0.2, "cm"))
p <- p + theme(legend.text=element_text(size=6))
p <- p + theme(axis.title.x=element_text(size=6), axis.text.x=element_text(size=6))
p <- p + scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), labels = percent)
p <- p + theme(axis.text.y=element_text(size=6))
# p <- p + xlab("Percent converted") + ylab("Marginal change in erosion potential per ha converted (T / ha / yr)")
p <- p + xlab("Percent converted") + ylab(expression(atop("Marginal change in eros. potential",
                                          paste("per ha converted (T / ha / yr)"))))
p <- p + theme(axis.title.y=element_text(size=6))
p <- p + facet_wrap(~ site, nrow=1, scales = "free") + theme(strip.background = element_blank(),
                                                             strip.text.x = element_blank())
fig3b <- p
print(fig3b)
jpgname <- paste(figdir, "Fig3_a-d_2x2.jpg", sep = "/")
# ggsave(filename=jpgname, plot=fig3b, width=3.5, height=3.5, units="in", dpi=600)

mylegend <- g_legend(fig3a)
jpgname <- paste(figdir, "Fig3_a-h.jpg", sep = "/")
jpeg(jpgname, width = 6, height=2.75, units = "in", res = 600)
grid.arrange(arrangeGrob(fig3a + theme(legend.position="none") + xlab(""),
                         fig3b + theme(legend.position="none"),
                         nrow=2),
                         mylegend, heights=c(10, 1))
dev.off()

# because fig 1a and 1b don't share x and y units, facet_grid won't work
# must place the plots together manually
# make fig 1a
fig1a_data <- all_data[which(!is.na(all_data$Total.cumulative.sediment.export) &
                     all_data$k == 2), ]

fig1a_data$sed_thousands <- fig1a_data$Total.cumulative.sediment.export / 1000
p <- ggplot(fig1a_data, aes(x = Area.converted..ha., y = sed_thousands, group = pattern))
if (color){
  p <- p + geom_line(aes(colour = pattern), size = 0.8) + print_theme
  p <- p + scale_colour_manual(values = cbPalette, name = "")
} else {
  p <- p + geom_line(aes(linetype = pattern), size = 0.8) + print_theme
  p <- p + scale_linetype_manual(values = lines, name = "")
}
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3,"line"))
p <- p + theme(legend.position="bottom")
# p <- p + scale_x_continuous(breaks = seq(10, 90, 10))
p <- p + xlab("Area converted (ha)") + ylab("Total sediment export (kT / yr)")
p <- p + theme(axis.text=element_text(size=9))
p <- p + theme(plot.margin=unit(c(0.1, 0.1, 0.13, 0.1), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm")) 
p <- p + annotate("text", label = "a", x = 0, y = 2.5, size = 5)
# p <- p + theme(axis.ticks = element_blank())
fig1a <- p

# make fig 1b
fig1b_data <- all_data[which(!is.na(all_data$change.in.sediment.per.area) &
                             !is.na(all_data$k)), ]

p <- ggplot(fig1b_data, aes(x = percent.converted, y = change.in.sediment.per.area, group = pattern))
if (color){
  p <- p + geom_line(aes(colour = pattern), size = 0.8)
  p <- p + scale_colour_manual(values = cbPalette, name = "")
} else {
  p <- p + geom_line(aes(linetype = pattern), size = 0.8)
  p <- p + scale_linetype_manual(values = lines, name = "")
}
p <- p + scale_y_continuous(breaks = seq(0, 125, 25))
p <- p + xlab("Percent converted")
p <- p + ylab("Marginal change in sediment export (T / ha / yr)")
#p <- p + ylab(expression(atop("Marginal change in sediment", paste("export (T / ha / yr)"))))
p <- p + scale_x_continuous(labels = percent)
p <- p + print_theme
p <- p + theme(axis.text=element_text(size=9), axis.title.y=element_text(size=7.2))
p <- p + theme(plot.margin=unit(c(0.1, 0.2, 0.13, 0), "cm"))
p <- p + theme(legend.margin=unit(-1,"cm")) 
p <- p + annotate("text", label = "b", x = 0.1, y = 140, size = 5)
# p <- p + theme(axis.ticks = element_blank())
fig1b <- p

# place figs 1a and 1b together
# get legend from one plot to serve as common legend for both
mylegend <- g_legend(fig1a)

# export to file
if(color){
  jpgname <- paste(figdir, "Fig1_color.jpg", sep = "/")
} else {
  jpgname <- paste(figdir, "Fig1.jpg", sep = "/")
}
jpeg(jpgname, width = 6, height=2.75, units = "in", res = 600)
grid.arrange(arrangeGrob(fig1a + theme(legend.position="none"),
                         fig1b + theme(legend.position="none"),
                         nrow=1),
             mylegend, heights=c(10, 1))
dev.off()

## make figs for supplemental material
supp1a_data <- all_data[which(!is.na(all_data$Total.cumulative.sediment.export) &
                                  !is.na(all_data$k)), ]
supp1a_data$sed_thousands <- supp1a_data$Total.cumulative.sediment.export / 1000

p <- ggplot(supp1a_data, aes(x = percent.converted, y = export.per.ha, group = interaction(k, pattern)))
p <- p + geom_line(aes(linetype = pattern, col = k), size = 0.8) + print_theme # size = lwd
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "K", guide = guide_legend(nrow = 2))
p <- p + xlab("Percent converted")
p <- p + ylab("Total sediment export (T / ha / yr)")
# p <- p + ylab(expression(atop("Total sediment export", paste("(kT / yr)"))))
p <- p + theme(plot.margin=unit(c(0.1, 0, 0.1, 0.05), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm"))
p <- p + scale_x_continuous(labels = percent, breaks = seq(0.2, 0.8, 0.2))
p <- p + annotate("text", label = "a", x = 0, y = 42, size = 4)
# p <- p + theme(axis.ticks = element_blank())
figsupp1a <- p

supp1b_data <- all_data[which(!is.na(all_data$Total.cumulative.sediment.export) &
                                !is.na(all_data$ic)), ]
supp1b_data$sed_thousands <- supp1b_data$Total.cumulative.sediment.export / 1000
p <- ggplot(supp1b_data, aes(x = percent.converted, y = export.per.ha, group = interaction(ic, pattern)))
p <- p + geom_line(aes(linetype = pattern, col = ic), size = 0.8) + print_theme
# p <- p + scale_size(range=c(0.1, 1.5), guide=FALSE)
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "IC0", guide = guide_legend(nrow = 2))
p <- p + xlab("Percent converted") + ylab("")
p <- p + theme(plot.margin=unit(c(0.1, 0, 0.1, -0.3), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm"))
p <- p + scale_x_continuous(labels = percent, breaks = seq(0.2, 0.8, 0.2))
p <- p + annotate("text", label = "b", x = 0, y = 35, size = 4)
# p <- p + theme(axis.ticks = element_blank())
figsupp1b <- p

supp1c_data <- all_data[which(!is.na(all_data$Total.cumulative.sediment.export) &
                                !is.na(all_data$slope)), ]
supp1c_data$sed_thousands <- supp1c_data$Total.cumulative.sediment.export / 1000
p <- ggplot(supp1c_data, aes(x = percent.converted, y = export.per.ha, group = interaction(slope, pattern)))
p <- p + geom_line(aes(linetype = pattern, col = slope), size = 0.8) + print_theme
# p <- p + scale_size(range=c(0.1, 1.5), guide=FALSE)
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "Slope", guide = guide_legend(nrow = 2))
p <- p + xlab("Percent converted") + ylab("")
p <- p + theme(plot.margin=unit(c(0.1, 0, 0.1, -0.3), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm"))
p <- p + scale_x_continuous(labels = percent, breaks = seq(0.2, 0.8, 0.2))
p <- p + annotate("text", label = "c", x = 0, y = 41, size = 4)
# p <- p + theme(axis.ticks = element_blank())
figsupp1c <- p

supp1d_data <- all_data[which(!is.na(all_data$Total.cumulative.sediment.export) &
                                !is.na(all_data$extent)), ]
supp1d_data$sed_thousands <- supp1d_data$Total.cumulative.sediment.export / 1000
p <- ggplot(supp1d_data, aes(x = percent.converted, y = export.per.ha, group = interaction(extent, pattern)))
p <- p + geom_line(aes(linetype = pattern, guide=FALSE, col = extent), guide = FALSE, size = 0.8) + print_theme
# p <- p + scale_size(range=c(0.1, 1.5), guide=FALSE)
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "Area", guide = guide_legend(nrow = 2))
p <- p + xlab("Percent converted") + ylab("")
p <- p + theme(plot.margin=unit(c(0.1, 0.2, 0.1, -0.3), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm"))
p <- p + scale_x_continuous(labels = percent, breaks = seq(0.2, 0.8, 0.2))
p <- p + annotate("text", label = "d", x = 0, y = 38.5, size = 4)
# p <- p + theme(axis.ticks = element_blank())
figsupp1d <- p

jpgname <- paste(figdir, "supp_fig1.jpg", sep = "/")
jpeg(file = jpgname, units="in", res=300, width = 9, height=3.5)
grid.arrange(arrangeGrob(figsupp1a, figsupp1b, figsupp1c, figsupp1d, nrow=1),
             mylegend, heights=c(10, 0.6))
dev.off()

supp2a_data <- all_data[which(!is.na(all_data$change.in.sediment.per.area) &
                                !is.na(all_data$slope)), ]
p <- ggplot(supp2a_data, aes(x = percent.converted, y = change.in.sediment.per.area, group = interaction(slope, pattern)))
p <- p + geom_line(aes(linetype = pattern, col = slope), size = 0.8) + print_theme
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "Slope")
p <- p + xlab("Percent converted") + ylab("Marginal change in sediment export (T / ha / yr)")
p <- p + scale_x_continuous(labels = percent)
p <- p + theme(plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm")) 
p <- p + annotate("text", label = "a", x = 0.1, y = 180, size = 5)
figsupp2a <- p
print(figsupp2a)

supp2b_data <- all_data[which(!is.na(all_data$change.in.sediment.per.area) &
                                !is.na(all_data$extent)), ]
p <- ggplot(supp2b_data, aes(x = percent.converted, y = change.in.sediment.per.area, group = interaction(extent, pattern)))
p <- p + geom_line(aes(linetype = pattern, col = extent), size = 0.8) + print_theme
p <- p + scale_linetype_manual(values = lines, name = "", guide = FALSE)
p <- p + scale_colour_manual(values = cbPalette, name = "Area")
p <- p + theme(legend.key = element_blank()) # , legend.title=element_blank())
p <- p + theme(legend.position="bottom")
p <- p + xlab("Percent converted") + ylab("")
p <- p + scale_x_continuous(labels = percent)
p <- p + theme(plot.margin=unit(c(0.1, 0.1, 0.1, -0.2), "cm"))
p <- p + theme(legend.margin=unit(-0.7,"cm")) 
p <- p + annotate("text", label = "b", x = 0.1, y = 150, size = 5)
figsupp2b <- p
print(figsupp2b)

jpgname <- paste(figdir, "supp_fig2.jpg", sep = "/")
jpeg(file = jpgname, units="in", res=300, width = 7, height=4)
grid.arrange(arrangeGrob(figsupp2a, figsupp2b, nrow=1), mylegend, heights=c(10, 0.65))
dev.off()

