## plot Unilever results for Becky's MS
## Ginger Kowal 2.12.15

## some lines that are commented out are aesthetic options I played with
## but decided against, you may wish to re-implement

library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

# I stole this from stackoverflow: get legend from one plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# general ggplot theme for print figs
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Unilever/Manuscript materials' 
figdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Unilever/Manuscript materials/figs'

all_data <- read.table(paste(datadir, 'fig_data.txt', sep = "/"), header = TRUE, sep="\t")
all_data$actual_baseline <- factor(all_data$actual.baseline, levels = c("baseline", "actual"))
all_data$site <- factor(all_data$site, levels = c("Iowa", "Heilongjiang", "Jiangxi", "Mato Grosso", "synthetic")) 
all_data$pattern <- factor(all_data$pattern, levels = c("To stream", "From stream", "Buffer", "From cropland"))
# these line types will map to a factor level in the defined order
lines <- c("longdash", "dotted", "solid", "twodash")

# make fig 2 first
fig2_data <- all_data[(all_data$percent.converted >= 0.1 & 
                  all_data$percent.converted <= 0.95 &
                  all_data$site %in% c("Iowa", "Heilongjiang", "Jiangxi", "Mato Grosso")), ]

# build the ggplot figure
p <- ggplot(fig2_data, aes(x = percent.converted, y = export, group = pattern))
p <- p + geom_line(aes(linetype = pattern) , size = 0.8) + print_theme
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3.7, "line"))
p <- p + theme(legend.position="bottom")
p <- p + scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), labels = percent)

# this is the part that builds the grid
p <- p + facet_grid(site ~ actual_baseline, scales = "free")

# map line types defined above to factor levels
p <- p + scale_linetype_manual(values = lines, name = "", guide = guide_legend(nrow = 2))
p <- p + xlab("Percent converted") + ylab("Rate of sediment export (T / ha)")
# p <- p + theme(axis.ticks = element_blank())
fig2 <- p

# take a look
print(fig2)

# export the figure to file
pngname <- paste(figdir, "Fig2.png", sep = "/")
png(file = pngname, units="in", res=300, width = 5, height=9)
print(fig2)
dev.off()

# because fig 1a and 1b don't share x and y units, facet_grid won't work
# must place the plots together manually
# make fig 1a
fig1a_data <- all_data[(!is.na(all_data$Total.cumulative.sediment.export) &
                     all_data$k == 2), ]

p <- ggplot(fig1a_data, aes(x = Area.converted..ha., y = Total.cumulative.sediment.export, group = pattern))
p <- p + geom_line(aes(linetype = pattern) , size = 0.8) + print_theme
p <- p + theme(legend.key = element_blank(), legend.title=element_blank())
p <- p + theme(legend.key.width=unit(3,"line"))
p <- p + theme(legend.position="bottom")
# p <- p + scale_x_continuous(breaks = seq(10, 90, 10))
p <- p + scale_linetype_manual(values = lines, name = "")
p <- p + xlab("Area converted (ha)") + ylab("Total cumulative sediment export (T)")
p <- p + theme(plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p <- p + annotate("text", label = "a", x = 0, y = 2500, size = 5)
# p <- p + theme(axis.ticks = element_blank())
fig1a <- p
print(fig1a)

# make fig 1b
fig1b_data <- all_data[!is.na(all_data$change.in.sediment.per.area), ]

p <- ggplot(fig1b_data, aes(x = percent.converted, y = change.in.sediment.per.area, group = pattern))
p <- p + geom_line(aes(linetype = pattern) , size = 0.8) + print_theme
p <- p + scale_linetype_manual(values = lines, name = "")
p <- p + scale_y_continuous(breaks = seq(0, 125, 25))
p <- p + xlab("Percent converted") + ylab("Change in sediment per area (T / ha)")
p <- p + scale_x_continuous(labels = percent)
p <- p + theme(plot.margin=unit(c(0.1, 0.2, 0.1, 0.1), "cm"))
p <- p + annotate("text", label = "b", x = 0, y = 150, size = 5)
# p <- p + theme(axis.ticks = element_blank())
fig1b <- p
print(fig1b)

# place figs 1a and 1b together
# get legend from one plot to serve as common legend for both
mylegend <- g_legend(fig1a)
fig1 <- grid.arrange(arrangeGrob(fig1a + theme(legend.position="none"),
                                 fig1b + theme(legend.position="none"),
                                 nrow=1),
                     mylegend, heights=c(10, 1))
# take a look
print(fig1)

# export to file
pngname <- paste(figdir, "Fig1.png", sep = "/")
png(file = pngname, units="in", res=300, width = 6, height=3)
grid.arrange(arrangeGrob(fig1a + theme(legend.position="none"),
                         fig1b + theme(legend.position="none"),
                         nrow=1),
             mylegend, heights=c(10, 1))
dev.off()