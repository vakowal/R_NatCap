### plot synthetic landscapes results
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

datadir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Unilever/Synthetic_landscapes/InVEST_workspace/SDR/"

broken <- read.table(paste(datadir, 'synthetic_landscapes_summary_broken.txt', sep = ""), header = TRUE)
broken$conversion_type <- as.factor(broken$conversion_type)
broken$DEM <- factor(broken$DEM, levels = c('constant', 'broken'))

p <- ggplot(broken, aes(x = X._conversion, y = sed_export, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ DEM) + print_theme
p <- p + ggtitle("Slope: constant or 'broken'")
print(p)

doubled <- read.table(paste(datadir, 'synthetic_landscapes_summary_doubled.txt', sep = ""), header = TRUE)
doubled$conversion_type <- as.factor(doubled$conversion_type)
doubled$n_col <- as.factor(doubled$n_col)

p <- ggplot(doubled, aes(x = X._conversion, y = sed_export, group = conversion_type, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ n_col) + print_theme
p <- p + ggtitle("Number of columns")
print(p)

IC0 <- read.table(paste(datadir, 'synthetic_landscapes_summary_IC0.txt', sep = ""), header = TRUE)
IC0$conversion_type <- as.factor(IC0$conversion_type)
IC0$IC0 <- as.factor(IC0$IC0)

p <- ggplot(IC0, aes(x = X._conversion, y = sed_export, group = conversion_type, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ IC0) + print_theme
p <- p + ggtitle("IC0")
print(p)

kb <- read.table(paste(datadir, 'synthetic_landscapes_summary_kb_12.11.14.txt', sep = ""), header = TRUE)
kb$conversion_type <- as.factor(kb$conversion_type)
kb$kb <- as.factor(kb$k)

p <- ggplot(kb, aes(x = X._conversion, y = sed_export, group = conversion_type, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ kb) + print_theme
p <- p + ggtitle("Kb: latest model release (12.11.14)")
print(p)

datadir <- 'C:/Users/Ginger/Documents/NatCap/GIS_local/Unilever/synthetic_landscapes/latest_release_12.11.14/'

broken <- read.table(paste(datadir, 'synthetic_landscapes_summary_broken_12.11.14.txt', sep = ""), header = TRUE)
broken$conversion_type <- as.factor(broken$conversion_type)
broken$DEM <- factor(broken$dem, levels = c('constant', 'broken'))

p <- ggplot(broken, aes(x = X._conversion, y = sed_export, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ DEM) + print_theme
p <- p + ggtitle("Slope: constant or 'broken', 12.11.14 release")
print(p)

doubled <- read.table(paste(datadir, 'synthetic_landscapes_summary_doubled_12.11.14.txt', sep = ""), header = TRUE)
doubled$conversion_type <- as.factor(doubled$conversion_type)
doubled$n_col <- as.factor(doubled$n_col)

p <- ggplot(doubled, aes(x = X._conversion, y = sed_export, group = conversion_type, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ n_col) + print_theme
p <- p + ggtitle("Number of columns, 12.11.14 release")
print(p)

IC0 <- read.table(paste(datadir, 'synthetic_landscapes_summary_ic0_12.11.14.txt', sep = ""), header = TRUE)
IC0$conversion_type <- as.factor(IC0$conversion_type)
IC0$IC0 <- as.factor(IC0$ic0)

p <- ggplot(IC0, aes(x = X._conversion, y = sed_export, group = conversion_type, linetype = conversion_type)) 
p <- p + geom_point() + geom_line()
p <- p + facet_wrap(~ IC0) + print_theme
p <- p + ggtitle("IC0, 12.11.14 release")
print(p)
