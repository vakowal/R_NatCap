# Plot output from livestock forage model #

library(ggplot2)
library(grid)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

results_dir <- "C:/Users/Ginger/Documents/Python/Output"
results_folder <- "Forage_Model_140728-1412"
suffix <- gsub("Forage_Model_", "", results_folder)
imgpath <- paste(results_dir, results_folder, "/", sep = "/")

summary_file <- read.csv(paste(results_dir, results_folder, "summary.csv", sep = "/"), header = TRUE)
steps <- max(summary_file[, "Step"])

veg_plot <- ggplot(summary_file, aes(x = Step, y = Standing_veg)) + geom_line() + ylab("Standing vegetation (kg dry matter)") # + print_theme
livestock_plot <- ggplot(summary_file, aes(x = Step, y = Herd_avg_weight)) + geom_line() + ylab("Average herbivore weight (kg)") # + print_theme

pngname <- paste(imgpath, "Standing_veg.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=9)
print(veg_plot)
dev.off()

pngname <- paste(imgpath, "Herd_average_weight.png", sep="")
png(file=pngname, units="in", res=300, width=7.5, height=9)
print(livestock_plot)
dev.off()
