# plot frontiers from optimizer

library(ggplot2)
library(scatterplot3d)

print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

outer_dir <- "C:/Users/Ginger/Dropbox/Optimization/CGIAR-WLE/Peru_optimization_results_11.8.16"
imgdir <- paste(outer_dir, "frontier_figs", sep="/")
suf_list <- list.files(outer_dir)
suf_list <- suf_list[-c(1, 5, 6)]
for(f in suf_list){
  # sum_df <- read.csv(paste(outer_dir, f, "score_summary.csv", sep="/"))
  f <- 'animal_weights_survey_default'
  imgdir <- 'C:/Users/Ginger/Desktop'
  sum_df <- read.csv("C:/Users/Ginger/Downloads/animal_weights_survey_default_beta_scores_summary.csv")
  
  p <- ggplot(sum_df, aes(x=sdr_score, y=swy_score))
  p <- p + geom_point()
  p <- p + xlab("SDR score (tons sediment per year)") + ylab("SWY score (mm?)")
  p <- p + print_theme
  pngname <- paste(imgdir, paste("sdr_x_swy_", f, ".png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=4, height=4)
  print(p)
  dev.off()
  
  p <- ggplot(sum_df, aes(x=livestock_score, y=swy_score))
  p <- p + geom_point()
  p <- p + xlab("Livestock score (average yearly kg gained)") + ylab("SWY score (mm?)")
  p <- p + print_theme
  pngname <- paste(imgdir, paste("livestock_x_swy_", f, ".png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=4, height=4)
  print(p)
  dev.off()
  
  p <- ggplot(sum_df, aes(x=livestock_score,  y=sdr_score))
  p <- p + geom_point()
  p <- p + xlab("Livestock score (average yearly kg gained)") + ylab("SDR score (tons sediment per year)")
  p <- p + print_theme
  p <- p + scale_y_reverse()
  pngname <- paste(imgdir, paste("livestock_x_sdr_", f, ".png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=4, height=4)
  print(p)
  dev.off()
}

sum_df$water_obj_score <- sum_df$sdr_score * sum_df$sdr_weight + sum
p <- ggplot(sum_df, aes(x=livestock_score, y=swy_score, group=sdr_score))
p <- p + geom_point(aes(colour=sdr_score))
p <- p + xlab("Livestock score (average yearly kg gained)") + ylab("SDR score (tons sediment per year)")
p <- p + print_theme
print(p)

test3d <- scatterplot3d(sum_df$livestock_score, sum_df$swy_score, sum_df$sdr_score)
print(test3d)

test3d <- scatterplot3d(sum_df$sdr_score, sum_df$swy_score, sum_df$livestock_score )
print(test3d)

test1 <- cor.test(sum_df$livestock_score, sum_df$sdr_score)
test2 <- cor.test(sum_df$livestock_score, sum_df$swy_score)
test3 <- cor.test(sum_df$swy_score, sum_df$sdr_score)
