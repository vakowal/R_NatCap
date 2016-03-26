# livestock stocking density info from Peru household survey

library(ggplot2)

outdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/stocking_density_histograms"
data_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/stocking_density_table.csv"
data <- read.csv(data_file)
data[data == 0] <- NA

# make boxplots of each animal density
outdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/density_by_district"
for (column in c('cattle_per_ha', 'camelids_per_ha', 'sheep_per_ha', 'total_animals_per_ha')){
  p <- ggplot(data, aes(x=factor(district)))
  p <- p + aes_q(y=as.name(column))
  p <- p + geom_boxplot() + xlab('district')
  pngname <- paste(outdir, '/', column, ".png", sep = "")
  png(file=pngname, units="in", res=300, width=7, height=4)
  print(p)
  dev.off()
}

# make hist of each density col
for (column in colnames(data)){
  pngname <- paste(outdir, '/', column, ".png", sep = "")
  png(file=pngname, units="in", res=300, width=4, height=3)
  hist(data[, column], breaks=50, main=column, xlab=column)
  dev.off()
}

# calculate means of distribution
for (column in colnames(data)[11:13]){
  print(column)
  dat <- data[, column]
  quan_l <- quantile(dat, probs=c(0, 0.33, 0.67, 1), na.rm=TRUE)
  print(quan_l)
  level1 <- dat[dat > quan_l[1] & dat < quan_l[2] & !is.na(dat)]
  level1_m <- mean(level1)
  level2 <- dat[dat > quan_l[2] & dat < quan_l[3] & !is.na(dat)]
  level2_m <- mean(level2)
  level3 <- dat[dat > quan_l[3] & dat < quan_l[4] & !is.na(dat)]
  level3_m <- mean(level3)
  means <- c(level1_m, level2_m, level3_m)
  print(means)
}