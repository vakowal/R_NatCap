# explore empirical cattle weight gain data from Suyian ranch

library(ggplot2)

data_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Suyian_ranch_steer_weights/combined_weights.csv"
data <- read.csv(data_csv)

steers <- data[which(data$status == 'steer'), ]
data <- steers

dates <- colnames(data)[5:16]
num_meas <- length(dates)
list1 <- list()
for(id in unique(data$animal_id)){
  df <- data.frame('id'=rep(id, num_meas),
                   'kg'=t(subset(data, animal_id == id, select=dates)),
                   'date'=dates)
  colnames(df) <- c('id', 'kg', 'date')
  df <- df[!is.na(df$kg), ]
  list1[[id]] <- df
}
weights_by_id <- do.call(rbind, list1)
weights_by_id$date <- as.Date(weights_by_id$date,  format="X%m.%d.%Y")

p <- ggplot(weights_by_id, aes(x=date, y=kg, group=id))
p <- p + geom_line(group=id)
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Suyian_ranch_steer_weights/steer_weights_by_id.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

w_only <- data[, c(5:16)]
w_means <- colMeans(w_only, na.rm=TRUE)
w_min <- sapply(w_only, min, na.rm=TRUE)
w_max <- sapply(w_only, max, na.rm=TRUE)
q25 <- sapply(w_only, quantile, probs=c(0.25), 
              na.rm=TRUE, names=FALSE)
q75 <- sapply(w_only, quantile, probs=c(0.75), 
              na.rm=TRUE, names=FALSE)
w_mean_df <- data.frame('id'=rep(0, length(w_means)),
                        'kg'=w_means,
                        'date'=dates,
                        'min'=w_min,
                        'max'=w_max,
                        'q25'=q25,
                        'q75'=q75)
w_mean_df$date <- as.Date(w_mean_df$date,  format="X%m.%d.%Y")

p <- ggplot(w_mean_df, aes(x=date, y=kg))
p <- p + geom_line()
p <- p + geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.2)
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Suyian_ranch_steer_weights/steer_mean_25_75_perc.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()
