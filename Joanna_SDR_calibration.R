### summarize sediment data from CCAMP available for calibration of SDR
# for Joanna Nelson's Salinas project
# 8.29.15

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(sm)
library(gdata)

datafile <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/OpenWater_qry_2015_08_13_10_28_55.csv'
data <- read.csv(datafile)
data$SampleDate <- as.Date(data$SampleDate, format="%Y-%m-%d %H:%M:%S")
data$SampleYear <- as.factor(format(data$SampleDate, "%Y"))
data$SiteTag <- as.factor(data$SiteTag)
data <- subset(data, QACode == "None")

# how many sites were sampled in each year?
num_years = length(levels(data$SampleYear))
site_counts = data.frame("year"=numeric(num_years), "sites"=numeric(num_years))
i <- 1
for (year in levels(data$SampleYear)){
  subset <- data[which(data$SampleYear == year), ]
  sites <- unique(subset$SiteTag)
  site_counts[i, "year"] <- year
  site_counts[i, "sites"] <- length(sites)
  i <- i + 1
}

# split data into time periods corresponding to Joanna's landcover
before <- data[which(data$SampleYear %in% c("2005", "2006", "2007")), ]
before$period <- 'before'
after <- data[which(data$SampleYear %in% c("2010", "2012")), ]
after$period <- 'after'

# was Result_val different between the two time periods?
mean(before$Result_val)
[1] 2521.946
mean(after$Result_val)
[1] 10677.68

combined <- rbind(before, after)
combined$period <- as.factor(combined$period)

# density plot: before and after time periods overlaid
p <- ggplot(combined, aes(x = Result_val)) + 
      geom_density(data=subset(combined, period == 'before'), fill = "red", alpha = 0.2) + 
      geom_density(data=subset(combined, period == 'after'), fill = "blue", alpha = 0.2)
p <- p + scale_x_continuous(limits=c(0, 25000))
print(p)

# how many sites are in common, and different, between those two time periods?
in_common <- intersect(before$SiteTag, after$SiteTag)
setdiff(before$SiteTag, after$SiteTag)

site_comparison = data.frame("site" = character(length(in_common)), 
                             "before" = numeric(length(in_common)),
                             "after" = numeric(length(in_common)),
                             "percent_change" = numeric(length(in_common)))
i <- 1
for (site in in_common){
  b_site <- subset(before, SiteTag == site)
  a_site <- subset(after, SiteTag == site)
  site_comparison[i, 'before'] <- mean(b_site$Result_val)
  site_comparison[i, 'after'] <- mean(a_site$Result_val)
  site_comparison[i, 'percent_change'] <- (mean(a_site$Result_val) - mean(b_site$Result_val)) / mean(b_site$Result_val)
  i <- i + 1
}
site_comparison$site <- in_common

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/Site_comparisons.csv"
write.csv(site_comparison, save_as)

subset <- data[which(data$SampleYear %in% c("2005", "2006", "2007", "2010", "2012")), ]
save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/CCAMP_2005-6-7-10-12.csv"
write.csv(subset, save_as)

########################
# How many sites were sampled >= 12 times across all time periods?
num_sites <- length(unique(data$SiteTag))
site_df <- data.frame("site" = character(num_sites),
                      "num_samples" = numeric(num_sites))
i <- 1
for (site in unique(data$SiteTag)){
  samples <- subset(data, SiteTag == site)
  samples <- samples[!duplicated(samples$SampleDate), ]
  num_samples <- dim(samples)[1]
  # site_df[i, "site"] <- site
  site_df[i, "num_samples"] <- num_samples
  i <- i + 1
}
site_df$site <- unique(data$SiteTag)
viable <- subset(site_df, num_samples > 11)
viable_space_matched <- viable[which(viable$site %in% match$site_tag), ]

########################
# Sites matched with SDR watersheds in GIS
match_sheet <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/matched_sites.csv'
match <- read.csv(match_sheet)

# time_subset <- data[which(data$SampleYear %in% c("2005", "2006", "2007", "2010", "2012")), ]
# 10.17.15: ignore time subset
time_subset <- data
space_time_subset <- time_subset[which(time_subset$SiteTag %in% match$site_tag), ]

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/CCAMP_SDR_matched_data.csv"
write.csv(space_time_subset, save_as)

################
# Nutrient data for NDR
n_datafile <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/OpenWater_qry_2015_09_05_09_03_25.csv"
n_data <- read.csv(n_datafile)
n_data$SampleDate <- as.Date(n_data$SampleDate, format="%Y-%m-%d %H:%M:%S")
n_data$SampleYear <- as.factor(format(n_data$SampleDate, "%Y"))
n_data$SiteTag <- as.factor(n_data$SiteTag)
n_data <- subset(n_data, QACode == "None")

matched_sites_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/NDR_matched_sites.csv"
site_match <- read.csv(matched_sites_file)
site_match$SiteTag <- as.factor(site_match$SiteTag)

matched_data <- subset(n_data, SiteTag %in% site_match$SiteTag)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/CCAMP_NDR_matched_data.csv"
write.csv(matched_data, save_as)

#########################
# Flow data
flow_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/flow_data.csv"
flow_data <- read.csv(flow_file)
flow_data$SampleDate <- as.Date(flow_data$SampleDate, format="%Y-%m-%d")
flow_data$SiteTag_c <- as.character(flow_data$SiteTag)

sdr_data <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/CCAMP_SDR_matched_data.csv")
ndr_data <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/CCAMP_NDR_matched_data.csv")

sdr_data$SampleDate <- as.Date(sdr_data$SampleDate, format="%Y-%m-%d")
# sdr_data$SiteTag <- as.factor(sdr_data$SiteTag)
ndr_data$SampleDate <- as.Date(ndr_data$SampleDate, format="%Y-%m-%d")
# ndr_data$SiteTag <- as.factor(ndr_data$SiteTag)

data <- sdr_data
data$flow_cfs <- NA
for (row in 1:(dim(data)[1])){
  sample <- data[row, ]
  site <- as.character(sample$SiteTag)
  date <- sample$SampleDate
  match <- flow_data[which(flow_data$SiteTag_c == site & flow_data$SampleDate == date), ]
  if (dim(match)[1] == 1){
    data[row, 'flow_cfs'] <- match$Result_val
  }
  else if (sd(match$Result_val) == 0){
    data[row, 'flow_cfs'] <- match[1, "Result_val"]
  }
}
not_matched <- data[which(is.na(data$flow_cfs)), ]

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/CCAMP_NDR_matched_data_flow.csv"
write.csv(data, save_as)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/CCAMP_SDR_matched_data_flow.csv"
write.csv(data, save_as)

###############################
# write fixed width files for input to LOADEST
write_loadest_file <- function(data, save_dir, calib=TRUE, suffix=NULL){
  for (site in unique(data$SiteTag)){
    if(calib){  # write calibration file
      sset <- data[which(data$SiteTag == site), c('SampleDate', 'flow_cfs', 'Result_val')]
    }
    else {  # write estimation file (almost exact same information)
      sset <- data[which(data$SiteTag == site), c('SampleDate', 'flow_cfs')]
    }
    sset <- sset[which(sset$flow_cfs >= 0.00001), ]
    sset <- sset[!duplicated(sset$SampleDate), ]
    sset$time <- 1200
    sset$SampleDate_n <- format(sset$SampleDate, format="%Y%m%d")
    if(calib){
      to_write <- sset[, c('SampleDate_n', 'time', 'flow_cfs', 'Result_val')]
      to_write$Result_val <- round(to_write$Result_val, 5)
      width = c(8, 10, 10, 10)
    }
    else{
      to_write <- sset[, c('SampleDate_n', 'time', 'flow_cfs')]
      width = c(8, 10, 10)
    }
    to_write$flow_cfs <- round(to_write$flow_cfs, 5)
    if (is.null(suffix)){
      save_as <- paste(save_dir, paste(site, '.inp', sep=""), sep="/")
    }
    else{
      save_as <- paste(save_dir, paste(site, '_', suffix, '.inp', sep=""), sep="/")
    }
    write.fwf(to_write, save_as, justify="left", colnames=FALSE, width=width)
  }
}

ndr_data <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/CCAMP_NDR_matched_data_flow.csv")
ndr_data$SampleDate <- as.Date(ndr_data$SampleDate, format="%Y-%m-%d")
before <- ndr_data[which(ndr_data$SampleYear < 2008), ]
before$SiteTag <- as.factor(before$SiteTag)
after <- ndr_data[which(ndr_data$SampleYear > 2008), ]
data <- ndr_data
save_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/loadest_calib_files"
write_loadest_file(data, calib=TRUE, save_dir)
save_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/NDR_calibration/Data/loadest_est_files"
write_loadest_file(data, calib=FALSE, save_dir)

sdr_data <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/CCAMP_SDR_matched_data_flow.csv")
sdr_data$SampleDate <- as.Date(sdr_data$SampleDate, format="%Y-%m-%d")
sdr_data$SiteTag <- as.factor(sdr_data$SiteTag)
# before <- sdr_data[which(sdr_data$SampleYear < 2008), ]
# before$SiteTag <- as.factor(before$SiteTag)
# after <- sdr_data[which(sdr_data$SampleYear > 2008), ]
save_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/loadest_calib_files"
write_loadest_file(sdr_data, save_dir, calib=TRUE)
# write_loadest_file(before, save_dir, calib=TRUE, suffix='before')
# write_loadest_file(after, save_dir, calib=TRUE, suffix='after')
save_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Joanna/SDR_calibration/CCAMP_data/loadest_est_files"
write_loadest_file(sdr_data, save_dir, calib=FALSE)
# write_loadest_file(before, save_dir, calib=FALSE, suffix='before')
# write_loadest_file(after, save_dir, calib=FALSE, suffix='after')

#####################################
# summarize fit of calibrated runs
summary_table <- read.csv("C:/Users/Ginger/Desktop/Joanna_SDR_results.csv")
runs <- c('run_1_11.12', 'run_0_11.12', 'run_2_11.12')  # TODO get runs through grep
result_df <- data.frame(run=character(length(runs)), ssq=numeric(length(runs)),
                        stringsAsFactors=FALSE)
i <- 1
for (run in runs){
  ssq <- 0
  for(row in 1:(dim(summary_table)[1] - 1)){
    diff <- summary_table[row, 'Empirical'] - summary_table[row, run]
    diff_sq <- diff^2
    ssq <- ssq + diff_sq
  }
  result_df[i, 'run'] <- run
  result_df[i, 'ssq'] <- ssq
  i <- i + 1
}

p <- ggplot(summary_table, aes(x=Empirical, y=run_1_11.12))
p <- p + geom_point() + ylab("Modeled sediment export") + xlab("Empirical sediment export")
print(p)
imgpath <- "F:/From_NatCap_backup/Joanna/SDR_calibration"
pngname <- paste(imgpath, "Modeled_vs_empirical_SDR_k_1_no_restr.png", sep="/")
png(file=pngname, units="in", res=300, width=4, height=4)
print(p)
dev.off()

p <- ggplot(summary_table, aes(x=Empirical, y=run_1))
p <- p + geom_point()
print(p)
p <- ggplot(summary_table, aes(x=Empirical, y=run_2))
p <- p + geom_point()
print(p)

