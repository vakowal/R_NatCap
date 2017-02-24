# various summary plots: vegetation at Ol Pejeta Conservancy
library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# pin frame data
pin_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_pinframe.csv")
pin_df <- pin_df[, (1:59)]

pin_df$trans_pos <- paste(pin_df$Date, pin_df$Site, pin_df$Position_m, sep="_")
pin_pos_sum <- aggregate(pin_df[, 6:59], by=list(pin_df$trans_pos), FUN=sum)
pin_pos_sum <- data.frame(pin_pos_sum,
                          do.call(rbind, strsplit(as.character(pin_pos_sum$Group.1),'_')))
pin_pos_sum$transect <- paste(pin_pos_sum$X1, pin_pos_sum$X2, sep="_")

pin_mean <- aggregate(pin_pos_sum[, 2:55], by=list(pin_pos_sum$transect), FUN=mean)
colnames(pin_mean)[1] <- 'transect'

find_green <- function(val){
  letters <- strsplit(val, split="")
  if(length(letters[[1]]) > 3){
    test_letter <- letters[[1]][4]
  }
  else{
    test_letter <- letters[[1]][3]
  }
  if(test_letter == 'G'){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
green_col_idx <- sapply(colnames(pin_mean), find_green, USE.NAMES=FALSE)
brown_cols <- pin_mean[, (!green_col_idx)]
green_brown_summary <- data.frame(pin_mean[, 1])
colnames(green_brown_summary) <- 'transect'
green_brown_summary$green_sum <- rowSums(pin_mean[, (green_col_idx)])
green_brown_summary$brown_sum <- rowSums(brown_cols[, -1])
green_brown_summary$perc_green <- green_brown_summary$green_sum / (green_brown_summary$green_sum + green_brown_summary$brown_sum)
write.csv(green_brown_summary, "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/pinframe_green_brown_summary.csv",
          row.names=FALSE)

# compare with all groups' dung
green_brown_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/pinframe_green_brown_summary.csv",
                                stringsAsFactors=FALSE)
dung_sum <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/dung_summary.csv",
                     stringsAsFactors=FALSE)
dung_sum$bovid <- dung_sum$Buf + dung_sum$Cow

group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)
gr_subs <- gr_key_df[, c('Abbrev', 'Group1', 'Group5', 'Group3', 'Group6')]
means_t <- as.data.frame(t(dung_sum[, c(2:24, 29)]))
means_t$Abbrev <- rownames(means_t)

comb <- merge(means_t, gr_subs, by='Abbrev')
gr1_means <- aggregate(comb[, 2:287], by=list(comb$Group1), FUN=sum)
colnames(gr1_means) <- c('group', dung_sum$transect)
tr1 <- as.data.frame(t(gr1_means[, 2:287]))
colnames(tr1) <- gr1_means$group
tr1$transect <- rownames(tr1)

gr5_means <- aggregate(comb[, 2:287], by=list(comb$Group5), FUN=sum)
colnames(gr5_means) <- c('group', dung_sum$transect)
tr5 <- as.data.frame(t(gr5_means[, 2:287]))
colnames(tr5) <- gr5_means$group
tr5$transect <- rownames(tr5)

gr3_means <- aggregate(comb[, 2:287], by=list(comb$Group3), FUN=sum)
colnames(gr3_means) <- c('group', dung_sum$transect)
tr3 <- as.data.frame(t(gr3_means[, 2:287]))
colnames(tr3) <- gr3_means$group
tr3$transect <- rownames(tr3)

gr6_means <- aggregate(comb[, 2:287], by=list(comb$Group6), FUN=sum)
colnames(gr6_means) <- c('group', dung_sum$transect)
tr6 <- as.data.frame(t(gr6_means[, 2:287]))
colnames(tr6) <- gr6_means$group
tr6$transect <- rownames(tr6)

grouped <- merge(tr1, tr3)
grouped <- merge(grouped, tr5)
grouped <- merge(grouped, tr6)

dung_gr_br <- merge(grouped, green_brown_summary, by='transect')
for (r in (1:NROW(dung_gr_br))){
  date <- unlist(strsplit(as.character(dung_gr_br[r, 'transect']), split="_"))[1]
  date <- as.Date(date, format="%d-%b-%y")
  dung_gr_br[r, 'date'] <- date
  dung_gr_br[r, 'Year'] <- format(date, "%Y")
  dung_gr_br[r, 'month'] <- format(date, "%m")
  dung_gr_br[r, 'year_month'] <- format(date, "%Y_%m")
}

# month as covariate
subs <- dung_gr_br[, c('bovid', 'grazer_ex_bovid', 'grazer_inc_bovid',
                       'green_sum', 'perc_green', 'transect', 'year_month')]
summary_list <- list()
for(gr in c('bovid', 'grazer_ex_bovid', 'grazer_inc_bovid')){
  for(rs in c('green_sum', 'perc_green')){
    df_copy <- subs
    df_copy$dung <- df_copy[, gr]
    df_copy$response <- df_copy[, rs]
    fit <- lm(response ~ dung*year_month, data=df_copy)
    label <- paste(gr, rs, sep="-")
    summary_list[[label]] <- summary(fit)
  }
}

# barplot of average precip on OPC, for empirical sampling months
precip <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/2014_2015_OPC_precip.csv")
precip$date <- as.Date(paste(precip$Year, precip$month, '01', sep="-"),
                       format='%Y-%m-%d')
ave_precip <- aggregate(precip_cm~date, data=precip, FUN=mean)
ave_precip$year_month <- format(ave_precip$date, '%Y_%m')
ave_precip$month <- format(ave_precip$date, "%m")
emp_months <- unique(dung_gr_br$year_month)
precip_res <- ave_precip[which(ave_precip$year_month %in% emp_months), ]
labs = precip_res$month
p <- ggplot(precip_res, aes(x=year_month, y=precip_cm))
p <- p + geom_bar(stat='identity')
p <- p + scale_x_discrete(labels=labs)
p <- p + xlab("Month") + ylab("Precipitation (cm)") + print_theme
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/OPC_avg_precip_sampling_mos.png"
png(file=pngname, units="in", res=300, width=3, height=2.5)
print(p)
dev.off()

# average precip on OPC for simulated months (Nov 2014 - Dec 2015)
sim_months <- c("2014_11", "2014_12", "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06",
                "2015_07", "2015_08", "2015_09", "2015_10", "2015_11", "2015_12")
precip_res <- ave_precip[which(ave_precip$year_month %in% sim_months), ]
labs = precip_res$month
p <- ggplot(precip_res, aes(x=year_month, y=precip_cm))
p <- p + geom_bar(stat='identity')
p <- p + scale_x_discrete(labels=labs)
p <- p + xlab("Month") + ylab("Precipitation (cm)") + print_theme
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/OPC_avg_precip_sim_mos.png"
png(file=pngname, units="in", res=300, width=2.87, height=2.37)
print(p)
dev.off()

# one regression per month
subs <- dung_gr_br[, c('bovid', 'grazer_ex_bovid', 'grazer_inc_bovid',
                       'green_sum', 'perc_green', 'transect', 'year_month')]

sum_df <- data.frame('animal_group'=c(), 'response'=c(),
                     'year_month'=c(), 'dung_estimate'=c(),
                     'dung_p_val'=c(), 'num_transects'=c())
i <- 1
for(gr in c('bovid', 'grazer_ex_bovid', 'grazer_inc_bovid')){
  for(rs in c('green_sum', 'perc_green')){
    for(year_month in unique(subs$year_month)){
      df_copy <- subs[which(subs$year_month == year_month), ]
      df_copy$dung <- df_copy[, gr]
      df_copy$response <- df_copy[, rs]
      fit <- lm(response ~ dung, data=df_copy)
      sum_df[i, 'animal_group'] <- gr
      sum_df[i, 'response'] <- rs
      sum_df[i, 'year_month'] <- year_month
      sum_df[i, 'dung_estimate'] <- coef(summary(fit))['dung', 'Estimate']
      sum_df[i, 'dung_p_val'] <- coef(summary(fit))['dung', 'Pr(>|t|)']
      sum_df[i, 'num_transects'] <- dim(df_copy)[1]
      i <- i + 1
    }
  }
}

write.csv(sum_df, file="C:/Users/Ginger/Desktop/sum_df.csv", row.names=FALSE)

for (r in (1:NROW(sum_df))){
  month <- unlist(strsplit((sum_df[r, 'year_month']), split="_"))[2]
  sum_df[r, 'month'] <- month
}
sum_df <- sum_df[order(sum_df$animal_group, sum_df$response,
                       sum_df$year_month), ]
labs <- sum_df$month
sum_df$sig <- "NS"
sum_df[which(sum_df$dung_p_val <= 0.05), "sig"] <- "significant"


sum_df$animal_group <- factor(sum_df$animal_group,
                              levels=c('bovid', 'grazer_ex_bovid'),
                              labels=c('Bovid', 'Grazer (ex. bovid)'))
sum_df$response <- factor(sum_df$response,
                          levels=c('perc_green'), labels=c('% green hits'))
sum_df <- sum_df[which(sum_df$response == '% green hits'), ]
sum_df <- sum_df[which(sum_df$animal_group %in% c('Bovid', 'Grazer (ex. bovid)')), ]

p <- ggplot(sum_df, aes(x=year_month, y=dung_estimate, group=sig))
p <- p + geom_point(aes(shape=sig))
p <- p + scale_shape_manual(values=c(1, 19))
p <- p + facet_grid(response ~ animal_group, scales="free")
p <- p + print_theme
p <- p + scale_x_discrete(labels=labs)
p <- p + xlab("Month") + ylab("Dung: estimate")
p <- p + theme(legend.position="none")
print(p)

pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/perc_green~dung_by_month_lm_estimate.png"
png(file=pngname, units="in", res=300, width=4, height=2)
print(p)
dev.off()

# how many transects per month of sampling?
transect_count <- aggregate(dung_gr_br$transect, by=list(dung_gr_br$year_month),
                            FUN=count)
write.csv(transect_count, file="C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_transect_count_by_month.csv",
          row.names=FALSE)

# precip as covariate
precip <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/2014_2015_OPC_precip.csv")
precip$date <- as.Date(paste(precip$Year, precip$month, '01', sep="-"),
                       format='%Y-%m-%d')
ave_precip <- aggregate(precip_cm~date, data=precip, FUN=mean)
ave_precip$site <- 'site_average'
precip_df <- precip[, c('date', 'precip_cm', 'site')]
precip_df <- rbind(precip_df, ave_precip)

p <- ggplot(precip_df, aes(x=date, y=precip_cm, group=site))
p <- p + geom_line(aes(colour=site))
print(p)

ave_precip$Year <- format(ave_precip$date, '%Y')
ave_precip$month <- format(ave_precip$date, '%m')
ave_precip$year_month <- format(ave_precip$date, '%Y_%m')

ave_precip$date_f <- factor(ave_precip$date)
p <- ggplot(ave_precip, aes(x=month, y=precip_cm))
p <- p + geom_bar(stat='identity')
p <- p + xlab("Month") + ylab("Precipitation (cm)")
p <- p + facet_wrap(~Year, ncol=1) + print_theme
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/OPC_avg_precip_2014_2015.png"
png(file=pngname, units="in", res=300, width=5, height=6)
print(p)
dev.off()

ave_precip <- aggregate(precip_cm ~ Year + month, data=precip, FUN=mean)
ave_precip$month <- as.numeric(ave_precip$month)
ave_precip$month <- sprintf("%02d", ave_precip$month)
pdm_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_PDM_summary.csv")
dung_gr_br <- merge(dung_gr_br, ave_precip, by=c('Year', 'month'), all.x=TRUE)
dung_gr_br <- merge(dung_gr_br, pdm_summary)

dung_gr_br$year_month <- factor(dung_gr_br$year_month)

perc_g_fit <- lm(perc_green ~ all_dung, data=dung_gr_br)
green_sum_fit <- lm(green_sum ~ all_dung, data=dung_gr_br)

AIC(perc_g_fit)
AIC(green_sum_fit)

summary(perc_g_fit)
summary(green_sum_fit)

biomass_fit <- lm(biomass_kgha ~ precip_cm, data=dung_gr_br)
AIC(biomass_fit)
summary(biomass_fit)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/dung_v_perc_green_figs"
for(group in c('bovid', 'grazer_ex_bovid')){ # unique(gr_key_df$Group6)){
  qs <- quantile(dung_gr_br[, group], probs=c(0.25, 0.75))
  dung_gr_br$dung_level <- NA
  dung_gr_br[which(dung_gr_br[, group] <= qs[1]), 'dung_level'] <- 'lower_25%'
  dung_gr_br[which(dung_gr_br[, group] >= qs[2]), 'dung_level'] <- 'upper_25%'
  subs <- dung_gr_br[which(!is.na(dung_gr_br$dung_level)), ]
  p <- ggplot(subs, aes(x=dung_level, y=perc_green))
  p <- p + geom_boxplot()
  p <- p + facet_wrap(~year_month)
  p <- p + ggtitle(group)
  pngname <- paste(img_dir, paste(group, "_x_perc_green_25perc.png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=5, height=6)
  print(p)
  dev.off()
}


# compare with bovid dung
green_brown_summary <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/pinframe_green_brown_summary.csv",
                                stringsAsFactors=FALSE)
# restricted to 2 km of weather stations
dung_sum <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/bovid_dung_weather_2km.csv",
                     stringsAsFactors=FALSE)
dung_gr_br <- merge(dung_sum, green_brown_summary, by='transect')
dung_gr_br$Date <- as.Date(dung_gr_br$Date, format="%Y-%m-%d")
dung_gr_br$Year <- format(dung_gr_br$Date, "%Y")
dung_gr_br$month <- format(dung_gr_br$Date, "%m")
dung_gr_br[which(dung_gr_br$site == ' Serat gate'), 'site'] <- 'Serat'
dung_gr_br[which(dung_gr_br$site == 'Golf 7'), 'site'] <- 'Golf_7'
dung_gr_br[which(dung_gr_br$site == 'Loirugurugu'), 'site'] <- 'Loirugu'
dung_gr_br[which(dung_gr_br$site == 'Rongai gate'), 'site'] <- 'Rongai'
dung_gr_br[which(dung_gr_br$site == 'Simira'), 'site'] <- 'Sirima'

precip <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/2014_2015_precip.csv")
precip$month <- sprintf("%02d", precip$month)
sum_df <- merge(dung_gr_br, precip, by=c("site", "Year", "month"), all.x=TRUE)
write.csv(sum_df, "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/precip_live_dead_dung_summary.csv",
          row.names=FALSE)

## testing: use previous month's rainfall
# precip$month_actual <- precip$month
# for(i in 1:NROW(precip)){
  # precip[i, 'month'] <- precip[i, 'month'] + 1
# }
# precip$month <- precip$month + 1


perc_g_fit <- lm(perc_green ~ bovid*precip_cm + site, data=sum_df)
green_sum_fit <- lm(green_sum ~ bovid*precip_cm + site, data=sum_df)

AIC(perc_g_fit)
AIC(green_sum_fit)

summary(perc_g_fit)
summary(green_sum_fit)

dung_sum <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/OPC_dung_analysis/dung_summary.csv")
dung_gr_br <- merge(dung_sum, green_brown_summary, by='transect')
dung_gr_br$bovid <- dung_gr_br$Buf + dung_gr_br$Cow
dung_gr_br$Date <- as.Date(dung_gr_br$Date, format="%d-%b-%y")
dung_gr_br$year_month <- format(dung_gr_br$Date, "%Y-%m")

subs <- dung_gr_br[which(dung_gr_br$year_month %in% c("2015-11", "2015-12")), ]
p <- ggplot(subs, aes(x=bovid, y=perc_green))
p <- p + geom_point()
p <- p + xlab("bovid dung density") + ylab("green proportion of biomass")
print(p)
subs_test <- cor.test(subs$bovid, subs$green_sum, method="spearman")

opp <- dung_gr_br[which(!dung_gr_br$year_month %in% c("2015-11", "2015-12", "2014-11")), ]
p <- ggplot(opp, aes(x=bovid, y=perc_green))
p <- p + geom_point()
p <- p + xlab("bovid dung density") + ylab("green proportion of biomass")
print(p)
opp_test <- cor.test(opp$bovid, opp$green_sum, method="spearman")

p <- ggplot(dung_gr_br, aes(x=bovid, y=perc_green))
p <- p + geom_point()
p <- p + xlab("bovid dung density") + ylab("green proportion of biomass")
p <- p + facet_wrap(~year_month)
pngname <- paste(img_dir, 'dung_vs_perc_live_by_month.png', sep="/")
png(file=pngname, units="in", res=300, width=7, height=7)
print(p)
dev.off()
t1 <- cor.test(dung_gr_br$bovid, dung_gr_br$perc_green, method="spearman")

p <- ggplot(dung_gr_br, aes(x=bovid, y=green_sum))
p <- p + geom_point()
p <- p + xlab("bovid dung density") + ylab("green biomass")
p <- p + facet_wrap(~year_month)
pngname <- paste(img_dir, 'dung_vs_live_biomass_by_month.png', sep="/")
png(file=pngname, units="in", res=300, width=7, height=7)
print(p)
dev.off()
t2 <- cor.test(dung_gr_br$bovid, dung_gr_br$green_sum, method="spearman")

sum_df <- data.frame('year_month'=c(), 'spearman_rho'=c(), 'p'=c())
i <- 1
for(month in unique(dung_gr_br$year_month)){
  subs <- dung_gr_br[which(dung_gr_br$year_month == month), ]
  t1 <- cor.test(subs$bovid, subs$green_sum, method="spearman")
  sum_df[i, 'year_month'] <- month
  sum_df[i, 'spearman_rho'] <- t1[[4]]
  sum_df[i, 'p'] <- t1[[3]]
  i <- i + 1
}

green_biomass_cor_summary <- sum_df
perc_gr_cor_summary <- sum_df

# live:dead vs back-calc grazing intensity
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/back_calc_match_last_measurement/bc_12_mo_intensity.csv"
b_c_intens_df <- read.csv(comparison_csv)
b_c_live_dead <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/back_calc_match_last_measurement/live_dead_summary.csv")
b_c_df <- merge(b_c_intens_df, b_c_live_dead, by='site')

img_dir <- "C:/Users/Ginger/Desktop"
p <- ggplot(b_c_df, aes(x=total_rem, y=avg_ratio_live_dead))
p <- p + geom_point()
p <- p + xlab("grazing intensity") + ylab("avg live:dead")
pngname <- paste(img_dir, 'intensity_vs_live_dead.png', sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

p <- ggplot(b_c_df, aes(x=total_rem, y=avg_live))
p <- p + geom_point()
p <- p + xlab("grazing intensity") + ylab("avg live biomass")
pngname <- paste(img_dir, 'intensity_vs_live_biomass.png', sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

p <- ggplot(b_c_df, aes(x=total_rem, y=perc_live))
p <- p + geom_point()
p <- p + xlab("grazing intensity") + ylab("avg green proportion of biomass")
pngname <- paste(img_dir, 'intensity_vs_perc_live.png', sep="/")
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

# summarize match by back-calc management routine
id_match <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/OPC_weather_id_match.csv")
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/back_calc_match_last_measurement/match_summary.csv")
sum_df <- merge(sum_df, id_match, by='site')
p <- ggplot(sum_df, aes(x=id, y=g_m2, group=sim_vs_emp))
p <- p + geom_point(aes(shape=sim_vs_emp))
p <- p + ylab('Biomass (grams per square m)') + xlab("Site")
p <- p + scale_x_continuous(breaks=seq(1, 8))
p <- p + print_theme
p <- p + theme(legend.position="none")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/OPC/back_calc_match_last_measurement/match_summary.png"
png(file=pngname, units="in", res=300, width=3, height=3)
print(p)
dev.off()

# how much does biomass in absence of grazing differ btw 6 weather stations?
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/zero_dens/combined_summary.csv")
img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/zero_dens"

sum_df$site <- factor(sum_df$site, levels=c('Loirugu', 'Rongai',
                                            'Kamok', 'Loidien', 
                                            'Serat', 'Research'))
p <- ggplot(sum_df, aes(x=site, y=total_kgha))
p <- p + geom_boxplot()
print(p)
pngname <- paste(img_dir, 'total_kgha_2015_boxplot.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

sum_2015 <- sum_df[which(sum_df$year == 2015), ]
p <- ggplot(sum_2015, aes(x=month, y=total_kgha, group=site))
p <- p + geom_line(aes(colour=site))
p <- p + geom_point(aes(colour=site))
print(p)
pngname <- paste(img_dir, 'total_kgha_line.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

## precip on OPC 2.15.17
indir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Kenya/input"
sites <- c('Golf_7', 'Sirima', 'Kamok', 'Loidien',
           'Research', 'Loirugu', 'Serat', 'Rongai')
widths <- c(6, 6, rep(7, 12))
df_list <- list()
for(s in sites){
  fwf <- paste(indir, "/", s, ".wth", sep="")
  df <- read.fwf(fwf, widths=widths)
  prec_df <- df[which(df$V1 == 'prec  '), ]
  prec_df <- prec_df[which(prec_df$V2 >= 2014), ]
  prec_df <- prec_df[, -1]
  prec_df$site <- s
  colnames(prec_df) <- c('Year', seq(1, 12), 'site')
  df_list[[s]] <- prec_df
}
precip_df <- do.call(rbind, df_list)
precip_res <- reshape(precip_df, idvar="site", varying=c(2:13),
                      v.names="precip_cm", direction="long", timevar='month',
                      new.row.names=1:1000)
write.csv(precip_res, "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/2014_2015_precip.csv",
          row.names=FALSE)

# plot average monthly precipitation by site
ave_monthly_precip <- do.call(rbind, (lapply(df_list, colMeans)))
trans_list <- list()
for(r in seq(1, dim(ave_monthly_precip)[1])){
  site <- rownames(ave_monthly_precip)[r]
  df <- data.frame('site'=rep(site, 12),
                   'ave_precip'=ave_monthly_precip[r, ],
                   'month'=seq(1,12))
  trans_list[[r]] <- df
}
plot_df <- do.call(rbind, trans_list)

p <- ggplot(plot_df, aes(x=month, y=ave_precip, group=site))
p <- p + geom_point(aes(colour=site))
p <- p + geom_line(aes(colour=site)) + ylab("average monthly precipitation (cm)")
print(p)
pngname <- "C:/Users/Ginger/Desktop/OPC_precip_line.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# plot yearly precipitation by site
yearly_precip <- do.call(rbind, lapply(df_list, rowSums))
trans_list <- list()
for(r in seq(1, dim(yearly_precip)[1])){
  site <- rownames(yearly_precip)[r]
  df <- data.frame('site'=rep(site, 18),
                   'yearly_precip'=yearly_precip[r, ],
                   'year'=seq(1998, 2015))
  trans_list[[r]] <- df
}
plot_df <- do.call(rbind, trans_list)

p <- ggplot(plot_df, aes(x=site, y=yearly_precip))
p <- p + geom_boxplot() + ylab("annual precipitation (cm)")
print(p)
pngname <- "C:/Users/Ginger/Desktop/OPC_boxplot.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()


# integrated test: empirical stocking density simulations compared to empirical biomass measurements
x10_comp_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison_x10_OPC_veg_9.30.16_by_weather.csv"
x10df <- read.csv(x10_comp_csv)
x10df <- x10df[which(x10df$sim_vs_emp == 'sim'), ]
x10df$multiplier <- '10'

grz_months <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/grazing_months.csv")
comparison_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison_OPC_veg_9.30.16_by_weather.csv"
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_by_weather.csv"
comp_df <- read.csv(comparison_csv)
veg_df <- read.csv(veg_csv)
veg_df <- veg_df[which(veg_df$year == 15), ]

p <- ggplot(veg_df, aes(x=month, y=mean_biomass_kgha))
p <- p + geom_ribbon(aes(ymin=min_biomass_kgha, ymax=max_biomass_kgha), alpha=0.2)
p <- p + facet_wrap(~site, scales='free_x')
p <- p + geom_point(aes(x=month, y=biomass_kg_ha, colour=sim_vs_emp),
                    data=comp_df)
p <- p + geom_line(aes(x=month, y=biomass_kg_ha, colour=sim_vs_emp),
                   data=comp_df)
p <- p + geom_point(aes(x=month, y=mean_biomass_kgha), data=grz_months)
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/comparison.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

comp_df = comp_df[which(comp_df$sim_vs_emp == 'sim'), ]
comp_df$multiplier <- '1'
comp_df = rbind(comp_df, x10df)
comp_df$multiplier <- factor(comp_df$multiplier, levels=c('10', '1'))
p <- ggplot(comp_df, aes(x=month, y=biomass_kg_ha))
p <- p + geom_point(aes(x=month, y=biomass_kg_ha, colour=multiplier))
p <- p + geom_line(aes(colour=multiplier))
p <- p + geom_point(aes(x=month, y=mean_biomass_kgha), data=grz_months)
p <- p + facet_wrap(~site, scales='free_x')
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Verification_calculations/OPC_integrated_test/empirical_stocking_density/empirical_density_vs_10x_empirical_density.png"
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# regional veg summary
## pin hits summarized by Felicia
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/HitsSummary_Aug_10_2016.csv"
veg_df <- read.csv(veg_csv)

veg_df[is.na(veg_df)] <- 0

spp_list <- unique(sapply(colnames(veg_df)[22:29], substr, start=5, stop=6))
prop_g_list <- paste("Prop", spp_list, "G", sep="")
prop_b_list <- paste("Prop", spp_list, "B", sep="")

for(i in c(1:length(spp_list))){
  veg_df[, (paste("prop_total_", spp_list[i], sep=""))] <- 
    veg_df[, prop_g_list[i]] + veg_df[, prop_b_list[i]]
}

fig_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Felicia/regional_veg_composition_plots"
for(spp in spp_list){
  col <- paste("prop_total_", spp, sep="")
  p <- ggplot(veg_df, aes_string(x="X", y=col))
  p <- p + geom_point() + ggtitle(spp) + xlab("Property index")
  filename <- paste(fig_dir, paste("prop_", spp, "_by_property.png", sep=""), sep="/")
  png(file=filename, units="in", res=300, width=7, height=5)
  print(p)
  dev.off()
}

# back-calculated management of Jenny's sites (33 sites, 9.14.16)
summary_csv = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/comparison_summary.csv"
sum_df = read.csv(summary_csv)

failed <- c('N4', 'M10', 'W06', 'GO', 'LO3', 'LO4')

p <- ggplot(sum_df, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp)) + geom_line(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=10, scales='free')
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/summary.png"
png(file=pngname, units="in", res=300, width=18, height=8)
print(p)
dev.off()

# linear interpolation
df_list = list()
diff_df_list = list()
i <- 1
for(site in unique(sum_df$site)){
  subs <- sum_df[which(sum_df$site == site), ]
  sim <- subs[which(subs$sim_vs_emp == 'simulated'), ]
  emp <- subs[which(subs$sim_vs_emp == 'empirical'), ]
  empinterp <- approx(x=emp$date, y=emp$biomass,
                      xout=seq(min(sim$date), max(sim$date), length.out=12))
  siminterp <- approx(x=sim$date, y=sim$biomass,
                      xout=seq(min(sim$date), max(sim$date), length.out=12))
  sumdiff <- sum(abs(siminterp$y - empinterp$y))
  interpdf <- rbind(data.frame('date'=siminterp$x, 'biomass'=siminterp$y,
                               'sim_vs_emp'=rep('simulated', 12),
                               'site'=rep(site, 12)),
                    data.frame('date'=empinterp$x, 'biomass'=empinterp$y,
                               'sim_vs_emp'=rep('empirical', 12),
                               'site'=rep(site, 12)))
  diff_df_list[[i]] <- data.frame('site'=site, 'sum_diff'=sumdiff)
  df_list[[i]] <- interpdf
  i <- i + 1
}
interpdf <- do.call(rbind, df_list)
diff_df <- do.call(rbind, diff_df_list)

p <- ggplot(interpdf, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=10, scales='free_x')
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/interpolated_points_all.png"
png(file=pngname, units="in", res=300, width=18, height=8)
print(p)
dev.off()

succeeded <- setdiff(interpdf$site, failed)
succ_interp <- interpdf[which(interpdf$site %in% succeeded), ]
p <- ggplot(succ_interp, aes(x=date, y=biomass, group=sim_vs_emp))
p <- p + geom_point(aes(colour=sim_vs_emp))
p <- p + geom_line(aes(colour=sim_vs_emp))
p <- p + facet_wrap(~site, ncol=4, scales='free_x')
p <- p + ylab("Biomass (g/m2)")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/interpolated_points_succeeded.png"
png(file=pngname, units="in", res=300, width=10, height=12)
print(p)
dev.off()

diff_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/sum_diff.csv"
write.csv(diff_df, diff_csv)

# joined sum_diff to site summary csv manually
site_summary_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/jenny_site_summary_open.csv"
site_summary_df = read.csv(site_summary_csv)

p <- ggplot(site_summary_df, aes(x=weather_distance_m, y=sum_weekly_diff))
p <- p + geom_point() + xlab("distance to nearest weather station (m)")
p <- p + ylab("sum(abs(simulated - empirical))")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/distance_to_weather_vs_diff.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

succeeded <- setdiff(site_summary_df$site, failed)
succ_subs <- site_summary_df[which(site_summary_df$site %in% succeeded), ]
p <- ggplot(succ_subs, aes(x=closest_weather, y=sum_weekly_diff))
p <- p + geom_point()
p <- p + xlab("Weather station")
p <- p + ylab("sum(abs(simulated - empirical))")
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Comparisons_with_CENTURY/back_calc_mgmt_9.13.16/figs/weather_stn_vs_diff.png"
png(file=pngname, units="in", res=300, width=7, height=5)
print(p)
dev.off()

## OPC veg analysis
coeff_var <- function(values){
  cv <- sd(values) / mean(values) * 100
  return(cv)
}

count <- function(values){
  return(length(values))
}

# analysis of veg data 9.30.16
metadata_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_metadata.csv"
PDM_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_data_9.30.16_PDM.csv"

meta_df <- read.csv(metadata_csv)
veg_df <- read.csv(PDM_csv)

# look for issues matching up PDM records with site lat/long
veg_ids <- unique(veg_df[, c("Date", "Site")])
veg_ids <- paste(veg_ids$Date, veg_ids$Site, sep="-")
meta_ids <- unique(meta_df[ , c("Date", "Site")])
meta_ids <- paste(meta_ids$Date, meta_ids$Site, sep="-")

mismatch <- c()
for (id in veg_ids){
  if (!is.element(id, meta_ids)){
    mismatch <- c(mismatch, id)
  }
}
in_veg_not_meta <- mismatch

mismatch <- c()
for (id in meta_ids){
  if (!is.element(id, veg_ids)){
    mismatch <- c(mismatch, id)
  }
}
in_meta_not_veg <- mismatch

PDM_count <- aggregate(PDM~Date + Site, data=veg_df, FUN=count)
trouble <- PDM_count[which(PDM_count$PDM != 11), ]

# calculate average PDM and sum of tree and shrub counts
# within transects
PDM_avg <- aggregate(PDM~Date + Site, data=veg_df, FUN=mean)
veg_df_res <- veg_df[which(veg_df$Position_m != 0), ]
tree_sum <- aggregate(Tree~Date + Site, data=veg_df_res, FUN=sum)
shrub_sum <- aggregate(Shrub~Date + Site, data=veg_df_res, FUN=sum)
veg_summarized <- PDM_avg
veg_summarized$biomass_kgha <- veg_summarized$PDM * 332.35 + 15.857
veg_summarized$tree_sum <- tree_sum$Tree
veg_summarized$shrub_sum <- shrub_sum$Shrub
veg_summarized$month <- 'NA'
veg_summarized$year <- 'NA'
for (r in (1:NROW(veg_summarized))){
  date_list <- unlist(strsplit(as.character(
    veg_summarized[r, "Date"]), split="-"))
  veg_summarized[r, 'month_year'] <- paste(date_list[2], date_list[3], sep="_")
  veg_summarized[r, 'year'] <- date_list[3]
}

meta_df$id <- paste(meta_df$Date, meta_df$Site, sep="_")
veg_summarized$id <- paste(veg_summarized$Date, veg_summarized$Site, sep="_")

site_list <- c('Loirugurugu', 'Loidien', 'Research', 'Kamok', 'Rongai', 'Serat', 'Simira', 'Golf 7')

# plot biomass measurements by site
veg_summarized$weather_stn <- 'NA'
for(site in site_list){
  meta_sub <- meta_df[which(meta_df$weather_2km == site), ]
  veg_summarized[which(veg_summarized$id %in% meta_sub$id), 'weather_stn'] <- site
}
veg_by_site <- veg_summarized[which(veg_summarized$weather_stn %in% site_list), ]

# summarize biomass by month
nrows <- length(site_list) * 2
diagnostic_df <- data.frame('total_measurements'=numeric(nrows),
                            'restricted_by_shrubs_trees'=numeric(nrows),
                            'site'=character(nrows), stringsAsFactors=FALSE)
df_list <- list()
i <- 1
for(site in site_list){
  meta_sub <- meta_df[which(meta_df$weather_2km == site), ]
  veg_sub <- veg_summarized[which(veg_summarized$id %in% meta_sub$id), ]
  if(NROW(veg_sub) == 0){
    next
  }
  total <- NROW(veg_sub)
  # veg_sub <- veg_sub[which(veg_sub$tree_sum <= 6), ]
  # veg_sub <- veg_sub[which(veg_sub$shrub_sum <= 8), ]
  restr <- NROW(veg_sub)
  if(restr == 0){
    next
  }
  diagnostic_df[i, 'total_measurements'] <- total
  diagnostic_df[i, 'restricted_by_shrubs_trees'] <- restr
  diagnostic_df[i, 'site'] <- as.character(site)
  mean_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=mean)
  min_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=min)
  max_biomass <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=max)
  n_meas <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=count)
  cv <- aggregate(biomass_kgha~month_year, data=veg_sub, FUN=coeff_var)
  sum_df <- cbind(mean_biomass, min_biomass$biomass_kgha, max_biomass$biomass_kgha,
                  n_meas$biomass_kgha, cv$biomass_kgha)
  colnames(sum_df) <- c('month_year', 'mean_biomass_kgha', 'min_biomass_kgha',
                        'max_biomass_kgha', 'num_measurements', 'cv')
  sum_df$site <- rep(site, NROW(sum_df))
  df_list[[site]] <- sum_df
  i <- i + 1
}
summary_df <- do.call(rbind, df_list)

diagnostic_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_sample_size.csv"
veg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/OPC_veg_9.30.16_by_weather_unrestricted_by_trees_shrubs.csv"
write.csv(diagnostic_df, file=diagnostic_csv, row.names=FALSE)
write.csv(summary_df, file=veg_csv, row.names=FALSE)

# analysis of veg data 11.25.15
outdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger"
file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/OPC_veg_data_11.25.15.csv"

data <- read.csv(file)
# restrict empirical biomass measurements to sites with few trees and shrubs
hist(data$Tree, breaks=50)
hist(data$Shrub, breaks=50)
treeperc <- quantile(data$Tree, probs=seq(0, 1, by=0.1))
shrubperc <- quantile(data$Shrub, probs=seq(0, 1, by=0.1))

df_list <- list()
data$date <- paste(data$Year, "-", data$Month, sep="")
for(site in c('Loirugurugu', 'Loidien', 'Research', 'Kamok', 'Rongai', 'Serat')){
  sub <- data[which(data$X2km_weat == site), ]
  sub <- sub[which(sub$Tree <=6), ]
  sub <- sub[which(sub$Shrub <= 8), ]
  summary <- aggregate(sub$Biomass, by=list(sub$date), FUN='mean')
  summary_2 <- aggregate(sub$Biomass, by=list(sub$date), FUN=length)
  summary_3 <- aggregate(sub$Biomass, by=list(sub$date), FUN=coeff_var)
  summary <- merge(summary, summary_2, by='Group.1')
  summary <- merge(summary, summary_3, by='Group.1')
  colnames(summary) <- c("date", "biomass (kg/ha)", "num_obs", "cv")
  summary$site <- rep(site, NROW(summary))
  df_list[[site]] <- summary
}
sum_df <- do.call(rbind, df_list)
outfile <- paste(outdir, 'summary_9.29.16.csv', sep="/")
write.csv(sum_df, outfile, row.names=FALSE)

locations <- unique(data[c("Lat", "Long")])
areas <- unique(data[c('Area')])
months <- unique(data["date"])

############## older analyses
Jenny_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/Jenny_biomass_reshaped.txt"
Sharon_file = "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Vegetation data_July-August_2014_biomass.txt"

cp_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Jenny/CP_combined.txt"

## Jenny's crude protein data
cp_dat <- read.table(cp_file, header = TRUE, sep = "\t")
cp_dat$Year <- factor(cp_dat$Year)
cp_dat$group <- interaction(cp_dat$Year, cp_dat$site)
cp_dat <- cp_dat[order(cp_dat$group), ]
cv_df <- aggregate(cp_dat$mj_per_kg_dm, by = list(cp_dat$Year), FUN = coeff_var)

## Jenny's data
veg_data <- read.table(Jenny_file, header = TRUE, sep = "\t")
veg_data$Transect <- as.factor(veg_data$site)
veg_data$caged <- as.factor(veg_data$caged)
veg_data$habitat <- as.factor(veg_data$habitat)

cattle_categorized <- veg_data[which(!is.na(veg_data$cattle)), ]
cattle_categorized <- cattle_categorized[cattle_categorized$caged == 'caged', ]
cattle_categorized$cattle <- factor(cattle_categorized$cattle, levels = c('none', 'low', 'medium', 'medium/high', 'high'))
cattle_categorized$site <- factor(cattle_categorized$site, levels = unique(cattle_categorized[order(cattle_categorized$cattle), 'site']))

p <- ggplot(cattle_categorized, aes(x = site, y = DM_g_per_sq_m, colour = cattle, order = cattle)) + geom_boxplot()
print(p)

sites <- c('GT', 'M05', 'MO', 'N4', 'W3', 'W06')
sites_veg <- veg_data[veg_data$site %in% sites, ]

p <- ggplot(sites_veg, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot()
p <- p + facet_wrap(~ caged, nrow = 2, ncol = 1)
p <- p + xlab('Site') + ylab('Dry matter (g per sq m)')
print(p)

uncaged_data <- sites_veg[sites_veg$caged == 'uncaged', ]

# means by site
aggdata <-aggregate(uncaged_data$DM_g_per_sq_m, by = list(uncaged_data$site), FUN=mean, na.rm=TRUE)

p <- ggplot(caged_data, aes(x = week, y = DM_g_per_sq_m)) + geom_line()
p <- p + facet_wrap(~ Transect, nrow = length(sites), ncol = 1) + print_theme
p <- p + scale_y_continuous(limits=c(50, 500))
p <- p + xlab('Sampling week') + ylab('Dry matter (g per sq m)')
p <- p + scale_x_continuous(breaks=c(0, 3, 6, 9)) + ggtitle("Empirical biomass: Jenny's caged sites")
print(p)

bplot <- ggplot(caged_data, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot() + print_theme
bplot <- bplot + scale_y_continuous(limits=c(50, 500))
bplot <- bplot + xlab('Site') + ylab('Dry matter (g per sq m)')
bplot <- bplot + ggtitle("Empirical biomass: Jenny's caged sites")
print(bplot)

pngname <- paste(outdir, "Jenny_caged_biomass.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 7)
print(p)
dev.off()

pngname <- paste(outdir, "Jenny_caged_biomass_boxplot.png", sep = "")
png(file = pngname, units = "in", res = 300, width = 5, height = 4)
print(bplot)
dev.off()

## Sharon's data
veg_data$Transect <- as.factor(veg_data$Transect)

model <- aov(DM_g_per_sq_m ~ Transect, data = veg_data)
tHSD <- TukeyHSD(model)

veg_data$label[veg_data$Transect == '1'] <- 'a,b'
veg_data$label[veg_data$Transect == '2'] <- 'a,b'
veg_data$label[veg_data$Transect == '3'] <- 'a,b'
veg_data$label[veg_data$Transect == '4'] <- 'a'
veg_data$label[veg_data$Transect == '5'] <- 'a,b'
veg_data$label[veg_data$Transect == '6'] <- 'b'

p <- ggplot(veg_data, aes(x = Transect, y = DM_g_per_sq_m)) + geom_boxplot()
p <- p + xlab('Transect') + ylab('Dry matter (g per sq m)')
print(p)
