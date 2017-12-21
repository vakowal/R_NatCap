# what is effect of rotation on the forage model?

library(ggplot2)
print_theme <- theme(strip.text.y=element_text(size=10), 
                     strip.text.x=element_text(size=9), 
                     axis.title.x=element_text(size=10), 
                     axis.title.y=element_text(size=10),
                     axis.text=element_text(size=10),
                     plot.title=element_text(size=10, face="bold"),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10)) + theme_bw()

# effect of rest-rotation
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/rest_effect/summary_figs/rest_effect_summary.csv")
sum_df$biomass_aglive_gm2 <- sum_df$aglivc * 2.5
sum_df$biomass_ag_gm2 <- sum_df$aglivc * 2.5 + sum_df$stdedc * 2.5
sum_df$perc_green_biomass <- sum_df$biomass_aglive_gm2 / sum_df$biomass_ag_gm2
sum_df$biomass_bglive_gm2 <- sum_df$bglivc * 2.5
sum_df$soilC_gm2 <- sum_df$somtc
sum_df$year <- floor(sum_df$time)
sum_df[is.na(sum_df$rest_period), 'rest_period'] <- 0

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/rest_effect/summary_figs"

# look at aboveground and belowground biomass and soilC
# at different levels of continuous grazing
cont_df <- sum_df[sum_df$treatment == 'continuous' & sum_df$year < 2007,
                  c('biomass_ag_gm2', 'biomass_bglive_gm2',
                    'soilC_gm2', 'time', 'sd', 'year')]
cont_res <- reshape(cont_df, varying=c('biomass_ag_gm2', 'biomass_bglive_gm2', 'soilC_gm2'),
                    timevar='label', v.names='amount',
                    times=c('biomass_ag_gm2', 'biomass_bglive_gm2', 'soilC_gm2'),
                    direction='long')
cont_res$sd <- as.factor(cont_res$sd)
p <- ggplot(cont_res, aes(x=time, y=amount, group=sd))
p <- p + geom_line(aes(colour=sd))
p <- p + facet_wrap(~label, scales='free')
print(p)

annual_soilC <- aggregate(soilC_gm2~sd + year, data=cont_df, FUN=mean)
annual_soilC$sd <- as.factor(annual_soilC$sd)
p <- ggplot(annual_soilC, aes(x=year, y=soilC_gm2, group=sd))
p <- p + geom_line(aes(colour=sd))
p <- p + ylab("Avg annual soil C")
print(p)
pngname <- paste(imgdir, "SoilC_avg_annual_continuous.png", sep="/")
png(file=pngname, units="in", res=300, width=6, height=4)
print(p)
dev.off()

pre_2007 <- sum_df[sum_df$year < 2007, c('biomass_ag_gm2', 'biomass_bglive_gm2',
                                         'soilC_gm2', 'time', 'sd', 'year',
                                        'rest_period', 'treatment')]
pre_2007$rest_period <- as.factor(pre_2007$rest_period)
mean_ag_biomass <- aggregate(biomass_ag_gm2~sd+rest_period+year+treatment,
                             data=pre_2007, FUN=mean)
mean_bg_biomass <- aggregate(biomass_bglive_gm2~sd+rest_period+year+treatment,
                             data=pre_2007, FUN=mean)
mean_soilC <- aggregate(soilC_gm2~sd+rest_period+year+treatment,
                             data=pre_2007, FUN=mean)
p <- ggplot(mean_ag_biomass, aes(x=year, y=biomass_ag_gm2, group=rest_period))
p <- p + geom_line(aes(colour=rest_period))
p <- p + facet_wrap(~sd)
pngname <- paste(imgdir, "AbovegroundBiomass_avg_annual_rot.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

p <- ggplot(mean_bg_biomass, aes(x=year, y=biomass_bglive_gm2, group=rest_period))
p <- p + geom_line(aes(colour=rest_period))
p <- p + facet_wrap(~sd)
pngname <- paste(imgdir, "BelowgroundLive_avg_annual_rot.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

p <- ggplot(mean_soilC, aes(x=year, y=soilC_gm2, group=rest_period))
p <- p + geom_line(aes(colour=rest_period))
p <- p + facet_wrap(~sd)
pngname <- paste(imgdir, "SoilC_avg_annual_rot.png", sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# Ucross: composition
# composition calculated in python script
comp_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/proportion_summary_0_anim.csv")
comp_df <- comp_df[which(comp_df$cp_ratio == 1.2), ]
comp_resh <- reshape(comp_df, varying=c('continuous', 'rotation'),
                     v.names='perc_diff', timevar='treatment', times=c('continuous', 'rotation'),
                     direction='long')
comp_resh$date <- comp_resh$year + (1/12) * comp_resh$month
p <- ggplot(comp_resh, aes(x=date, y=perc_diff, group=treatment))
p <- p + geom_line(aes(linetype=treatment))
# p <- p + facet_wrap(cp_ratio~high_quality_perc)
p <- p + ylab("Proportion nutritious grass")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/prop_high_cp_2_pastures.png"
png(file=pngname, units="in", res=300, width=8, height=4)
print(p)
dev.off()

# what's happening to pasture with 2 pastures ?
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/rot_3_pastures_0.2_v_0.8_perc/pasture_summary.csv")
sum_df <- sum_df[, -c(1, 2, 4, 5)]
sum_resh <- reshape(sum_df, varying=c('high_quality_total_kgha', 'low_quality_total_kgha'),
                    v.names='biomass', timevar='forage',
                    times=c('high_quality', 'low_quality'),
                    direction='long')
p <- ggplot(sum_resh, aes(x=step, y=biomass, group=forage))
p <- p + geom_line(aes(linetype=forage))
p <- p + facet_wrap(~pasture_index)
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/biomass_rot_3_pastures_0.2_v_0.8_perc.png"
png(file=pngname, units="in", res=300, width=7, height=2)
print(p)
dev.off()

# cow weight gain from Jennie Muir-Gordon's data
weight_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/data/Merlin_ranch/Jennie_Muir-Gordon/2016 cattle weights and grazing/2016 Preg check and weight.csv")
weight_df$Age <- factor(weight_df$Age)
p <- ggplot(weight_df, aes(x=Age, y=WEIGHTPREG))
p <- p + geom_boxplot()
print(p)

# average weight gain from 2 year old to 3 year old
mean(weight_df[which(weight_df$Age == '3 years'), 'WEIGHTPREG'],
     na.rm=TRUE) - 
  mean(weight_df[which(weight_df$Age == '2 years'), 'WEIGHTPREG'],
       na.rm=TRUE)

# first get grass production approximately right: zero sd
summary_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/zero_sd_KNZ_2/summary_results.csv")
live_grass_cols <- grep("green_kgha", colnames(summary_df), value=TRUE)
summary_df$total_green_kgha <- rowSums(summary_df[, live_grass_cols])
live_by_month <- as.data.frame(aggregate(total_green_kgha~month,
                               data=summary_df, FUN=mean))
live_by_month$lb_per_acre <- live_by_month$total_green_kgha * 0.892
p <- ggplot(live_by_month, aes(x=month, y=lb_per_acre))
p <- p + geom_point()
p <- p + scale_x_continuous(breaks=seq(1, 12, by=1))
p <- p + ylab("Average live biomass (lb/acre)")
print(p)
pngname <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ucross/zero_sd_figs/zero_sd_live_biomass.png"
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()

all_grass_cols <- grep("kgha", colnames(summary_df), value=TRUE)
all_grass_cols <- all_grass_cols[all_grass_cols != 'total_green_kgha']
summary_df$total_grass <- rowSums(summary_df[, all_grass_cols])
total_by_month <- as.data.frame(aggregate(total_grass~month,
                                          data=summary_df, FUN=mean))
p <- ggplot(total_by_month, aes(x=month, y=total_grass))
p <- p + geom_point()
p <- p + scale_x_continuous(breaks=seq(1, 12, by=1))
print(p)

# Ortega-S et al 2013
# regrowth at different defoliation levels
regrow_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/defol_exp/regrowth_summary.csv")
regrow_df$defoliation_step <- factor(regrow_df$defoliation_step)
p <- ggplot(regrow_df, aes(x=defoliation_level, y=perc_regrowth_defol_step))
p <- p + geom_point() + print_theme
p <- p + facet_wrap(~defoliation_step, nrow=2)
p <- p + ylab("Percent regrowth after defoliation")
print(p)
pngname <- paste(img_dir, 'percent_regrowth~defol_level.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(regrow_df, aes(x=defoliation_level, y=perc_regrowth_d.1))
p <- p + geom_point()
print(p)

p <- ggplot(regrow_df, aes(x=defoliation_level, y=perc_regrowth_d.2))
p <- p + geom_point()
print(p)

# strict growth: biomass produced in defoliation step
grow_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/defol_exp/growth_summary.csv")
grow_df$defoliation_step <- factor(grow_df$defoliation_step)
p <- ggplot(grow_df, aes(x=defoliation_level, y=growth_defol_step))
p <- p + geom_point() + print_theme
p <- p + facet_wrap(~defoliation_step, nrow=2)
p <- p + ylab("Growth after defoliation")
print(p)
pngname <- paste(img_dir, 'growth~defol_level.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# recovery time at different defoliation levels
recovery_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/defol_exp/recovery_summary.csv")
recovery_df$defoliation_step <- factor(recovery_df$defoliation_step)
recovery_df <- recovery_df[which(recovery_df$steps_to_recover < 999), ]
p <- ggplot(recovery_df, aes(x=defoliation_level, y=steps_to_recover))
p <- p + geom_point() + print_theme + ylab("Steps to recover to starting biomass")
p <- p + facet_wrap(~defoliation_step, nrow=2)
print(p)
pngname <- paste(img_dir, 'steps_to_recover~defol_level.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

p <- ggplot(recovery_df, aes(x=stocking_density, y=steps_to_recover))
p <- p + geom_point()
p <- p + facet_grid(~defoliation_step, scales="free")
print(p)

# stocking density test, Ucross
img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/stocking_density_figs"

# no forced intake
sum_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/stocking_density_test/stocking_density_test_summary.csv")
sum_df$gain_diff <- sum_df$gain_._diff * 100
sum_df$pasture_diff <- sum_df$pasture_._diff * 100
sum_df <- sum_df[, c('num_animals', 'num_pastures', 'gain_diff',
                     'pasture_diff')]
sum_res <- reshape(sum_df, idvar=c("num_animals", "num_pastures"),
                   varying=c('gain_diff', 'pasture_diff'),
                   v.names='diff', times=c('Animal gain', 'Pasture biomass'),
                   direction="long")
p <- ggplot(sum_res, aes(x=num_pastures, y=diff))
p <- p + geom_point()
p <- p + facet_grid(time~num_animals, scales='free')
p <- p + xlab("Number of pastures")
p <- p + ylab("Benefit of rotation (% relative to continuous)")
pngname <- paste(img_dir, 'benefit_of_rotation_no_forced_intake.png', sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# intake forced to AUE
sum_fi_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/stocking_density_test_force_intake/stocking_density_test_summary.csv")
sum_fi_df$gain_diff <- sum_fi_df$gain_._diff * 100
sum_fi_df$pasture_diff <- sum_fi_df$pasture_._diff * 100
sum_fi_df <- sum_fi_df[, c('num_animals', 'num_pastures', 'gain_diff',
                     'pasture_diff')]
sum_fi_res <- reshape(sum_fi_df, idvar=c("num_animals", "num_pastures"),
                   varying=c('gain_diff', 'pasture_diff'),
                   v.names='diff', times=c('Animal gain', 'Pasture biomass'),
                   direction="long")
p <- ggplot(sum_fi_res, aes(x=num_pastures, y=diff))
p <- p + geom_point()
p <- p + facet_grid(time~num_animals, scales='free_y')
p <- p + xlab("Number of pastures")
p <- p + ylab("Benefit of rotation (% relative to continuous)")
pngname <- paste(img_dir, 'benefit_of_rotation_forced_intake.png', sep="/")
png(file=pngname, units="in", res=300, width=7, height=4)
print(p)
dev.off()

# stocking density test
all_months <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/stocking_density_n_pasture_test_all_months/summary.csv")
all_months$gain_diff <- all_months$gain_._diff * 100
all_months$pasture_diff <- all_months$pasture_._diff * 100
p <- ggplot(all_months, aes(x=num_pastures, y=pasture_diff))
p <- p + geom_point()
p <- p + facet_wrap(~num_animals)
print(p)

p <- ggplot(all_months, aes(x=num_pastures, y=gain_diff))
p <- p + geom_point()
p <- p + facet_wrap(~num_animals)
print(p)

restr_months <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/stocking_density_test_mo2-11/summary.csv")

all_months$month_treatment <- 'Graze all months'
restr_months$month_treatment <- 'Graze months 2-11'
sum_df <- rbind(all_months, restr_months)
sum_df$gain_diff <- sum_df$gain_._diff * 100
sum_df$pasture_diff <- sum_df$pasture_._diff * 100

p <- ggplot(sum_df, aes(x=num_animals, y=gain_diff))
p <- p + geom_point() + ylab("Benefit of rotation: animal gain (%)")
p <- p + xlab("Herd size")
p <- p + facet_wrap(~month_treatment)
print(p)
pngname <- paste(img_dir, 'animal_gain_benefit_of_rotation.png', sep="/")
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()

p <- ggplot(sum_df, aes(x=num_animals, y=pasture_diff))
p <- p + geom_point() + ylab("Benefit of rotation: pasture biomass (%)")
p <- p + xlab("Herd size")
p <- p + facet_wrap(~month_treatment)
print(p)
pngname <- paste(img_dir, 'pasture_biomass_benefit_of_rotation.png', sep="/")
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()

# increasing desirable spp?
rot_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ucross/rot_3_pastures_0.2_v_0.8_perc_1.2_PRDX_1.2_v_1.6/pasture_summary.csv")
rot_df$total_grass <- rot_df$high_quality_total_kgha + rot_df$low_quality_total_kgha
rot_df$prop_hq <- rot_df$high_quality_total_kgha / rot_df$total_grass
p <- ggplot(rot_df, aes(x=step, y=prop_hq, group=pasture_index))
p <- p + geom_line()
print(p)

# diff between rotated and continuous, plots etc
img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/summary_figs"
cont_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/stocking_density_test_all_months/cont_35_animals/summary_results.csv")
sm_rot_p_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/smart_rotation_1mo/pasture_summary.csv")
sm_rot_a_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/smart_rotation_1mo/animal_summary.csv")
bl_rot_p_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/stocking_density_test_all_months/blind_rot_35_animals/pasture_summary.csv")
bl_rot_a_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/stocking_density_test_all_months/blind_rot_35_animals/animal_summary.csv")
zero_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/WitW/model_results/Ortega-S_et_al/zero_density/summary_results.csv")

cont_df$date <- cont_df$year + (1/12) * cont_df$month
zero_df$date <- zero_df$year + (1/12) * zero_df$month
sm_rot_p_df$date <- sm_rot_p_df$year + (1/12) * sm_rot_p_df$month
sm_rot_a_df$date <- sm_rot_a_df$year + (1/12) * sm_rot_a_df$month
bl_rot_p_df$date <- bl_rot_p_df$year + (1/12) * bl_rot_p_df$month
bl_rot_a_df$date <- bl_rot_a_df$year + (1/12) * bl_rot_a_df$month

# forage availability in absence of grazing
lines <- c("solid", "longdash", "dotted")
sub_z <- zero_df[which(zero_df$date > 2000 & zero_df$date < 2005), ]
sub_zreshp <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/zero_density/sub_z_resh.csv")
p <- ggplot(sub_zreshp, aes(x=step, y=kg_ha, group=biomass_label))
p <- p + geom_line(aes(linetype=biomass_label))
p + scale_linetype_manual(values=lines)
print(p)
pngname <- paste(img_dir, 'forage_zero_sd_2001.png', sep="/")
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()

# summarize mean pasture biomass between rotation and continuous
sm_r_pmean <- aggregate(grass_total_kgha~date + year + month, data=sm_rot_p_df, FUN=mean)
bl_r_pmean <- aggregate(grass_total_kgha~date + year + month, data=bl_rot_p_df, FUN=mean)
cont_df$total_grass_kgha <- cont_df$grass_dead_kgha + cont_df$grass_green_kgha
cont_pmean <- aggregate(total_grass_kgha~date + year + month, data=cont_df, FUN=mean)
colnames(cont_pmean)[4] <- "grass_total_kgha"

# difference between treatments, by month
colnames(sm_r_pmean)[4] <- 'sm_r_kgha'
colnames(bl_r_pmean)[4] <- 'bl_r_kgha'
colnames(cont_pmean)[4] <- 'cont_kgha'
pdiff <- merge(sm_r_pmean, bl_r_pmean, by=c('date', 'month', 'year'))
pdiff <- merge(pdiff, cont_pmean, by=c('date', 'month', 'year'))
pdiff$kgha_blrot_minus_cont <- pdiff$bl_r_kgha - pdiff$cont_kgha
pdiff$kgha_smrot_minus_cont <- pdiff$sm_r_kgha - pdiff$cont_kgha
pdiff$kgha_smrot_minus_blrot <- pdiff$sm_r_kgha - pdiff$bl_r_kgha

pdiff$month <- factor(pdiff$month)
p <- ggplot(pdiff, aes(x=month, y=kgha_blrot_minus_cont))
p <- p + geom_boxplot()
p <- p + ylab("Biomass (blind rot.) - biomass (continuous)")
print(p)
pngname <- paste(img_dir, 'biomass_blrot_minus_continuous.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

p <- ggplot(pdiff, aes(x=month, y=kgha_smrot_minus_cont))
p <- p + geom_boxplot()
p <- p + ylab("Biomass (smart rot.) - biomass (continuous)")
print(p)
pngname <- paste(img_dir, 'biomass_smrot_minus_continuous.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

p <- ggplot(pdiff, aes(x=month, y=kgha_smrot_minus_blrot))
p <- p + geom_boxplot()
p <- p + ylab("Biomass (smart rot.) - biomass (blind rot.)")
print(p)
pngname <- paste(img_dir, 'biomass_smrot_minus_blrot.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

# biomass: continuous vs blind rotation
cont_df$grass_total_kgha <- cont_df$grass_dead_kgha + cont_df$grass_green_kgha
cont_df$pasture_index <- 'cont'
p <- ggplot(bl_rot_p_df, aes(x=date, y=grass_total_kgha, group=pasture_index))
p <- p + geom_line(colour="#56B4E9")
p <- p + geom_line(data=cont_df, aes(x=date, y=grass_total_kgha),
                   size=1)
p <- p + ylab("Pasture biomass (kg/ha)")
print(p)
pngname <- paste(img_dir, 'bl_rot_cont_biomass_summary.png', sep='/')
png(file=pngname, units="in", res=300, width=8, height=5)
print(p)
dev.off()


# summarize gain and offtake between rotation and continuous
diff_resh <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/WitW/Ortega-S_et_al/summary_figs/diff_df_resh.csv")
diff_resh$treatment <- factor(diff_resh$treatment)
p <- ggplot(diff_resh, aes(x=treatment, y=offtake_per_anim))
p <- p + geom_boxplot()
print(p)

p <- ggplot(diff_resh, aes(x=treatment, y=gain))
p <- p + geom_boxplot()
print(p)
pngname <- paste(img_dir, 'gain_by_treatment_all_mos.png', sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

subs <- diff_resh[which(diff_resh$month < 12), ]
p <- ggplot(subs, aes(x=treatment, y=gain))
p <- p + geom_boxplot()
print(p)
pngname <- paste(img_dir, 'gain_by_treatment_mo_1-11.png', sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

subs <- diff_resh[which(diff_resh$month < 12 & diff_resh$month > 1), ]
p <- ggplot(subs, aes(x=treatment, y=gain))
p <- p + geom_boxplot()
print(p)
pngname <- paste(img_dir, 'gain_by_treatment_mo_2-11.png', sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

# what is difference in cattle weight gain and offtake?
anim_c <- cont_df[, c('cattle_gain_kg', 'total_offtake', 'date', 'year', 'month')]
anim_blr <- bl_rot_a_df[ c('animal_gain', 'total_offtake', 'date', 'year', 'month')]
anim_smr <- sm_rot_a_df[ c('animal_gain', 'total_offtake', 'date', 'year', 'month')]
colnames(anim_c)[1] <- 'gain_cont'
colnames(anim_c)[2] <- 'offtake_cont'
colnames(anim_blr)[1] <- 'gain_bl_rot'
colnames(anim_blr)[2] <- 'offtake_bl_rot'
colnames(anim_smr)[1] <- 'gain_sm_rot'
colnames(anim_smr)[2] <- 'offtake_sm_rot'
diff_df <- merge(anim_c, anim_blr, by=c('date', 'month', 'year'))
diff_df <- merge(diff_df, anim_smr, by=c('date', 'month', 'year'))
diff_df$gain_blrot_minus_cont <- diff_df$gain_bl_rot - diff_df$gain_cont
diff_df$gain_smrot_minus_cont <- diff_df$gain_sm_rot - diff_df$gain_cont
diff_df$gain_smrot_minus_blrot <- diff_df$gain_sm_rot - diff_df$gain_bl_rot
diff_df$offtake_blrot_minus_cont <- diff_df$offtake_bl_rot - diff_df$offtake_cont
diff_df$offtake_smrot_minus_cont <- diff_df$offtake_sm_rot - diff_df$offtake_cont
diff_df$offtake_smrot_minus_blrot <- diff_df$offtake_sm_rot - diff_df$offtake_bl_rot
diff_df$offtake_per_anim_cont <- diff_df$offtake_cont / 0.275
diff_df$offtake_per_anim_blrot <- diff_df$offtake_bl_rot / 2.31
diff_df$offtake_per_anim_smrot <- diff_df$offtake_sm_rot / 2.31
diff_df$offtake_per_anim_blrot_minus_cont <- diff_df$offtake_per_anim_blrot - diff_df$offtake_per_anim_cont
diff_df$offtake_per_anim_smrot_minus_cont <- diff_df$offtake_per_anim_smrot - diff_df$offtake_per_anim_cont

# is difference in gain attributable to difference in offtake?
p <- ggplot(diff_df, aes(x=offtake_per_anim_blrot_minus_cont, y=gain_blrot_minus_cont))
p <- p + geom_point(aes(color=month))
print(p)
pngname <- paste(img_dir, 'offtake_v_gain_bl_minus_cont_35_animals.png', sep="/")
png(file=pngname, units="in", res=300, width=4, height=3)
print(p)
dev.off()

p <- ggplot(diff_df, aes(x=offtake_per_anim_smrot_minus_cont, y=gain_smrot_minus_cont))
p <- p + geom_point(aes(color=month))
print(p)

summary(diff_df$gain_blrot_minus_cont)
summary(diff_df$gain_smrot_minus_cont)
summary(diff_df$gain_smrot_minus_blrot)
hist(diff_df$gain_blrot_minus_cont, breaks=50)
hist(diff_df$gain_smrot_minus_cont, breaks=50)
hist(diff_df$gain_smrot_minus_blrot, breaks=50)

diff_df$month <- factor(diff_df$month)
p <- ggplot(diff_df, aes(x=month, y=gain_smrot_minus_blrot))
p <- p + ylab('cattle gain (smart) - cattle gain (blind)')
p <- p + geom_boxplot()
print(p)
pngname <- paste(img_dir, 'smrot_minus_blrot_1mo.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

p <- ggplot(diff_df, aes(x=month, y=gain_blrot_minus_cont))
p <- p + geom_boxplot()
p <- p + ylab('cattle gain (blind) - cattle gain (continuous)')
print(p)
pngname <- paste(img_dir, 'blrot_minus_cont_1mo.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

p <- ggplot(diff_df, aes(x=month, y=gain_smrot_minus_cont))
p <- p + geom_boxplot()
p <- p + ylab('cattle gain (smart) - cattle gain (continuous)')
print(p)
pngname <- paste(img_dir, 'smrot_minus_cont_1mo.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

# what is difference in pasture biomass?
mean_cont <- mean(cont_df$total_grass_kgha)
mean_blrot <- mean(bl_rot_p_df[which(bl_rot_p_df$date <= 2012), 'grass_total_kgha'])
mean_smrot <- mean(sm_rot_p_df[which(sm_rot_p_df$date <= 2012), 'grass_total_kgha'])

# biomass over time in continuous grazed
subs_c <- cont_df[which(cont_df$date > 2000 & cont_df$date < 2005), ]
p <- ggplot(subs_c, aes(x=date, y=grass_green_kgha))
p <- p + geom_line()
print(p)
pngname <- paste(img_dir, 'pasture_biomass_cont.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

# biomass in selected pasture
bl_rot_p_df$pasture_index <- factor(bl_rot_p_df$pasture_index)
selected_df <- data.frame('date'=numeric(NROW(bl_rot_a_df)),
                          'grass_total_kgha'=numeric(NROW(bl_rot_a_df)),
                          stringsAsFactors=FALSE)
for(r in 1:NROW(bl_rot_a_df)){
  pindx <- bl_rot_a_df[r, 'pasture_index']
  t <- bl_rot_a_df[r, 'step']
  selected_df[r, 'grass_total_kgha'] <- bl_rot_p_df[which(bl_rot_p_df$step == (t-1) &
                                                            bl_rot_p_df$pasture_index == pindx),
                                                 'grass_total_kgha']
  selected_df[r, 'date'] <- bl_rot_p_df[which(bl_rot_p_df$step == (t-1) &
                                                bl_rot_p_df$pasture_index == pindx),
                                     'date']
}

subs_df <- bl_rot_p_df[which(bl_rot_p_df$date > 2000 & bl_rot_p_df$date < 2005), ]
subs_sel <- selected_df[which(selected_df$date > 2000 & selected_df$date < 2005), ]
p <- ggplot(subs_df, aes(x=date, y=grass_total_kgha))
p <- p + geom_line(aes(color=pasture_index))
p <- p + geom_point(data=subs_sel, aes(x=date, y=grass_total_kgha))
print(p)
pngname <- paste(img_dir, 'pasture_biomass_blind_rotation_1_mo.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()

## smart rotation ; over time, between pastures, and in selected pasture
sm_rot_p_df$pasture_index <- factor(sm_rot_p_df$pasture_index)
selected_df <- data.frame('date'=numeric(NROW(sm_rot_a_df)),
                          'grass_total_kgha'=numeric(NROW(sm_rot_a_df)),
                          stringsAsFactors=FALSE)
for(r in 1:NROW(sm_rot_a_df)){
  pindx <- sm_rot_a_df[r, 'pasture_index']
  t <- sm_rot_a_df[r, 'step']
  selected_df[r, 'grass_total_kgha'] <- sm_rot_p_df[which(sm_rot_p_df$step == (t-1) &
                                                            sm_rot_p_df$pasture_index == pindx),
                                                    'grass_total_kgha']
  selected_df[r, 'date'] <- sm_rot_p_df[which(sm_rot_p_df$step == (t-1) &
                                                sm_rot_p_df$pasture_index == pindx),
                                        'date']
}
subs_df <- sm_rot_p_df[which(sm_rot_p_df$date > 2000 & sm_rot_p_df$date < 2005), ]
subs_sel <- selected_df[which(selected_df$date > 2000 & selected_df$date < 2005), ]
p <- ggplot(subs_df, aes(x=date, y=grass_total_kgha))
p <- p + geom_line(aes(color=pasture_index))
p <- p + geom_point(data=subs_sel, aes(x=date, y=grass_total_kgha))
print(p)
pngname <- paste(img_dir, 'pasture_biomass_smart_rotation_3_mo.png', sep="/")
png(file=pngname, units="in", res=300, width=10, height=5)
print(p)
dev.off()


### CGIAR Peru
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/comparison_8.9.16.csv"
sum_df <- read.csv(sum_csv)

metrics <- c('avg_biomass', "total_biomass", "avg_monthly_gain")

rotated <- subset(sum_df, duration < 24)
rotated$avg_monthly_gain <- rotated$gain_kg / 12

# precipitation
precip_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/avg_precip"
precip_df <- data.frame("subbasin"=numeric(14), "mean_precip"=numeric(14),
                        "sum_precip"=numeric(14), "std_precip"=numeric(14))
for(sbasin in 1:14){
  precip_csv <- paste(precip_dir, "/", sbasin, ".csv", sep="")
  df <- read.csv(precip_csv, header=FALSE)
  precip_df[sbasin, "subbasin"] <- sbasin
  precip_df[sbasin, "mean_precip"] <- mean(df$V1)
  precip_df[sbasin, "sum_precip"] <- sum(df$V1)
  precip_df[sbasin, "std_precip"] <- sd(df$V1)
}

# what is the difference in each metric between rotation schedules of the same duration and subbasin?
ids <- unique(rotated[, c('subbasin', 'duration')])
diff <- data.frame(abs(diff(as.matrix(rotated[, metrics]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
colnames(odd_diff) <- paste("diff", colnames(odd_diff), sep="_")
diff_df <- cbind(odd_diff, ids)
diff_df <- merge(diff_df, precip_df, by="subbasin")
summary_df <- diff_df
for(m in metrics){
  summary_df[, paste("mean", m, sep="_")] <- diff_df[, paste("diff", m, sep="_")]/2
}

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/figs"
precip_descrips <- colnames(precip_df)[c(2, 3, 4)]
  
for(metric in metrics){
  dif_metric <- paste("diff", metric, sep="_")
  for(pr in precip_descrips){
    p <- ggplot(summary_df, aes_string(x=pr, y=dif_metric,
                                    group="duration"))
    p <- p + geom_point(aes(colour=as.factor(duration)))
    p <- p + print_theme
    pngname <- paste(img_dir, "/diff_", metric, "~", pr, ".png", sep="")
    png(file=pngname, units="in", res=150, width=7, height=3)
    print(p)
    dev.off()
  }
}

# mean of metrics within rotated schedules, vs non-rotated (full-year) schedule
constant <- subset(sum_df, duration == 24)
constant$avg_monthly_gain <- constant$gain_kg / 24
mean_df <- aggregate(rotated[, metrics], by=list(
                     rotated$subbasin, rotated$duration),
                     mean)
colnames(mean_df)[1] <- "subbasin"
colnames(mean_df)[2] <- "duration"
mean_df <- merge(mean_df, precip_df, by="subbasin")
constant_df <- merge(constant, precip_df, by="subbasin")
for(metric in metrics){
  for(pr in precip_descrips){
    p <- ggplot(mean_df, aes_string(x=pr, y=metric,
                                       group="duration"))
    p <- p + geom_jitter(aes(colour=as.factor(duration)))
    p <- p + geom_point(data=constant_df, aes_string(
                        x=pr, y=metric))
    p <- p + print_theme
    pngname <- paste(img_dir, "/", metric, "~", pr,
                     "_mean_rotation_vs_full_year.png", sep="")
    png(file=pngname, units="in", res=150, width=7, height=3)
    print(p)
    dev.off()
  }
}

# means by subbasin instead of precip descriptors
for(metric in metrics){
  p <- ggplot(mean_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  p <- p + geom_jitter(aes(colour=as.factor(duration)),
                       width=0.3)
  p <- p + geom_point(data=constant_df, aes_string(
                      x="subbasin", y=metric))
  p <- p + print_theme
  pngname <- paste(img_dir, "/", metric, "~subbasin",
                   "_mean_rotation_vs_full_year.png", sep="")
  png(file=pngname, units="in", res=150, width=7, height=3)
  print(p)
  dev.off()
}

# raw values: rotational schedules vs non-rotated (full-year) schedule
rotated_df <- merge(rotated, precip_df, by="subbasin")
for(metric in metrics){
  p <- ggplot(rotated_df, aes_string(x="subbasin", y=metric,
                                  group="duration"))
  p <- p + geom_jitter(aes(colour=as.factor(duration)),
                       width=0.3)
  p <- p + geom_point(data=constant_df, aes_string(
    x="subbasin", y=metric))
  p <- p + print_theme
  pngname <- paste(img_dir, "/", metric, "~subbasin",
                   "_rotation_vs_full_year.png", sep="")
  png(file=pngname, units="in", res=150, width=7, height=3)
  print(p)
  dev.off()
}

###### differences in rotated schedules vs differences between interventions
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/test_rotation/figs"
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/rotation_high_sd/comparison_8.22.16.csv"
sum_df <- read.csv(sum_csv)

ids <- unique(sum_df[, c('subbasin', 'duration', 'sd_level', 'animal_type')])
sum_df$id <- paste(sum_df$subbasin, sum_df$duration, sum_df$sd_level, sum_df$animal_type, sep=".")
biomass_means <- aggregate(sum_df$avg_biomass, by=list(sum_df$id), FUN=mean)
colnames(biomass_means) <- c('id', 'avg_biomass')
sum_df_sub <- unique(sum_df[, colnames(sum_df)[c(1, 5, 8, 9, 10)]])
biomass_means <- merge(sum_df_sub, biomass_means, by='id')
biomass_means[(biomass_means$duration == 204 & 
                 biomass_means$sd_level == 'rechigh'), 'level'] <- 'high'
biomass_means[(biomass_means$duration == 4 & 
                 biomass_means$sd_level == 'rechigh'), 'level'] <- 'high-rot'
biomass_means[(biomass_means$duration == 204 & 
                 biomass_means$sd_level == 'low'), 'level'] <- 'low'
biomass_means[(biomass_means$duration == 4 & 
                 biomass_means$sd_level == 'low'), 'level'] <- 'low-rot'
biomass_means$intervention <- paste(biomass_means$animal_type, biomass_means$level, sep="-")

## calculate all pairwise differences between interventions within subbasin
diff_list <- list()
sb <- unique(biomass_means$subbasin)
for(sbasin in sb){
  subs <- biomass_means[which(biomass_means$subbasin == sbasin), 'avg_biomass']
  diffs <- c(dist(subs))
  df <- data.frame('subbasin'=rep(sbasin, length(diffs)), 'diff'=diffs)
  diff_list[[sbasin]] <- df
}
diff_df <- do.call(rbind, diff_list)
diff_df$difference_type <- 'intervention'

# calculate all differences between schedules of the same intervention type
sbasins <- unique(sum_df$subbasin)
rotated <- subset(sum_df, duration < 204)
diff <- data.frame(abs(diff(as.matrix(rotated[, c('avg_yearly_gain', 'avg_biomass')]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
odd_diff <- cbind(odd_diff, sbasins)
odd_diff <- odd_diff[, c('sbasins', 'avg_biomass')]
odd_diff$difference_type <- 'schedule'

colnames(odd_diff) <- colnames(diff_df)
plot_df <- rbind(diff_df, odd_diff)

p <- ggplot(plot_df, aes(x=subbasin, y=diff, colour=difference_type))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + ylab('diff: avg monthly biomass (kg/ha)')
pngname <- paste(imgpath, "diff_avg_biomass_intervention_v_schedule.png", sep="/")
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

# how does difference between schedules vary by intervention?
rotated <- subset(sum_df, duration < 204)
ids <- unique(rotated[, c('animal_type', 'subbasin', 'sd_level')])
diff <- data.frame(abs(diff(as.matrix(rotated[, c('avg_yearly_gain', 'avg_biomass')]))))
odd_diff <- diff[seq(1, dim(diff)[1], 2), ]
odd_diff <- cbind(odd_diff, ids)
odd_diff$intervention <- paste(odd_diff$animal_type, odd_diff$sd_level, sep="-")
odd_diff$subbasin <- as.factor(odd_diff$subbasin)

p <- ggplot(odd_diff, aes(x=intervention, y=avg_biomass))
p <- p + geom_point(aes(colour=as.factor(subbasin)))
p <- p + ylab("diff avg monthly biomass between schedules")
pngname <- "C:/Users/Ginger/Desktop/diff_avg_biomass_by_intervention.png"
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

p <- ggplot(odd_diff, aes(x=intervention, y=avg_yearly_gain))
p <- p + geom_point(aes(colour=as.factor(subbasin)))
p <- p + ylab("diff avg yearly gain between schedules")
pngname <- "C:/Users/Ginger/Desktop/diff_avg_yearly_gain_by_intervention.png"
png(file=pngname, units="in", res=150, width=9, height=5)
print(p)
dev.off()

## how does average monthly gain differ between rotated and non-rotated schedules?
sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/rotation_high_sd/comparison_8.25.16.csv"
sum_df <- read.csv(sum_csv)

sum_df$id <- paste(sum_df$subbasin, sum_df$sd_level, sum_df$animal_type, sep=".")
rotated <- subset(sum_df, duration < 204)
full <- subset(sum_df, duration == 204)
monthly_gain_means <- aggregate(rotated$avg_gain_kg, by=list(rotated$id), FUN=mean)
colnames(monthly_gain_means) <- c('id', 'avg_monthly_gain')
sum_df_sub <- unique(rotated[, colnames(rotated)[c(1, 8, 9, 10)]])
monthly_gain_rotated <- merge(sum_df_sub, monthly_gain_means, by='id')
monthly_gain_rotated$schedule <- 'rotation'
full_sub <- full[, colnames(full)[c(10, 1, 8, 9, 3)]]
colnames(full_sub)[5] <- 'avg_monthly_gain'
full_sub$schedule <- 'full'
plot_df <- rbind(monthly_gain_rotated, full_sub)
levels(plot_df$sd_level) <- c("high density", "low density")

p <- ggplot(plot_df, aes(x=subbasin, y=avg_monthly_gain, colour=schedule))
p <- p + geom_point()
p <- p + facet_grid(animal_type~sd_level, scales='free')
pngname <- paste(imgpath, "avg_monthly_gain_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()

## avg monthly biomass
ids <- unique(sum_df[, c('subbasin', 'duration', 'sd_level', 'animal_type')])
sum_df$id <- paste(sum_df$subbasin, sum_df$duration, sum_df$sd_level, sum_df$animal_type, sep=".")
biomass_means <- aggregate(sum_df$avg_biomass, by=list(sum_df$id), FUN=mean)
colnames(biomass_means) <- c('id', 'avg_biomass')
sum_df_sub <- unique(sum_df[, colnames(sum_df)[c(1, 5, 8, 9, 10)]])
biomass_means <- merge(sum_df_sub, biomass_means, by='id')
biomass_means[which(biomass_means$duration == 4), 'schedule'] <- "rotation"
biomass_means[which(biomass_means$duration == 204), 'schedule'] <- "full"
biomass_means[which(biomass_means$sd_level == 'rechigh'), 'stocking_density'] <- "high"
biomass_means[which(biomass_means$sd_level == 'reclow'), 'stocking_density'] <- "low"

p <- ggplot(biomass_means, aes(x=subbasin, y=avg_biomass, colour=schedule))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + facet_grid(animal_type~stocking_density, scales='free')
p <- p + ylab("avg monthly biomass (kg/ha)")
pngname <- paste(imgpath, "avg_biomass_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()

## total yearly gain
marginal_table <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_table_8.25.16.csv"
marg_df <- read.csv(marginal_table)
marg_df$schedule <- "full"
marg_df[which(marg_df$density == 'ighrot'), 'schedule'] <- "rotation"
marg_df[which(marg_df$density == 'lowrot'), 'schedule'] <- "rotation"
marg_df$stocking_density <- marg_df$density
marg_df[which(marg_df$density == 'ighrot'), 'stocking_density'] <- "high"
marg_df[which(marg_df$density == 'lowrot'), 'stocking_density'] <- "low"

p <- ggplot(marg_df, aes(x=subbasin, y=total_delta_weight_kg, colour=schedule))
p <- p + geom_jitter(position = position_jitter(width=0.1))
p <- p + facet_grid(animal~stocking_density, scales='free')
p <- p + ylab("total yearly gain (kg)")
pngname <- paste(imgpath, "total_yearly_gain_rotated_vs_not_reclow.png", sep="/")
png(file=pngname, units="in", res=150, width=6, height=6)
print(p)
dev.off()
