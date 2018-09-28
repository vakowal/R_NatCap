# plot regional productivity energy balance for Kenya MS

library(ggplot2)
# multiple densities, multiple conception steps
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/energy_balance_summary.csv")
# ensure that management threshold works as I expect it to
min_biomass <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/min_biomass_summary.csv")
p <- ggplot(min_biomass, aes(x=site, y=min_biomass))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
print(p)

# aggregated metric: # months during the simulation where diet was insufficient
# to meet needs of the herd
sum_df <- sum_df[sum_df$step >= 0,
                 c('conception_step', 'density', 'site', 'herd_avg_diet_sufficiency')]
sufficiency_threshold <- 0  # threshold under which we consider the animals' dietary needs not met
fun <- function(diet_sufficiency_values, sufficiency_threshold){
  count_insufficient <- length(diet_sufficiency_values[diet_sufficiency_values < sufficiency_threshold])
  return(count_insufficient)
}
num_months_insufficient <- aggregate(herd_avg_diet_sufficiency ~ conception_step + density + site,
                                     data=sum_df, FUN=fun, sufficiency_threshold=sufficiency_threshold)
colnames(num_months_insufficient)[4] <- 'num_months_insufficient'
p <- ggplot(num_months_insufficient, aes(x=site, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
p <- p + ylab("# months with insufficient diet")
print(p)

# how sensitive is # months insufficient to timing of conception month?
subs <- num_months_insufficient[num_months_insufficient$density > 0.4, ]
p <- ggplot(subs, aes(x=site, y=num_months_insufficient))
p <- p + geom_jitter(width=0.2, height=0)
p <- p + facet_wrap(~density)
p <- p + ylab("# months with insufficient diet")
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_density.png"
png(file=pngname, units="in", res=300, width=8, height=3)
print(p)
dev.off()

# diet insufficiency vs rainfall
precip_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Climate/regional_average_annual_precip_centroid.csv")
precip_suff <- merge(subs, precip_df, by.x="site", by.y="FID")
p <- ggplot(precip_suff, aes(x=avg_annual_rainfall_mm, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_grid(conception_step~density)
p <- p + ylab("# months with insufficient diet")
p <- p + xlab("Average annual rainfall (mm)")
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/energy_balance/num_months_insufficient_x_rainfall.png"
png(file=pngname, units="in", res=300, width=4, height=7)
print(p)
dev.off()

## precip perturbations
pert_df <- read.csv("C:/Users/ginge/Documents/NatCap/model_inputs_Kenya/regional_precip_perturbations/precip_perturbations.csv")
p <- ggplot(pert_df, aes(x=pci_intended_perc_change, y=pci))
p <- p + geom_point(aes(colour=site))
p <- p + facet_wrap(~totp_perc_change)
print(p)

# diet insufficiency with precip perturbations
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/energy_balance_summary.csv")
sum_df <- sum_df[sum_df$step >= 0, colnames(sum_df)[2:6]]
sufficiency_threshold <- 0  # threshold under which we consider the animals' dietary needs not met
fun <- function(diet_sufficiency_values, sufficiency_threshold){
  count_insufficient <- length(diet_sufficiency_values[diet_sufficiency_values < sufficiency_threshold])
  return(count_insufficient)
}
num_months_insufficient <- aggregate(herd_avg_diet_sufficiency ~ PCI_perc_change + total_precip_perc_change + site,
                                     data=sum_df, FUN=fun, sufficiency_threshold=sufficiency_threshold)
colnames(num_months_insufficient)[4] <- 'num_months_insufficient'

perturb_df <- read.csv("C:/Users/ginge/Documents/NatCap/model_inputs_Kenya/regional_precip_perturbations/precip_perturbations.csv")
perturb_df <- perturb_df[, colnames(perturb_df)[2:6]]
colnames(perturb_df) <- c('PCI', 'PCI_perc_change', 'site', 'tot_precip', 'total_precip_perc_change')

sum_perturb_df <- merge(num_months_insufficient, perturb_df)
duplicates <- sum_perturb_df[duplicated(sum_perturb_df), ]  # there are no duplicated rows
sum_perturb_df$PCI <- sum_perturb_df$PCI *100
sum_perturb_df$precip_bin <- cut(sum_perturb_df$tot_precip, breaks=12)

p <- ggplot(sum_perturb_df, aes(x=PCI, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_wrap(~precip_bin)
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_v_PCI.png"
png(file=pngname, units="in", res=300, width=6, height=6)
print(p)
dev.off()

p <- ggplot(sum_perturb_df, aes(x=PCI_perc_change, y=num_months_insufficient))
p <- p + geom_point()
p <- p + facet_wrap(~total_precip_perc_change)
print(p)

p <- ggplot(sum_perturb_df, aes(x=tot_precip, y=num_months_insufficient))
p <- p + geom_point()
print(p)

## max viable density with precip perturb
library(ggplot2)
sum_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/Forage_model/model_results/regional_properties/precip_perturbations/num_months_insufficient_summary_2018-09-28--15_59_27.csv")
subs_1or2 <- sum_df[(sum_df$num_months_insufficient == 1) | (sum_df$num_months_insufficient == 2), ]

perturb_df <- read.csv("C:/Users/ginge/Documents/NatCap/model_inputs_Kenya/regional_precip_perturbations/precip_perturbations.csv")
perturb_df <- perturb_df[, colnames(perturb_df)[2:6]]
colnames(perturb_df) <- c('PCI', 'PCI_perc_change', 'site', 'tot_precip', 'total_precip_perc_change')

sum_perturb_df <- merge(subs_1or2, perturb_df)
duplicates <- sum_perturb_df[duplicated(sum_perturb_df), ]  # there are no duplicated rows
sum_perturb_df$PCI <- sum_perturb_df$PCI *100
sum_perturb_df$precip_bin <- cut(sum_perturb_df$tot_precip, breaks=6)
sum_perturb_df$PCI_bin <- cut(sum_perturb_df$PCI, breaks=6)

p <- ggplot(sum_perturb_df, aes(x=tot_precip, y=density))
p <- p + geom_point()
p <- p + facet_wrap(~PCI_bin)
print(p)

p <- ggplot(sum_perturb_df, aes(x=PCI, y=density))
p <- p + geom_point()
p <- p + facet_wrap(~precip_bin)
print(p)

model <- lm(density~PCI*tot_precip + site, data=sum_perturb_df)
summary(model)
