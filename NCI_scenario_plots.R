# exploring predicted cancer cases by scenario
# 5.22.20

# cancer cases by scenario by country, masked by confidence intervals
cases_masked_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/NCI_NDR/Results_5.15.20/endpoints/cancer_cases_by_country.csv")

# cancer cases by scenario by country, not masked
cases_df <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/NCI_NDR/Results_5.15.20/endpoints_not_masked/cancer_cases_by_country.csv")

abs_change_df <- data.frame('ISO3'=cases_df$ISO3)
abs_change_df$delta_cancer_baseline_to_potential_masked <- (
  cases_masked_df$global_potential_vegetation_ - cases_masked_df$fixedarea_currentpractices)
abs_change_df$delta_cancer_baseline_to_potential <- (
  cases_df$global_potential_vegetation_ - cases_df$fixedarea_currentpractices)
abs_change_df$delta_cancer_baseline_to_extens_intens_irr_masked <- (
  cases_masked_df$extensification_intensified_irrigated_ - cases_masked_df$fixedarea_currentpractices)
abs_change_df$delta_cancer_baseline_to_extens_intens_irr <- (
  cases_df$extensification_intensified_irrigated_ - cases_df$fixedarea_currentpractices)
write.csv(abs_change_df, "C:/Users/ginge/Dropbox/NatCap_backup/NCI WB/Analysis_results/Updated_NDR_5.18.20/absolute_change_from_baseline.csv")

perc_change_df <- data.frame('ISO3'=cases_df$ISO3)
perc_change_df$delta_cancer_baseline_to_potential_masked <- (
  (cases_masked_df$fixedarea_currentpractices - cases_masked_df$global_potential_vegetation_) / 
  cases_masked_df$fixedarea_currentpractices) * 100
perc_change_df$delta_cancer_baseline_to_potential <- (
  (cases_df$fixedarea_currentpractices - cases_df$global_potential_vegetation_) / 
    cases_df$fixedarea_currentpractices) * 100
perc_change_df$delta_cancer_baseline_to_extens_intens_irr_masked <- (
  (cases_masked_df$fixedarea_currentpractices - cases_masked_df$extensification_intensified_irrigated_) /
    cases_masked_df$fixedarea_currentpractices) * 100
perc_change_df$delta_cancer_baseline_to_extens_intens_irr <- (
  (cases_df$fixedarea_currentpractices - cases_df$extensification_intensified_irrigated_) /
    cases_df$fixedarea_currentpractices) * 100
write.csv(perc_change_df, "C:/Users/ginge/Dropbox/NatCap_backup/NCI WB/Analysis_results/Updated_NDR_5.18.20/perc_change_from_baseline.csv")

library(ggplot2)
p <- ggplot(summary_df, aes(x=delta_cancer_baseline_to_potential))
p <- p + geom_histogram() + ggtitle("Not masked")
p <- p + xlab("Percent change, cancer cases: baseline to potential veg")
print(p)

p <- ggplot(summary_df, aes(x=delta_cancer_baseline_to_potential_masked))
p <- p + geom_histogram() + ggtitle("Masked by confidence interval")
p <- p + xlab("Percent change, cancer cases: baseline to potential veg")
print(p)

# find largest spread among scenarios by country
summary_df <- data.frame('ISO3'=cases_df$ISO3)
summary_df$perc_change_potentialveg_to_extensintensirrig <- (
  (cases_df$extensification_intensified_irrigated_ - cases_df$global_potential_vegetation_) / 
    cases_df$global_potential_vegetation_) * 100

scenario_df <- subset(cases_df, select=-c(ISO3))
summary_df$min_cancer_cases <- do.call(pmin, scenario_df)
summary_df$max_cancer_cases <- do.call(pmax, scenario_df)
summary_df$perc_change_minscenario_to_maxscenario <- (
  (summary_df$max_cancer_cases - summary_df$min_cancer_cases) /
    summary_df$min_cancer_cases) * 100
library(ggplot2)
p <- ggplot(summary_df, aes(x=perc_change_minscenario_to_maxscenario, y=perc_change_potentialveg_to_extensintensirrig))
p <- p + geom_point() + geom_abline(slope=1, intercept=0)
print(p)

summary_df <- summary_df[, c('ISO3', 'perc_change_potentialveg_to_extensintensirrig', 'perc_change_minscenario_to_maxscenario')]
write.csv(summary_df, "C:/Users/ginge/Dropbox/NatCap_backup/NCI WB/Analysis_results/Updated_NDR_5.18.20/percent_change_cancer_by_country_unmasked.csv",
          row.names=FALSE)
