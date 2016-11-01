## processing net gain data from Peru household survey
library(ggplot2)

gain_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/Net_gain_10.4.16.csv"
weight_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/Animal_weight_10.5.16.csv"

gain_df <- read.csv(gain_csv, skip=1)
weight_df <- read.csv(weight_csv)
weight_means <- colMeans(weight_df, na.rm=TRUE)

combined <- merge(gain_df, weight_df, by='id_hogar')
sheepm <- lm(ganancia_neta_ovinos~Sheep..kg.per.animal.sold., combined)
summary(sheepm)

cheesem <- lm(queso_ganancia_unitaria~Cows..kg.per.animal.sold., combined)
summary(cheesem)
sheepwoolm <- lm(lana_ganancia_unitaria~Sheep..kg.per.animal.sold., combined)
summary(sheepwoolm)
alpacawoolm <- lm(fibra_alpaca_ganancia_unitaria~Alpacas..kg.per.animal.sold., combined)
summary(alpacawoolm)

sheeptotallm <- lm(ganancia_neta_ovinos_procesados~Sheep..kg.per.animal.sold., combined)
cattletotallm <- lm(ganancia_neta_vacunos_procesados~Cows..kg.per.animal.sold., combined)
alpacatotallm <- lm(ganancia_neta_alpacas_procesados~Alpacas..kg.per.animal.sold., combined)
summary(sheeptotallm)
summary(cattletotallm)
summary(alpacatotallm)

p <- ggplot(combined, aes(x=Cows..kg.per.animal.sold., y=queso_ganancia_unitaria))
p <- p + geom_point()
p <- p + geom_abline(intercept=-213.179, slope=1.6753)
print(p)
pngname <- paste(imgdir, "Cow_cheese.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

bulldf <- combined[, c('Bulls..kg.per.animal.sold.', 'ganancia_neta_vacunos')]
cowdf <- combined[, c('Cows..kg.per.animal.sold.', 'ganancia_neta_vacunos')]
colnames(bulldf)[1] <- 'cattle_weight_kg'
colnames(cowdf)[1] <- 'cattle_weight_kg'
cattledf <- rbind(bulldf, cowdf)
cattlem <- lm(ganancia_neta_vacunos~cattle_weight_kg, cattledf)
summary(cattlem)

alpacadf <- combined[, c('Alpacas..kg.per.animal.sold.', 'ganancia_neta_alpacas')]
llamadf <- combined[, c('Llamas..kg.per.animal.sold.', 'ganancia_neta_llamas')]
colnames(alpacadf) <- c('camelid_weight_kg', 'ganancia_neta')
colnames(llamadf) <- c('camelid_weight_kg', 'ganancia_neta')
cameliddf <- rbind(alpacadf, llamadf)
camelidm <- lm(ganancia_neta~camelid_weight_kg, cameliddf)
summary(camelidm)

imgdir <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Household_survey_livestock_portion/net_gain_figs"

p <- ggplot(combined, aes(x=Sheep..kg.per.animal.sold., y=ganancia_neta_ovinos))
p <- p + geom_point()
p <- p + geom_abline(intercept=98.19, slope=1.13)
print(p)
pngname <- paste(imgdir, "Sheep.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(cattledf, aes(x=cattle_weight_kg, y=ganancia_neta_vacunos))
p <- p + geom_point()
p <- p + geom_abline(intercept=736.5832, slope=0.8924)
print(p)
pngname <- paste(imgdir, "Cattle.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(cameliddf, aes(x=camelid_weight_kg, y=ganancia_neta))
p <- p + geom_point()
p <- p + geom_abline(intercept=172.7764, slope=0.6386)
print(p)
pngname <- paste(imgdir, "Camelids.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()


#### marginal values measured by economic gain vs strictly kg gained
imgpath <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_value_comparisons_10.18.16"
marg_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/Forage_model_results/marginal_table_10.18.16.csv"
marg_df <- read.csv(marg_csv)
marg_df$intervention <- paste(marg_df$animal, marg_df$density, sep="*")
marg_df$subbasin <- as.factor(marg_df$subbasin)

p <- ggplot(marg_df, aes(x=intervention, y=total_delta_weight_kg, group=subbasin))
p <- p + geom_point(aes(colour=subbasin)) + ggtitle("Delta weight")
print(p)
pngname <- paste(imgpath, "Delta_weight_by_subbasin.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(marg_df, aes(x=intervention, y=total_delta_weight_kg))
p <- p + geom_boxplot() + ggtitle("Delta weight")
print(p)
pngname <- paste(imgpath, "Delta_weight_boxplot.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(marg_df, aes(x=intervention, y=delta.soles, group=subbasin))
p <- p + geom_point(aes(colour=subbasin)) + ggtitle("Delta soles")
print(p)
pngname <- paste(imgpath, "Delta_soles_by_subbasin.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

p <- ggplot(marg_df, aes(x=intervention, y=delta.soles))
p <- p + geom_boxplot() + ggtitle("Delta soles")
print(p)
pngname <- paste(imgpath, "Delta_soles_boxplot.png", sep="/")
png(file=pngname, units="in", res=300, width=5, height=4)
print(p)
dev.off()

