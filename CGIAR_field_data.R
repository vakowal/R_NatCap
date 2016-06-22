# PDM records: Peru field data

library(ggplot2)

norm <- function(vec){
  normalized = (vec - min(vec)) / (max(vec) - min(vec))
  return(normalized)
}

PDM_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/PDM_records.csv"
PDM_df <- read.csv(PDM_file)
biomass_file <- "C:/Users/Ginger/Dropbox/NatCap_backup/CGIAR/Peru/field_data/clipped_biomass_g.csv"
biomass_df <- read.csv(biomass_file)

p <- ggplot(PDM_df, aes(factor(Polygon_id), PDM))
p <- p + geom_boxplot()
print(p)

PDM_means <- aggregate(PDM_df$PDM, by=list(id=PDM_df$Polygon_id), FUN=mean, na.rm=TRUE)
PDM_plot_df <- data.frame('Polygon_id'=PDM_means$id, 'norm_value'=norm(PDM_means$x),
                          'label'=rep('PDM', length(PDM_means$id)))
biomass_plot_df <- data.frame('Polygon_id'=biomass_df$Polygon_id,
                              'norm_value'=norm(biomass_df$g),
                              'label'=rep('clipped_biomass', length(biomass_df$Polygon_id)))
plot_df <- rbind(biomass_plot_df, PDM_plot_df)

p <- ggplot(plot_df, aes(x=Polygon_id, y=norm_value, group=label, linetype=label))
p <- p + geom_line()
p <- p + geom_point(aes(colour=label))
print(p)

norm_cor <- cor.test(PDM_norm$x, biomass_norm, method="spearman")




