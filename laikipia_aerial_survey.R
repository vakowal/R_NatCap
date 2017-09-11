# analyze aerial survey data from the Laikipia wildlife forum

# habitat data with our properties joined
hab_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/habitat_records_GK_inside_properties.csv")

# replace categories with mean of the % that they span
categories <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/cover_categories.csv")

hab_df$tree_cover <- 0
for(c in unique(hab_df$tree_)){
  med <- categories[categories$category == c, 'median']
  hab_df[hab_df$tree_ == c, 'tree_cover'] <- med
}

hab_df$shrub_cover <- 0
for(c in unique(hab_df$shrub_)){
  med <- categories[categories$category == c, 'median']
  hab_df[hab_df$shrub_ == c, 'shrub_cover'] <- med
}

hab_df$herb_cover <- 0
for(c in unique(hab_df$herb_)){
  med <- categories[categories$category == c, 'median']
  hab_df[hab_df$herb_ == c, 'herb_cover'] <- med
}

subs <- hab_df[, c('tree_cover', 'shrub_cover', 'herb_cover',
                   'property')]
sum_tree <- aggregate(tree_cover~property, data=subs, FUN=mean)
sum_shrub <- aggregate(shrub_cover~property, data=subs, FUN=mean)
sum_herb <- aggregate(herb_cover~property, data=subs, FUN=mean)
sum_n <- aggregate(herb_cover~property, data=subs, FUN=length)
colnames(sum_n)[2] <- 'num_points'
sum_df <- merge(sum_tree, sum_n)
sum_df <- merge(sum_df, sum_shrub)
sum_df <- merge(sum_df, sum_herb)

save_as <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/habitat_summary.csv"
write.csv(sum_df, save_as, row.names=FALSE)

# relationship of habitat from aerial survey with wildlife density from dung
sum_df <- read.csv(save_as)
dung_sum_csv <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/From_Sharon/Processed_by_Ginger/regional_dung_2015_property_means.csv"
property_means <- read.csv(dung_sum_csv)
group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key, stringsAsFactors=FALSE)

means_t <- as.data.frame(t(property_means[2:24]))
colnames(means_t) <- property_means$Property
means_t$Abbrev <- rownames(means_t)
comb <- merge(means_t, gr_key_df, by='Abbrev')
gr1_means <- aggregate(comb[, 2:25], by=list(comb$Group11), FUN=sum)
gr1_res <- as.data.frame(t(gr1_means[, 2:25]))
colnames(gr1_res) <- gr1_means$Group.1
gr1_res$Property <- row.names(gr1_res)

habitat_anim_by_property <- merge(sum_df, gr1_res, by="Property")

sum_df <- data.frame('dung_type'=character(), 'habitat_type'=character(),
                     'pearson_cor'=numeric(), 'pearson_pval'=numeric(),
                     stringsAsFactors=FALSE)
i <- 1
img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/cor_animal_dung_habitat"
for(anim in colnames(habitat_anim_by_property)[c(6:10, 12)]){
  for(hab in c('tree_cover', 'shrub_cover', 'herb_cover')){
    p <- ggplot(habitat_anim_by_property, aes_string(x=hab, y=anim))
    p <- p + geom_point()
    pngname <- paste(img_dir, paste(anim, "_v_", hab, ".png", sep=""), sep="/")
    png(file=pngname, units="in", res=300, width=4, height=3.5)
    print(p)
    dev.off()
    pe_test <- cor.test(habitat_anim_by_property[, anim],
                        habitat_anim_by_property[, hab], method="pearson")
    sum_df[i, 'dung_type'] <- anim
    sum_df[i, 'habitat_type'] <- hab
    sum_df[i, 'pearson_cor'] <- pe_test[[4]]
    sum_df[i, 'pearson_pval'] <- pe_test[[3]]
    i <- i + 1
  }
}
write.csv(sum_df, paste(img_dir, 'correlation_summary.csv', sep='/'), row.names=FALSE)

####
sum_df <- data.frame('group'=character(), 'spearman_rho'=numeric(),
                     'spearman_pval'=numeric(), 'pearson_cor'=numeric(),
                     'pearson_pval'=numeric(), stringsAsFactors=FALSE)
i <- 1
dung_join_df <- g1_df
gr <- 'grazer'
for(gr in unique(dung_join_df$group)){
  sub_df <- dung_join_df[which(dung_join_df$group == gr), ]
  joined <- merge(sub_df, c_df, by="site")
  p <- ggplot(joined, aes(x=total_rem, y=mean_dung))
  p <- p + geom_point()
  p <- p + xlab("Modeled grazing intensity") + ylab(paste(gr, " dung per transect", sep=""))
  p <- p + ylab("Grazer (ex. bovid) dung per transect")
  p <- p + print_theme
  pngname <- paste(fig_dir, paste(gr, "ex_bovid_x_back_calc.png", sep=""), sep="/")
  png(file=pngname, units="in", res=300, width=5, height=5)
  print(p)
  dev.off()
  sp_test <- cor.test(joined$mean_dung, joined$total_rem, method="spearman")
  pe_test <- cor.test(joined$mean_dung, joined$total_rem, method="pearson")
  sum_df[i, 'group'] <- gr
  sum_df[i, 'spearman_rho'] <- sp_test[[4]]
  sum_df[i, 'spearman_pval'] <- sp_test[[3]]
  sum_df[i, 'pearson_cor'] <- pe_test[[4]]
  sum_df[i, 'pearson_pval'] <- pe_test[[3]]
  i <- i + 1
}
save_as <- paste(result_dir, "correlation_summary.csv", sep="/")
write.csv(sum_df, save_as, row.names=FALSE)
####
# association of different wildlife types with habitat data, both from aerial survey
hab_anim_df <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/habitat_animal_joined.csv",
                        stringsAsFactors=FALSE)
hab_anim_df <- hab_anim_df[(hab_anim_df$Species_na != 'Cattle Free Ranging') &
                             (hab_anim_df$Species_na != 'Cattle Paddock') &
                             (hab_anim_df$Species_na != 'Cattle Tethered') &
                             (hab_anim_df$Species_na != 'Elephant Bones Grey') &
                             (hab_anim_df$Species_na != 'Elephant Bones white in color') &
                             (hab_anim_df$Species_na != 'Gerenuk') &
                             (hab_anim_df$Species_na != 'Lesser Kudu'), ]
hab_anim_df[hab_anim_df$Species_na == 'Giraffe Reticulated', 'Species_na'] <- 'Giraffe'
hab_anim_df[hab_anim_df$Species_na == "Grevy's zebra", 'Species_na'] <- 'zebra'
hab_anim_df[hab_anim_df$Species_na == "Burchell's zebra", 'Species_na'] <- 'zebra'

categories <- read.csv("C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/cover_categories.csv")
hab_anim_df$tree_cover <- 0
for(c in unique(hab_anim_df$tree_)){
  med <- categories[categories$category == c, 'median']
  hab_anim_df[hab_anim_df$tree_ == c, 'tree_cover'] <- med
}
hab_anim_df$shrub_cover <- 0
for(c in unique(hab_anim_df$shrub_)){
  med <- categories[categories$category == c, 'median']
  hab_anim_df[hab_anim_df$shrub_ == c, 'shrub_cover'] <- med
}
hab_anim_df$herb_cover <- 0
for(c in unique(hab_anim_df$herb_)){
  med <- categories[categories$category == c, 'median']
  hab_anim_df[hab_anim_df$herb_ == c, 'herb_cover'] <- med
}
subs <- hab_anim_df[, c('tree_cover', 'shrub_cover', 'herb_cover',
                        'TARGET_FID', 'Species_na', 'GridPopula')]

# translate animal types between LWF's scheme and mine
group_key <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Forage_model/Kenya_ticks_project_specific/wildlife_group_definition.csv"
gr_key_df <- read.csv(group_key)
atest <- setdiff(gr_key_df$LWF_class, unique(hab_anim_df$Species_na))
atest <- setdiff(unique(hab_anim_df$Species_na), gr_key_df$LWF_class)
gr_key_df <- gr_key_df[, c('Group11', 'LWF_class')]
hab_anim <- merge(subs, gr_key_df, by.x='Species_na', by.y='LWF_class')

# sum animal counts by my diet type classifications
sum_by_type <- aggregate(GridPopula~Group11+TARGET_FID, data=hab_anim, FUN=sum)
sum_res <- reshape(sum_by_type, idvar='TARGET_FID', timevar='Group11', direction="wide")
colnames(sum_res) <- gsub('GridPopula.', "", colnames(sum_res))

habitat <- unique(hab_anim_df[, c('tree_cover', 'shrub_cover', 'herb_cover',
                                'TARGET_FID')])
hab_anim_merge <- merge(habitat, sum_res)

img_dir <- "C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/Data/Kenya/Laikipia_aerial_survey/summary_GK/cor_animal_type_habitat"
for(anim in unique(sum_by_type$Group11)){
  for(hab in c('tree_cover', 'shrub_cover', 'herb_cover')){
    p <- ggplot(hab_anim_merge, aes_string(x=hab, y=anim))
    p <- p + geom_jitter(width=0.8)
    pngname <- paste(img_dir, paste(anim, "_v_", hab, ".png", sep=""), sep="/")
    png(file=pngname, units="in", res=300, width=4, height=3.5)
    print(p)
    dev.off()
  }
}
