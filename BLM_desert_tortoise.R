# habitat analyses for desert tortoise including native and introduced vegetation types from Landfire EVT

library(raster)
library(ggplot2)

# evt introduced classes
evt_key <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Landfire/US_140EVT_04252017/CSV_Data/LF_140EVT_09152016.csv")

# evt, annual potential, and perennial cover at Nussear's coarse resolution
evt_nussear_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/landfire_evt_res_to_nussear.tif"
annual_nussear_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/Nussear_2009/ofr20091102 Environmental Layers/annProx.asc"
perennial_nussear_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/Nussear_2009/ofr20091102 Environmental Layers/pctCov.asc"

# load
evt <- raster(evt_nussear_res_file)
Nussear <- list()
Nussear[['annual_potential']] <- raster(annual_nussear_res_file)
Nussear[['perennial_cover']] <- raster(perennial_nussear_res_file)

# introduced LULC classes only
evt_key_subs <- evt_key[evt_key$VALUE %in% unique(values(evt)), ]
evt_introduced_classes <- evt_key_subs[grep("^Introduced", evt_key_subs$CLASSNAME), ]

# random permutations: how distinct are subsets identified by overlap with introduced LULCs?
n_test <- 100
df_list <- list()
listidx <- 1
for(n_idx in c('annual_potential', 'perennial_cover')){
  nussear_layer <- Nussear[[n_idx]]
  overall_mean <- mean(values(nussear_layer), na.rm=TRUE)  # mean value of the entire distribution
  for(r in (1:dim(evt_introduced_classes)[1])){
    class_name <- evt_introduced_classes[r, 'CLASSNAME']
    value <- evt_introduced_classes[r, 'VALUE']
    evt_introduced <- evt
    evt_introduced[evt_introduced != value] <- NA
    
    nuss_subset = nussear_layer
    nuss_subset[evt != value] <- NA
    
    sample_mean <- mean(values(nuss_subset), na.rm=TRUE)  # mean value of overlap area
    sample_diff <- abs(sample_mean - overall_mean)
    n_sig <- 0
    for(test_idx in 1:n_test){
      random_sample <- sampleRandom(nussear_layer,
                                    size=length(c(values(evt_introduced))))
      perm_mean <- mean(random_sample, na.rm=TRUE)  # mean value of random sample
      perm_meandiff <- abs(perm_mean - overall_mean)
      if(perm_meandiff > sample_diff){
        n_sig <- n_sig + 1
      }
    }
    df_list[[listidx]] <- data.frame('Nussear_variable'=n_idx,
                                     'EVT_LULC_class'=class_name,
                                     'number_random_samples_exceeding'=n_sig,
                                     'number_random_samples_tested'=n_test)
    listidx = listidx + 1
  }
}
summary_df <- do.call(rbind, df_list)

# box plots: visualize Nussear variables over entire area,
# vs inside areas overlapping introduced LULC
save_dir <- "C:/Users/ginge/Dropbox/NatCap_backup/BLM/desert_tortoise/evt_vs_nussear/Nussear_coarse"
for(n_idx in c('annual_potential', 'perennial_cover')){
  nussear_layer <- Nussear[[n_idx]]
  for(r in (1:dim(evt_introduced_classes)[1])){
    class_name <- evt_introduced_classes[r, 'CLASSNAME']
    value <- evt_introduced_classes[r, 'VALUE']
    evt_introduced <- evt
    evt_introduced[evt_introduced != value] <- NA
    
    nuss_subset = nussear_layer
    nuss_subset[evt != value] <- NA
    whole_area <- data.frame('Nussear_value'=c(values(nussear_layer)),
                             'area'=(rep('entire area', length(c(values(nussear_layer))))))
    subset <- data.frame('Nussear_value'=c(values(nuss_subset)),
                         'area'=(rep(class_name, length(c(values(nuss_subset))))))
    df <- rbind(whole_area, subset)
    p <- ggplot(df, aes(x=area, y=Nussear_value))
    p <- p + geom_boxplot()
    p <- p + ggtitle(n_idx)
    pngname <- paste(save_dir,
                     "/", paste(n_idx, "_v_", class_name, ".png", sep=""),
                     sep="")
    png(file=pngname, units="in", res=300, width=5, height=4)
    print(p)
    dev.off()
  }
}

# reclassify EVT for inclusion in Maxent modeling
evt_key <- read.csv("C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Landfire/US_140EVT_04252017/CSV_Data/LF_140EVT_09152016.csv")
evt_nussear_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/landfire_evt_res_to_nussear.tif"
evt <- raster(evt_nussear_res_file)
evt_key_subs <- evt_key[evt_key$VALUE %in% unique(values(evt)), ]
evt_introduced_classes <- evt_key_subs[grep("^Introduced", evt_key_subs$CLASSNAME), 'VALUE']
evt_native_classes <- setdiff(evt_key_subs$VALUE, evt_introduced_classes)
evt_introduced_only <- evt
for(n_class in evt_native_classes){
  evt_introduced_only[evt_introduced_only == n_class] <- 0
}
evt_introduced_combined <- evt_introduced_only
for(value in evt_introduced_classes){
  evt_introduced_combined[evt_introduced_combined == value] <- 1
}

save_dir <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/maxent_predictors"
writeRaster(evt_introduced_only,
            filename=paste(save_dir, "evt_introduced_classes_only.asc", sep="/"),
            format='ascii', prj=TRUE)
writeRaster(evt_introduced_combined,
            filename=paste(save_dir, "evt_introduced_classes_combined.asc", sep="/"),
            format='ascii', prj=TRUE)

# point to raster: tortoise observations
torts_shp <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/tort_obs_combined_proj.shp"
torts_pt <- shapefile(torts_shp)
evt_nussear_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/landfire_evt_res_to_nussear.tif"
template_raster <- raster(evt_nussear_res_file)
tort_ras <- rasterize(torts_pt, template_raster, field=1)
tort_ras_nodata <- reclassify(tort_ras, cbind(NA, 0))
tort_ras_nodata_masked <- mask(tort_ras_nodata, template_raster)
writeRaster(tort_ras_nodata_masked,
            filename="C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/tort_presence.asc",
            format='ascii', prj=TRUE, overwrite=TRUE)

# random pixels inside area without presence
tort_absence <- reclassify(tort_ras_nodata_masked, cbind(1, NA))
random_background_points <- sampleRandom(tort_absence, 7138, cells=TRUE)
tort_absence[random_background_points[, 1]] <- 1
background_points <- reclassify(tort_absence, cbind(0, NA))
writeRaster(background_points,
            filename="C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/tort_background_points.asc",
            format='ascii', prj=TRUE, overwrite=TRUE)

# EVT's fine resolution
evt_evt_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/landfire_evt.tif"
annual_evt_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/annProx_res_to_evt.tif"
perennial_evt_res_file <- "C:/Users/ginge/Documents/NatCap/GIS_local/BLM/Mojave/desert_tortoise/native_introduced_habitat/pctCov_res_to_evt.tif"

# load
evt <- raster(evt_evt_res_file)
Nussear <- list()
Nussear[['annual_potential']] <- raster(annual_evt_res_file)
Nussear[['perennial_cover']] <- raster(perennial_evt_res_file)

# introduced LULC classes only
evt_key_subs <- evt_key[evt_key$VALUE %in% unique(values(evt)), ]
evt_introduced_classes <- evt_key_subs[grep("^Introduced", evt_key_subs$CLASSNAME), ]

# boxplots at fine resolution don't work - too large for memory
