# plot PFTs in Mongolia
library(ggplot2)

pft_summary <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/PFT/PFT_MODIS_VCF_CLM.csv")
colnames(pft_summary)[1] <- 'PFT'
pft_summary[pft_summary$PFT == "bareground", "PFT"] <- 'bare'

figdir <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/data/PFT/figs"
modis_df <- pft_summary[pft_summary$source == 'MODIS_VCF', ]
p <- ggplot(modis_df, aes(x=NAME, y=MEAN, group=PFT))
p <- p + geom_point(aes(shape=PFT))
p <- p + xlab("Soum") + ylab("Mean percent cover")
print(p)
pngname <- paste(figdir, 'MODIS_VCF.png', sep='/')
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()

clm_df <- pft_summary[pft_summary$source == 'CLM', ]
clm_df <- clm_df[clm_df$PFT != "c4 grass", ]
p <- ggplot(clm_df, aes(x=NAME, y=MEAN, group=PFT))
p <- p + geom_point(aes(shape=PFT))
p <- p + xlab("Soum") + ylab("Mean percent cover")
p <- p + theme(legend.position="bottom")
print(p)
pngname <- paste(figdir, 'CLM_with_bareground.png', sep='/')
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()

clm_df <- clm_df[clm_df$PFT != "bare", ]
p <- ggplot(clm_df, aes(x=NAME, y=MEAN, group=PFT))
p <- p + geom_point(aes(shape=PFT))
p <- p + xlab("Soum") + ylab("Mean percent cover")
p <- p + theme(legend.position="bottom")
print(p)
pngname <- paste(figdir, 'CLM_ex_bareground.png', sep='/')
png(file=pngname, units="in", res=300, width=5, height=3)
print(p)
dev.off()
