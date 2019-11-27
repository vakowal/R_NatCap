nelem_1 <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/pycentury_dev/1_nelem1.lis"
nelem_2 <- "C:/Users/ginge/Dropbox/NatCap_backup/Mongolia/model_results/pycentury_dev/1/CENTURY_outputs_m12_y2016/1.lis"

origdir <- 'C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Century46_PC_Jan-2014' 
outvars <- read.table(paste(origdir, '/outvars.txt', sep = ""), header = FALSE)
outvars$V1 <- gsub("\\(", ".", outvars$V1)
outvars$V1 <- gsub("\\,", ".", outvars$V1)
outvars$V1 <- gsub("\\)", "", outvars$V1)
widths <- rep(16, length(outvars[, 1]) + 1)

nelem_1_df <- read.fwf(nelem_1, widths, skip=2)
nelem_2_df <- read.fwf(nelem_2, widths, skip=2)
cnames <-  c('date', as.character(outvars$V1))
colnames(nelem_1_df) <- cnames
colnames(nelem_2_df) <- cnames

nelem_1_df$nelem <- 1
nelem_2_df$nelem <- 2
sum_df <- rbind(nelem_1_df, nelem_2_df)
sum_df <- sum_df[sum_df$date > 2000, ]
library(ggplot2)

sum_df$nelem <- as.factor(sum_df$nelem)
p <- ggplot(sum_df, aes(x=date, y=aglivc, group=nelem, linetype=nelem))
p <- p + geom_line()
print(p)

p <- ggplot(sum_df, aes(x=date, y=minerl.1.1, group=nelem, linetype=nelem))
p <- p + geom_line()
print(p)

###################
# diff between two models, one decomposition time step
delta_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/delta_sv_decomp_one_time_step.csv")
old_t0_df <- delta_df[delta_df$source == "old_century_before",
                    c('state_var', 'value')]
colnames(old_t0_df) <- c('state_var', 'old_century_before')
old_t1_df <- delta_df[delta_df$source == "old_century_after",
                      c('state_var', 'value')]
colnames(old_t1_df) <- c('state_var', 'old_century_after')
new_t1_df <- delta_df[delta_df$source == "new_after",
                      c('state_var', 'value')]
colnames(new_t1_df) <- c('state_var', 'new_after')
wide_df <- merge(old_t0_df, old_t1_df)
wide_df <- merge(wide_df, new_t1_df)
wide_df$delta_old_model <- wide_df$old_century_after - wide_df$old_century_before
wide_df$delta_new_model <- wide_df$new_after - wide_df$old_century_before

library(ggplot2)
p <- ggplot(wide_df, aes(x=delta_old_model, y=delta_new_model))
p <- p + geom_point()
p <- p + xlab("delta state variable: Century") + ylab("delta state variable: pyCentury")
p <- p + geom_abline(slope=1, intercept=0, linetype=3)
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/delta_new_v_old_century.png"
png(file=pngname, units="in", res=300, width=3, height=2.5)
print(p)
dev.off()

## diff: one entire monthly time step
# diff between two models, one decomposition time step
delta_df <- read.csv("C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/delta_sv_decomp_one_monthly_time_step.csv")
old_t0_df <- delta_df[delta_df$source == "old_century_before",
                      c('state_var', 'value')]
colnames(old_t0_df) <- c('state_var', 'old_century_before')
old_t1_df <- delta_df[delta_df$source == "old_century_after",
                      c('state_var', 'value')]
colnames(old_t1_df) <- c('state_var', 'old_century_after')
new_t0_df <- delta_df[delta_df$source == 'new_before',
                      c('state_var', 'value')]
colnames(new_t0_df) <- c('state_var', 'new_before')
new_t1_df <- delta_df[delta_df$source == "new_after",
                      c('state_var', 'value')]
colnames(new_t1_df) <- c('state_var', 'new_after')
wide_df <- merge(old_t0_df, old_t1_df)
wide_df <- merge(wide_df, new_t0_df)
wide_df <- merge(wide_df, new_t1_df)
wide_df$delta_old_model <- wide_df$old_century_after - wide_df$old_century_before
wide_df$delta_new_model <- wide_df$new_after - wide_df$new_before
wide_df$resid <- abs(wide_df$delta_new_model - wide_df$delta_old_model)
wide_df$perc_miss <- abs(wide_df$resid / wide_df$delta_old_model)

library(ggplot2)
p <- ggplot(wide_df, aes(x=delta_old_model, y=delta_new_model))
p <- p + geom_point()
p <- p + xlab("delta state variable: Century") + ylab("delta state variable: pyCentury")
p <- p + geom_abline(slope=1, intercept=0, linetype=3)
print(p)
pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/delta_new_v_old_century_1month.png"
png(file=pngname, units="in", res=300, width=3, height=2.5)
print(p)
dev.off()

p <- ggplot(wide_df, aes(x=1, y=resid))
p <- p + geom_boxplot()
print(p)

p <- ggplot(wide_df, aes(x=resid, y=perc_miss))
p <- p + geom_point()
print(p)

pngname <- "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/delta_new_v_old_century_1month_hist.png"
png(file=pngname, units="in", res=300, width=6, height=4)
hist(wide_df$resid, breaks=50, main="Mismatch: decomposition-related state var, 1 month",
     xlab="Delta new model - delta old model")
dev.off()

write.csv(wide_df, "C:/Users/ginge/Dropbox/NatCap_backup/Forage_model/pyCentury_dev_documents/mismatch_1month.csv",
          row.names=FALSE)
