### Process soil data from AfSIS, calculate available water capacity
## equations from Zacharias and Wessolek 2007

datadir = 'C:/Users/Ginger/Dropbox/NatCap_backup/Kenya/Data/Soil'

# AfSIS soil layers:
# sd1: 0━5 cm
# sd2: 5━15 cm
# sd3: 15━30 cm
# sd4: 30━60 cm
# sd5: 60━100 cm
# sd6: 100━200 cm

upper_horizon = c('sd1', 'sd2')  # layers in upper soil horizon (PEGASUS expects 0-20 cm)
lower_horizon = c('sd5', 'sd6')  # layers in lower soil horizon (PEGASUS expects 50-150 cm)

prefixes = c('bld_', 'clyppt_', 'sndppt_')
suffixes = c(upper_horizon, lower_horizon)

# read original ascii files
data_list = list()
for (pr in prefixes) {
  for (suff in suffixes) {
    name = paste(pr, suff, '_m.txt', sep = "")
    file = paste(datadir, name, sep = "/")
    data = read.table(file, header = FALSE)
    data[data == -9999] <- NA
    data_list[[name]] = data
  }
}

for (suff in suffixes) {
  sand_m = as.data.frame(data_list[[paste('sndppt_', suff, '_m.txt', sep = "")]])
  clay_m = as.data.frame(data_list[[paste('clyppt_', suff, '_m.txt', sep = "")]])
  bulk_dens_m = as.data.frame(data_list[[paste('bld_', suff, '_m.txt', sep = "")]])
  
  nrows = nrow(sand_m)
  ncols = ncol(sand_m)
  
  AWC = data.frame(matrix(NA, nrows, ncols))
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sand = sand_m[i, j]
      clay = clay_m[i, j]
      bulk_dens = bulk_dens_m[i, j]
      if (any(is.na(c(sand, clay, bulk_dens)))) {
        print('skipped due to NA')
        next
      }
      else {
        if (sand < 66.5) {
          theta_r <- 0
          theta_s <- 0.788 + 0.001 * clay - 0.263 * bulk_dens
          alpha <- exp(-0.648 + 0.023 * sand + 0.044 * clay - 3.168 * bulk_dens)
          n <- 1.392 - 0.418 * sand ** -0.024 + clay ** -0.704
        }
        else {
          theta_r <- 0
          theta_s <- 0.89 - 0.001 * clay - 0.322 * bulk_dens
          alpha <- exp(-4.197 + 0.013 * sand + 0.076 * clay - 0.276 * bulk_dens)
          n <- -2.562 + 0.000000007 * sand ** 4.004 + 3.75 * clay ** -0.016
        }
        m = 1 - 1/n
        theta_fc = theta_r + ((theta_s - theta_r) / (1 + (alpha * 33) ** n) ** m)
        theta_pwp = theta_r + ((theta_s - theta_r) / (1 + (alpha * 1500) ** n) ** m)
        AWC[i, j] = theta_fc - theta_pwp
      }
      print('finished one column...')
    }
    print('finished one row...')
  }
  AWC_name <- paste("AWC_", suff, sep = "")
  assign(AWC_name, AWC)
}
  
# sum across layers in horizons, weighted by depth of each layer
# AfSIS soil layers:
# sd1: 0━5 cm
# sd2: 5━15 cm
# sd3: 15━30 cm
# sd4: 30━60 cm
# sd5: 60━100 cm
# sd6: 100━200 cm

AWC_0_15 <- AWC_sd1 * 5 + AWC_sd2 * 10  # following Ugbaje Reuter 2013
AWC_60_200 <- AWC_sd5 * 40 + AWC_sd6 * 100

AWC_0_15[is.na(AWC_0_15)] <- -9999
AWC_60_200[is.na(AWC_60_200)] <- -9999

outdir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Kenya/Data/Soil/AWC/'
write.table(AWC_0_15, paste(outdir, "AWC_0_15.txt", sep = ""), row.names=FALSE, col.names=FALSE)
write.table(AWC_60_200, paste(outdir, "AWC_60_200.txt", sep = ""), row.names=FALSE, col.names=FALSE)

