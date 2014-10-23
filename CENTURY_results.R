# Plot output data from CENTURY

outdir <- 'C:/Users/Ginger/Documents/NatCap/Other_Peoples_Models/CENTURY4.6/Kenya'
datadir <- 'C:/Users/Ginger/Documents/NatCap/Other_Peoples_Models/CENTURY4.6/Century46_PC_Jan-2014' 
outvars <- read.table(paste(datadir, '/outvars.txt', sep = ""), header = FALSE)

widths <- rep(16, length(outvars[, 1]) + 1)
opc1 <- read.fwf(paste(outdir, '/opc1.lis', sep = ""), widths, skip = 2)
opc2 <- read.fwf(paste(outdir, '/opc2.lis', sep = ""), widths, skip = 2)
opc6 <- read.fwf(paste(outdir, '/opc6.lis', sep = ""), widths, skip = 2)

col.names = read.table(paste(outdir, '/opc1.lis', sep = ""), nrow = 1, as.is = TRUE)
colnames(opc1) <- col.names
colnames(opc2) <- col.names
colnames(opc6) <- col.names

plot(opc1[103:152, 'time'], opc1[103:152, 'aglivc'], ylab = 'aglivc - opc1', ylim = c(0, 170))
plot(opc2[103:152, 'time'], opc2[103:152, 'aglivc'], ylab = 'aglivc - opc2', ylim = c(0, 170))
plot(opc6[103:152, 'time'], opc6[103:152, 'aglivc'], ylab = 'aglivc - opc6', ylim = c(0, 170))

plot(opc1[103:152, 'time'], opc1[103:152, 'stdedc'], ylab = 'standing dead C - opc1')
plot(opc2[103:152, 'time'], opc2[103:152, 'stdedc'], ylab = 'standing dead C - opc2')
plot(opc6[103:152, 'time'], opc6[103:152, 'stdedc'], ylab = 'standing dead C - opc6')

combined <- opc1[120:152, c('time', 'aglivc', 'stdedc')]
colnames(combined) <- c('time', 'live-opc1', 'stded-opc1')
combined[, 'live-opc2'] <- opc2[120:152, 'aglivc']
combined[, 'stded-opc2'] <- opc2[120:152, 'stdedc']
combined[, 'live-opc6'] <- opc6[120:152, 'aglivc']
combined[, 'stded-opc6'] <- opc6[120:152, 'stdedc']
combined[, 'sum-opc1'] <- combined[, 'live-opc1'] + combined[, 'stded-opc1']
combined[, 'sum-opc2'] <- combined[, 'live-opc2'] + combined[, 'stded-opc2']
combined[, 'sum-opc6'] <- combined[, 'live-opc6'] + combined[, 'stded-opc6']
measured <- as.data.frame(matrix(NA, nrow = 3, ncol = 2))
measured$site <- c('OPC1', 'OPC2', 'OPC6')
measured$g_dm_sqm <- c(284.8152, 334.9443, 381.9566)
  
plot(combined[, 'time'], combined[, 'sum-opc1'], type = 'n', xlab = 'Year', ylab = 'Dry matter (g per sq m)', ylim = c(280, 390))
lines(combined[, 'time'], combined[, 'sum-opc1'], lwd = 2)
lines(combined[, 'time'], combined[, 'sum-opc2'], col = 'blue', lwd = 2)
lines(combined[, 'time'], combined[, 'sum-opc6'], pch = 17, col = 'green', lwd = 2)
points(2004, measured[1, 'g_dm_sqm'], pch = 15)
points(2004, measured[2, 'g_dm_sqm'], pch = 16, col = 'blue')
points(2004, measured[3, 'g_dm_sqm'], pch = 17, col = 'green')
legend(2005.2, 380, c('OPC1', 'OPC2', 'OPC6'), pch = c(15, 16, 17), col = c('black', 'blue', 'green'))

averages <- colMeans(combined)
