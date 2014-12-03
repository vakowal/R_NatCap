# Plot output data from CENTURY

datadir <- 'C:/Users/Ginger/Dropbox/NatCap_backup/Forage_model/CENTURY4.6/Century46_PC_Jan-2014' 
outdir <- datadir
outvars <- read.table(paste(datadir, '/outvars.txt', sep = ""), header = FALSE)

widths <- rep(16, length(outvars[, 1]) + 1)
Kenya <- read.fwf(paste(outdir, '/K_P93.lis', sep = ""), widths, skip = 2)
col.names = read.table(paste(outdir, '/K_P93.lis', sep = ""), nrow = 1, as.is = TRUE)
colnames(Kenya) <- col.names

recent <- Kenya[(Kenya$time > 5000),]
k_jan <- recent[(recent$time %% 1 == 0), ]
k_mar <- Kenya[(Kenya$time - floor(Kenya$time) == 0.25), ]
k_may <- Kenya[((Kenya$time - floor(Kenya$time)) > 0.41) & ((Kenya$time - floor(Kenya$time)) < 0.43), ]
k_jul <- Kenya[((Kenya$time - floor(Kenya$time)) > 0.57) & ((Kenya$time - floor(Kenya$time)) < 0.59), ]
k_sep <- Kenya[((Kenya$time - floor(Kenya$time)) > 0.74) & ((Kenya$time - floor(Kenya$time)) < 0.76), ]
k_nov <- Kenya[((Kenya$time - floor(Kenya$time)) > 0.91) & ((Kenya$time - floor(Kenya$time)) < 0.93), ]

opc1g <- read.fwf(paste(outdir, '/opc1a-g.lis', sep = ""), widths, skip = 2)
opc4 <- read.fwf(paste(outdir, '/opc4.lis', sep = ""), widths, skip = 2)
opc6 <- read.fwf(paste(outdir, '/opc6.lis', sep = ""), widths, skip = 2)


colnames(opc4) <- col.names
colnames(opc6) <- col.names

plot(opc1[-1, 'time'], opc1[-1, 'aglivc'], ylab = 'aglivc - opc1')#, ylim = c(0, 170))
plot(opc4[-1, 'time'], opc4[-1, 'aglivc'], ylab = 'aglivc - opc4')#, ylim = c(0, 170))
plot(opc6[-1, 'time'], opc6[-1, 'aglivc'], ylab = 'aglivc - opc6')#, ylim = c(0, 170))

plot(opc1[-1, 'time'], opc1[-1, 'stdedc'], ylab = 'standing dead C - opc1')
plot(opc4[-1, 'time'], opc4[-1, 'stdedc'], ylab = 'standing dead C - opc4')
plot(opc6[-1, 'time'], opc6[-1, 'stdedc'], ylab = 'standing dead C - opc6')

combined <- opc1[opc1$time > 1998 & opc1$time <= 2014.5, c('time', 'aglivc', 'stdedc')]
colnames(combined) <- c('time', 'live-opc1', 'stded-opc1')
combined[, 'live-opc1g'] <- opc1g[opc1g$time > 1998 & opc1g$time <= 2014.5, 'aglivc']
combined[, 'stded-opc1g'] <- opc1g[opc1g$time > 1998 & opc1g$time <= 2014.5, 'stdedc']

combined[, 'live-opc4'] <- opc4[opc4$time > 1998 & opc4$time <= 2014.5, 'aglivc']
combined[, 'stded-opc4'] <- opc4[opc4$time > 1998 & opc4$time <= 2014.5, 'stdedc']
combined[, 'live-opc6'] <- opc6[opc6$time > 1998 & opc6$time <= 2014.5, 'aglivc']
combined[, 'stded-opc6'] <- opc6[opc6$time > 1998 & opc6$time <= 2014.5, 'stdedc']
combined[, 'sum-opc1'] <- combined[, 'live-opc1'] + combined[, 'stded-opc1']
combined[, 'sum-opc1g'] <- combined[, 'live-opc1g'] + combined[, 'stded-opc1g']
combined[, 'sum-opc4'] <- combined[, 'live-opc4'] + combined[, 'stded-opc4']
combined[, 'sum-opc6'] <- combined[, 'live-opc6'] + combined[, 'stded-opc6']
measured <- as.data.frame(matrix(NA, nrow = 3, ncol = 2))
measured$site <- c('OPC1', 'opc4', 'OPC6')
measured$g_dm_sqm <- c(284.8152, 340.6584909, 381.9566)

plot(combined[, 'time'], combined[, 'live-opc1'], type = 'n', xlab = 'Year', ylab = 'Live dm (g per sq m)', ylim = c(0, 200))
lines(combined[, 'time'], combined[, 'live-opc1'], lwd = 2)
lines(combined[, 'time'], combined[, 'live-opc1g'], lwd = 2, col = 'green')
lines(combined[, 'time'], combined[, 'live-opc4'], col = 'blue', lwd = 2)
lines(combined[, 'time'], combined[, 'live-opc6'], pch = 17, col = 'green', lwd = 2)

plot(combined[, 'time'], combined[, 'sum-opc1'], type = 'n', xlab = 'Year', ylab = 'Dry matter (g per sq m)', ylim = c(0, 400))
lines(combined[, 'time'], combined[, 'sum-opc4'], col = 'blue', lwd = 2)
lines(combined[, 'time'], combined[, 'sum-opc6'], pch = 17, col = 'green', lwd = 2)
lines(combined[, 'time'], combined[, 'sum-opc1'], lwd = 2)
lines(combined[, 'time'], combined[, 'sum-opc1g'], lwd = 2, col = 'green')

points(2014.5, measured[1, 'g_dm_sqm'], pch = 15)
points(2014.5, measured[2, 'g_dm_sqm'], pch = 16, col = 'blue')
points(2014.5, measured[3, 'g_dm_sqm'], pch = 17, col = 'green')
legend(2010, 390, c('OPC1', 'OPC4', 'OPC6'), lwd = c(2, 2, 2), col = c('black', 'blue', 'green'))

averages <- colMeans(combined)
