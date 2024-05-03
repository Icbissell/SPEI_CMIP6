
# source("SPEI_calc_his.R")

############ Appendix 1 ############

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/App1.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/App1.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# par(mfrow = c(2,1))
start <- as.Date("1979-01-01")
end <- as.Date("2014-12-31")
#get model averages
var <- "month_pr"

gfdl_ave <- mean_time(mod.GFDL, var)
ipsl_ave <- mean_time(mod.IPSL, var)
bcc_ave <- mean_time(mod.BCC, var)
mir_ave <- mean_time(mod.MIR, var)
giss_ave <- mean_time(mod.GISS, var)
can_ave <- mean_time(mod.CAN, var)
hist_ave <- mean_time(hist, var)

hist_dates <- as.Date(paste(hist$months, "01", sep = "-"))

#plot historical data
mycol <- adjustcolor('blue', alpha.f = 0.2)
plot(hist_dates, hist_ave, col = mycol, ylim = c(0, 250), 
     ylab = "Monthly precipition (mm)", xlab = "Year")
a <- zoo::rollmean(hist_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(a))
lines(date_sequence, a, col = "blue", lwd = 3)


#plot GFDL model
mod_dates <- as.Date(paste(mod.GFDL$months, "01", sep = "-"))

mycol <- adjustcolor('darkgreen', alpha.f = 0.2)
points(mod_dates, gfdl_ave, col = mycol)
b <- zoo::rollmean(gfdl_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(b))
lines(date_sequence, b, col = "darkgreen", lwd = 1.5)

#plot BCC model
mod_dates <- as.Date(paste(mod.BCC$months, "01", sep = "-"))

mycol <- adjustcolor('red', alpha.f = 0.2)
points(mod_dates, bcc_ave, col = mycol)
c <- zoo::rollmean(bcc_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(c))
lines(date_sequence, c, col = "red", lwd = 1.5)

#plot IPSL model
mod_dates <- as.Date(paste(mod.IPSL$months, "01", sep = "-"))

mycol <- adjustcolor('orange', alpha.f = 0.2)
points(mod_dates, ipsl_ave, col = mycol)
d <- zoo::rollmean(ipsl_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end,
                     length.out = length(d))
lines(date_sequence, d, col = 'orange', lwd = 1.5)

#plot MIR model
mod_dates <- as.Date(paste(mod.MIR$months, "01", sep = "-"))

mycol <- adjustcolor('purple', alpha.f = 0.2)
points(mod_dates, mir_ave, col = mycol)
e <- zoo::rollmean(mir_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(e))
lines(date_sequence, e, col = 'purple', lwd = 1.5)

#plot GISS model
mod_dates <- as.Date(paste(mod.GISS$months, "01", sep = "-"))

mycol <- adjustcolor('dodgerblue', alpha.f = 0.2)
points(mod_dates, giss_ave, col = mycol)
e <- zoo::rollmean(giss_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(e))
lines(date_sequence, e, col = 'dodgerblue', lwd = 1.5)

#plot CAN model
mod_dates <- as.Date(paste(mod.CAN$months, "01", sep = "-"))

mycol <- adjustcolor('pink', alpha.f = 0.2)
points(mod_dates, can_ave, col = mycol)
e <- zoo::rollmean(can_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = start, to = end, 
                     length.out = length(e))
lines(date_sequence, e, col = 'pink', lwd = 1.5)

#plot model average
model_ave <- mean_time(mod.mean, var)

mycol <- adjustcolor('black', alpha.f = 0.2)
points(mod_dates, model_ave, col = mycol)
f <- zoo::rollmean(model_ave, 12, na.rm = TRUE, align = "right")
date_sequence <- seq(from = as.Date("1980-01-01"), to = as.Date("2013-12-31"), 
                     length.out = length(f))
lines(date_sequence, f, col = 'black', lwd = 3)

legend("topleft", legend = c("Historical", "GFDL-ESM4", "BCC-CSM2-MR", "IPSL-CM6A-LR", "MIROC6", "GISS-E2-1-G","CanESM5","Ensemble average"), 
       col = c("blue", "darkgreen", "red", "orange", "purple", "dodgerblue", "pink", "black"), lwd = c(2, 1, 1, 1, 1, 1, 1, 2),
       lty = 1, cex = 1)

# close file
if(writeFile != 'off') {
  dev.off()
}




############ Appendix 3 ############
writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/App2.55.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/App2.5.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


par(mfrow = c(2,1))
par(mar = c(4,4,2,2))
#get model averages
var <- "month_pr"
mod <- mod.mean.SSP126
mod_ave <- mean_time(mod, var)

mod_dates <- as.Date(paste(mod$months, "01", sep = "-"))

#plot historical data
mycol <- adjustcolor(palette.colors()[2], alpha.f = 0.2)
plot(mod_dates, mod_ave, col = mycol, ylim = c(50, 150), 
     ylab = "", xlab = "")
a <- zoo::rollmean(mod_ave, 24, na.rm = TRUE, align = "right")
date_sequence <- seq(from = mod_dates[1], to = mod_dates[length(mod_dates)], 
                     length.out = length(a))
lines(date_sequence, a, col = palette.colors()[2], lwd = 3)

mod <- mod.mean.SSP585
mod_ave <- mean_time(mod, var)

mod_dates <- as.Date(paste(mod$months, "01", sep = "-"))

#plot historical data
mycol <- adjustcolor(palette.colors()[7], alpha.f = 0.2)
points(mod_dates, mod_ave, col = mycol)
a <- zoo::rollmean(mod_ave, 24, na.rm = TRUE, align = "right")
date_sequence <- seq(from = mod_dates[1], to = mod_dates[length(mod_dates)], 
                     length.out = length(a))
lines(date_sequence, a, col = palette.colors()[7], lwd = 3)

legend("topleft", c("SSP1-2.6","SSP5-8.5"), lwd = c(3,3), col = c(palette.colors()[2], palette.colors()[7]))
mtext("Precipitation", side = 3, line = 0.3, cex = 1.2)
mtext("Year", side = 1, line = 1.8)
mtext("Monthly average precipitation (mm)", side = 2, 
      line = 1.9, cex = 0.8)

###rlds

#get model averages
var <- "month_rlds"
mod <- mod.mean.SSP126
mod_ave <- mean_time(mod, var)

mod_dates <- as.Date(paste(mod$months, "01", sep = "-"))

#plot historical data
mycol <- adjustcolor(palette.colors()[2], alpha.f = 0.2)
plot(mod_dates, mod_ave, col = mycol, ylim = c(20, 35), 
     ylab = "", xlab = "")
a <- zoo::rollmean(mod_ave, 24, na.rm = TRUE, align = "right")
date_sequence <- seq(from = mod_dates[1], to = mod_dates[length(mod_dates)], 
                     length.out = length(a))
lines(date_sequence, a, col = palette.colors()[2], lwd = 3)

mod <- mod.mean.SSP585
mod_ave <- mean_time(mod, var)

mod_dates <- as.Date(paste(mod$months, "01", sep = "-"))

#plot historical data
mycol <- adjustcolor(palette.colors()[7], alpha.f = 0.2)
points(mod_dates, mod_ave, col = mycol)
a <- zoo::rollmean(mod_ave, 24, na.rm = TRUE, align = "right")
date_sequence <- seq(from = mod_dates[1], to = mod_dates[length(mod_dates)], 
                     length.out = length(a))
lines(date_sequence, a, col = palette.colors()[7], lwd = 3)

legend("topleft", c("SSP1-2.6","SSP5-8.5"), lwd = c(3,3), col = c(palette.colors()[2], palette.colors()[7]))
mtext("Downwelling longwave radiation", side = 3, line = 0.3, cex = 1.2)
mtext("Year", side = 1, line = 1.8)
mtext("Monthly average downwelling longwave radiation (mm)", side = 2, 
      line = 1.9, cex = 0.8)
# close file
if(writeFile != 'off') {
  dev.off()
}
