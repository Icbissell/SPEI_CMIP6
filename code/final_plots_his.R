library(raster)
library(terra)
library(sf)
library(plotrix)

# source("SPEI_calc_his.R")

############### Figure 2 #####################

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

##### Set Up the figure (margins, files, etc): #####

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure2.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure2.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

#make std function
vectorized_std_dev <- Vectorize(function(x, y) sqrt(var(c(x, y))))

par(mfrow = c(3,3))
par(mai = c(0.7, 0.7, 0.2, 0.2))
cols = hcl.colors(10, palette = "Zissou 1")
extent <- extent(-75.5, -70.5, 40.5, 43.5)
##### Plot 1 #####
var <- "month_pr"

mod1 <- mod.mean
mod2 <- hist

r1 <- stack(lapply(1:dim(mod1[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod1[[var]])[1], ncol = dim(mod1[[var]])[2], 
              xmn = min(mod1[["lon"]]), xmx = max(mod1[["lon"]]), 
              ymn = min(mod1[["lat"]]), ymx = max(mod1[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod1[[var]][, , i]
  return(r)
}))

r2 <- stack(lapply(1:dim(mod2[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod2[[var]])[1], ncol = dim(mod2[[var]])[2], 
              xmn = min(mod2[["lon"]]), xmx = max(mod2[["lon"]]), 
              ymn = min(mod2[["lat"]]), ymx = max(mod2[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod2[[var]][, , i]
  return(r)
}))

r2 <- crop(r2, extent(r1))
r1 <- resample(r1, r2, method = "bilinear")

p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$spearman, ext = extent, xlab="", 
     ylab="", col = cols)
mtext("Correlation Coefficient", side = 3, line = 0.4, font = 2)
mtext("Precipitation", side = 2, line = 2.7, cex = 0.8, font = 2)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 2 #####
p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$p.value, ext = extent,
     xlab="", ylab="", col = cols)
mtext("p-value", side = 3, line = 0.4, font = 2)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 3 #####
std_dev <- overlay(r1, r2, fun = vectorized_std_dev)
avg_std_dev <- mean(std_dev, na.rm = TRUE)

plot(avg_std_dev, xlab="", ylab="", 
     col = cols, ext = extent, legend.args = list(text = '(mm)', cex = 0.5))

mtext("Standard Deviation", side = 3, line = 0.4, font = 2)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 4 #####
var <- "month_rlds"

mod1 <- mod.mean
mod2 <- hist

r1 <- stack(lapply(1:dim(mod1[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod1[[var]])[1], ncol = dim(mod1[[var]])[2], 
              xmn = min(mod1[["lon"]]), xmx = max(mod1[["lon"]]), 
              ymn = min(mod1[["lat"]]), ymx = max(mod1[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod1[[var]][, , i]
  return(r)
}))

r2 <- stack(lapply(1:dim(mod2[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod2[[var]])[1], ncol = dim(mod2[[var]])[2], 
              xmn = min(mod2[["lon"]]), xmx = max(mod2[["lon"]]), 
              ymn = min(mod2[["lat"]]), ymx = max(mod2[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod2[[var]][, , i]
  return(r)
}))

r2 <- crop(r2, extent(r1))
r1 <- resample(r1, r2, method = "bilinear")

p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$spearman, ext = extent,
     xlab="", ylab="", col = cols)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)
mtext("Maximum temperature", side = 2, line = 2.7, cex = 0.8, font = 2)



##### Plot 5 #####
p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$p.value, ext = extent,
     xlab="", ylab="", col = cols)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 6 #####
std_dev <- overlay(r1, r2, fun = vectorized_std_dev)
avg_std_dev <- mean(std_dev, na.rm = TRUE)

plot(avg_std_dev,
     xlab="", ylab="", col = cols, 
     ext = extent, legend.args = list(text = '(mm)', cex = 0.5))
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 7 #####
var <- "month_PET"

mod1 <- mod.mean
mod2 <- hist

r1 <- stack(lapply(1:dim(mod1[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod1[[var]])[1], ncol = dim(mod1[[var]])[2], 
              xmn = min(mod1[["lon"]]), xmx = max(mod1[["lon"]]), 
              ymn = min(mod1[["lat"]]), ymx = max(mod1[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod1[[var]][, , i]
  return(r)
}))

r2 <- stack(lapply(1:dim(mod2[[var]])[3], function(i) {
  r <- raster(nrow = dim(mod2[[var]])[1], ncol = dim(mod2[[var]])[2], 
              xmn = min(mod2[["lon"]]), xmx = max(mod2[["lon"]]), 
              ymn = min(mod2[["lat"]]), ymx = max(mod2[["lat"]]), 
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  values(r) <- mod2[[var]][, , i]
  return(r)
}))

r2 <- crop(r2, extent(r1))
r1 <- resample(r1, r2, method = "bilinear")

p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$spearman, ext = extent,
     xlab="", ylab="", col = cols)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)
mtext("PET", side = 2, line = 2.7, cex = 0.8, font = 2)


##### Plot 8 #####
p <- raster::corLocal(r2, r1, test = T, method = "spearman")
plot(p$p.value, ext = extent,
     xlab="", ylab="", col = cols)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

##### Plot 9 #####
std_dev <- overlay(r1, r2, fun = vectorized_std_dev)
avg_std_dev <- mean(std_dev, na.rm = TRUE)

plot(avg_std_dev,
     xlab="", ylab="", col = cols, ext = extent, 
     legend.args = list(text = '(mm)', cex = 0.5), 
     useRaster=TRUE)
mtext("Latitude", side = 2, line = 1.9, cex = 0.5)
mtext("Longitude", side = 1, line = 1.5, cex = 0.5)

# close file
if(writeFile != 'off') {
  dev.off()
}


############### Figure 3 #####################

# writeFile <- 'pdf'
writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure3.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure3.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m, heights = c(0.7, 0.7, 0.1))

### rlds ###

var <- "rlds"

models <- list(mod.GFDL, mod.IPSL, mod.BCC, mod.MIR, mod.GISS, mod.CAN, mod.mean, cal.list)

averages <- lapply(models, function(model) {
  ave <- mean_time(model, var)
  ave <- ave[model$date %in% com_dates]
  return(ave)
})

hist_ave <- data.frame(var = mean_time(hist, var), date = hist$date)
hist_ave <- hist_ave[hist_ave$date %in% com_dates,,]

for (i in seq_along(averages)) {
  taylor.diagram(hist_ave$var, averages[[i]], add = (i != 1), 
                 col = palette.colors()[i + 1], cex = 1, pcex = 2, 
                 main = "", 
                 ngamma = 4, 
                 sd.arcs = T,
                 xlab = "")
}

mtext("Standard deviation", side = 1, line = 2.3, cex = 0.75)
mtext("Downwelling Longwave Radiation", side = 3, line = 0.7, cex = 1, font = 2)


### tasmin ###

var <- "tasmin"

models <- list(mod.GFDL, mod.IPSL, mod.BCC, mod.MIR, mod.GISS, mod.CAN, mod.mean, cal.list)

averages <- lapply(models, function(model) {
  ave <- mean_time(model, var)
  ave <- ave[model$date %in% com_dates]
  return(ave)
})

hist_ave <- data.frame(var = mean_time(hist, var), date = hist$date)
hist_ave <- hist_ave[hist_ave$date %in% com_dates,,]

for (i in seq_along(averages)) {
  taylor.diagram(hist_ave$var, averages[[i]], add = (i != 1), 
                 col = palette.colors()[i + 1], cex = 1, pcex = 2, 
                 main = "", 
                 ngamma = 4, 
                 sd.arcs = T,
                 xlab = "")
}

mtext("Standard deviation", side = 1, line = 2.3, cex = 0.75)
mtext("Minimum Temperature", side = 3, line = 0.7, cex = 1, font = 2)



### tasmax ###

var <- "tasmax"

models <- list(mod.GFDL, mod.IPSL, mod.BCC, mod.MIR, mod.GISS, mod.CAN, mod.mean, cal.list)

averages <- lapply(models, function(model) {
  ave <- mean_time(model, var)
  ave <- ave[model$date %in% com_dates]
  return(ave)
})

hist_ave <- data.frame(var = mean_time(hist, var), date = hist$date)
hist_ave <- hist_ave[hist_ave$date %in% com_dates,,]

for (i in seq_along(averages)) {
  taylor.diagram(hist_ave$var, averages[[i]], add = (i != 1), 
                 col = palette.colors()[i + 1], cex = 1, pcex = 2, 
                 main = "", 
                 ngamma = 4, 
                 sd.arcs = T,
                 xlab = "")
}

mtext("Standard deviation", side = 1, line = 2.3, cex = 0.75)
mtext("Maximum temperature", side = 3, line = 0.7, cex = 1, font = 2)


### pr ###

var <- "pr"
models <- list(mod.GFDL, mod.IPSL, mod.BCC, mod.MIR, mod.GISS, mod.CAN, mod.mean, cal.list)

averages <- lapply(models, function(model) {
  ave <- mean_time(model, var)
  ave <- ave[model$date %in% com_dates]
  return(ave)
})

hist_ave <- data.frame(var = mean_time(hist, var), date = hist$date)
hist_ave <- hist_ave[hist_ave$date %in% com_dates,,]

for (i in seq_along(averages)) {
  taylor.diagram(hist_ave$var, averages[[i]], add = (i != 1), 
                 col = palette.colors()[i + 1], cex = 1, pcex = 2, 
                 main = "", xlab = "", 
                 ngamma = 4, 
                 sd.arcs = T)
}

mtext("Standard deviation", side = 1, line = 2.3, cex = 0.75)
mtext("Precipitation", side = 3, line = 0.7, cex = 1, font = 2)


par(mar = c(1, 2, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c(models[[1]][["name"]], models[[2]][["name"]], 
                  models[[3]][["name"]], models[[4]][["name"]], 
                  models[[5]][["name"]], models[[6]][["name"]], 
                  models[[7]][["name"]], "Calibrated"), 
       col=palette.colors()[2:9], lwd=5, cex=1, horiz = T)

# close file
if(writeFile != 'off') {
  dev.off()
}

##############################################



########## Figure 4 #############




