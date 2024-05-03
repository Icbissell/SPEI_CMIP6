library(raster)
library(terra)
library(sf)
library(plotrix)

# source("SPEI_calc_fut.R")

############### Figure 4 #####################

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure4.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure4.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


par(mfrow = c(3,2))
par(mar = c(2, 3.2, 2, 2))
var <- c("SPEI_1", "SPEI_1", "SPEI_6", "SPEI_6", "SPEI_12", "SPEI_12")
var_name <- c("SPEI 1", "SPEI 1", "SPEI 6", "SPEI 6", "SPEI 12", "SPEI 12")

for (a in seq_along(var)) {
  
  ave <- list()
  var.vec <- var[a]
  
  if (a > 4) {
    par(mar = c(3, 3.2, 2, 2))
  }
  
  if (a %% 2 == 1) {
    # extract common dates
    mod.list <- mod.list.SSP126
    dates_char <- lapply(mod.list, function(x) as.character(x$date))
    com_dates <- as.Date(Reduce(intersect, dates_char))
    
    com_month <- format(com_dates, "%Y-%m")
    com_month <- unique(com_month)
    
    for (i in seq_along(mod.list)) {
      for (vec in var.vec) {
        mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
      }
    }
    
    for(i in seq_along(mod.list)) {
      mod <- mod.list[[i]]
      ave[[mod$name]] <- mean_time(mod[[var.vec]])
    }
    
    ave.df <- data.frame(ave)
    c <- rowMeans(ave.df, na.rm = TRUE)
    
    std_err <- apply(ave.df, 1, sd) / sqrt(nrow(ave.df))
    ci <- 1.96 * std_err
    x.seq <- as.Date(paste0(com_month, "-01-01"))
    
    
    plot(x.seq, c, type = 'l', xlab = "", ylab = "", ylim = c(-2,2))
    mtext("Year", side = 1, line = 2, cex = 0.7)
    mtext(paste(var_name[a]), side = 2, line = 2, cex = 0.85)
    if (a == 1) {
      mtext("SSP1-2.6", side = 3, cex = 1.5)
    }
    
    lower_range <- apply(ave.df, 1, min)
    upper_range <- apply(ave.df, 1, max)
    
    polygon(c(x.seq, rev(x.seq)), c(lower_range, rev(upper_range)), col = palette.colors(alpha = 0.5)[3], border = NA)
    lines(x.seq, c, type = "l", lwd = 2, col = palette.colors()[6])
    abline(h = 0, lty = 2)
  }
  else {
    mod.list <- mod.list.SSP585
    # extract common dates
    dates_char <- lapply(mod.list, function(x) as.character(x$date))
    com_dates <- as.Date(Reduce(intersect, dates_char))
    
    com_month <- format(com_dates, "%Y-%m")
    com_month <- unique(com_month)
    
    for (i in seq_along(mod.list)) {
      for (vec in var.vec) {
        mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
      }
    }
    
    for(i in seq_along(mod.list)) {
      mod <- mod.list[[i]]
      ave[[mod$name]] <- mean_time(mod[[var.vec]])
    }
    
    ave.df <- data.frame(ave)
    c <- rowMeans(ave.df, na.rm = TRUE)
    
    std_err <- apply(ave.df, 1, sd) / sqrt(nrow(ave.df))
    ci <- 1.96 * std_err
    x.seq <- as.Date(paste0(com_month, "-01-01"))
    
    
    plot(x.seq, c, type = 'l', xlab = "", ylab = "", ylim = c(-2,2))
    mtext("Year", side = 1, line = 2, cex = 0.7)
    if (a == 2) {
      mtext("SSP5-8.5", side = 3, cex = 1.5)
    }
    
    lower_range <- apply(ave.df, 1, min)
    upper_range <- apply(ave.df, 1, max)
    
    polygon(c(x.seq, rev(x.seq)), c(lower_range, rev(upper_range)), col = palette.colors(alpha = 0.5)[2], border = NA)
    lines(x.seq, c, type = "l", lwd = 2, col = palette.colors()[7])
    abline(h = 0, lty = 2)
  }
}

# close file
if(writeFile != 'off') {
  dev.off()
}


##############################################

############### Figure 5 #####################


# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure5.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure5.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

par(mfrow = c(2,1))
par(mar = c(3, 3, 2, 1.5))

### plot SSP126 ###

mod.list <- mod.list.SSP126

ave <- list()
var.vec <- "SPEI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim = c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 12", side = 2, line = 2, cex = 0.85)
mtext("SSP126", side = 3, cex = 1.5)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors(alpha = 0.8)[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 0.75, horiz = T)


### plot SSP585 ###

mod.list <- mod.list.SSP585

ave <- list()
var.vec <- "SPEI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim= c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 12", side = 2, line = 2, cex = 0.85)
mtext("SSP585", side = 3, cex = 1.5)
abline(h = 0, lty = 2)

abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors()[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 0.75, horiz = T)

# close file
if(writeFile != 'off') {
  dev.off()
}

##############################################

############### Figure 5.5 #####################


writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure5.5.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure5.5.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}



par(mfrow = c(4,1))
par(mar = c(3, 3, 2, 1.5))


### plot SSP126 ###

mod.list <- mod.list.SSP126

ave <- list()
var.vec <- "SPEI_6"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim = c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 6", side = 2, line = 2, cex = 0.85)
mtext("SSP1-2.6", side = 3, cex = 1.2)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_6"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors(alpha = 0.8)[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 1, horiz = T)


### plot SSP585 ###

mod.list <- mod.list.SSP585

ave <- list()
var.vec <- "SPEI_6"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim= c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 6", side = 2, line = 2, cex = 0.85)
mtext("SSP5-8.5", side = 3, cex = 1.2)
abline(h = 0, lty = 2)

abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_6"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors()[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 1, horiz = T)

##Now SPEI 12


### plot SSP126 ###

mod.list <- mod.list.SSP126

ave <- list()
var.vec <- "SPEI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim = c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 12", side = 2, line = 2, cex = 0.85)
mtext("SSP1-2.6", side = 3, cex = 1.2)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors(alpha = 0.8)[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 1, horiz = T)


### plot SSP585 ###

mod.list <- mod.list.SSP585

ave <- list()
var.vec <- "SPEI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

plot(x.seq, c, type = 'l', xlab = "", ylab = "", 
     col = palette.colors()[7], lwd = 2, ylim= c(-1.5, 1.5))
mtext("Year", side = 1, line = 2, cex = 0.85)
mtext("SPEI 12", side = 2, line = 2, cex = 0.85)
mtext("SSP5-8.5", side = 3, cex = 1.2)
abline(h = 0, lty = 2)

abline(h = 1, lty = 2)
abline(h = -1, lty = 2)

###### SPI plot now
ave <- list()
var.vec <- "SPI_12"
print(var.vec)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

com_month <- format(com_dates, "%Y-%m")
com_month <- unique(com_month)

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$months %in% com_month]
  }
}

for(i in seq_along(mod.list)) {
  mod <- mod.list[[i]]
  ave[[mod$name]] <- mean_time(mod[[var.vec]])
}

ave.df <- data.frame(ave)
c <- rowMeans(ave.df, na.rm = TRUE)

x.seq <- as.Date(paste0(com_month, "-01-01"))

lines(x.seq, c, type = 'l', xlab = "", ylab = "", 
      col = palette.colors()[5], lwd = 2)

legend("topleft", c("SPEI", "SPI"), 
       col = c(palette.colors()[7], palette.colors()[5]), 
       lwd = 2, cex = 1, horiz = T)


# close file
if(writeFile != 'off') {
  dev.off()
}



##############################################

############### Figure 6 pt 1 ###################

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure6pt1.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure6pt1.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

d_ind <- function(vec, thresh) {
  check <- vec
  check[is.na(check)] <- 0
  if (length(check[check < thresh]) > 0) {
    vec <- matrix(t(vec),ncol=1)
    vec <- drought::RunDS(vec, thresh)
    return(vec)
  }
  else {
    vec <- data.frame(Duration = 0)
    return(vec)
  }
}

drought_indices <- function(data, mid_date, var, thresh) {
  month_to_date <- as.Date(paste(data$months, "01", sep = "-"))
  early_indices <- mean_time(data[[var]][, , which(month_to_date < mid_date)])
  late_indices <- mean_time(data[[var]][, , which(month_to_date > mid_date)])
  
  early_drought <- d_ind(early_indices, thresh)
  late_drought <- d_ind(late_indices, thresh)
  
  return(list(early_drought = early_drought, late_drought = late_drought))
}

event_stats <- function(drought_list) {
  durations <- sapply(drought_list, function(x) sum(x$Duration[x$Duration > 2]))
  num_events <- sapply(drought_list, function(x) sum(!is.na(x$Duration[x$Duration > 2])))
  
  return(list(durations = durations, num_events = num_events))
}

par(mfrow = c(2,1))
par(mar = c(2.5,3, 2, 2))

var <- "SPEI_6"

mid.date <- as.Date("2058-01-01")
SPEI.126.mod <- drought_indices(mod.mean.SSP126, mid.date, var, -1)
SPEI.585.mod <- drought_indices(mod.mean.SSP585, mid.date, var, -1)

SPEI.126.mod[["stats"]] <- event_stats(SPEI.126.mod)
SPEI.585.mod[["stats"]] <- event_stats(SPEI.585.mod)

SPEI.126.ext <- drought_indices(mod.mean.SSP126, mid.date, var, -1.5)
SPEI.585.ext <- drought_indices(mod.mean.SSP585, mid.date, var, -1.5)

SPEI.126.ext[["stats"]] <- event_stats(SPEI.126.ext)
SPEI.585.ext[["stats"]] <- event_stats(SPEI.585.ext)

SPEI.126.sev <- drought_indices(mod.mean.SSP126, mid.date, var, -2)
SPEI.585.sev <- drought_indices(mod.mean.SSP585, mid.date, var, -2)

SPEI.126.sev[["stats"]] <- event_stats(SPEI.126.sev)
SPEI.585.sev[["stats"]] <- event_stats(SPEI.585.sev)

cats <- c("Moderate drought", "Severe drought", "Extreme drought")
cat_names <- c("SSP1-2.6 2015-2058", "SSP5-8.5 2015-2058", 
               "SSP1-2.6 2058-2100", "SSP5-8.5 2058-2100")
values <- matrix(c(
  SPEI.126.mod$stats$durations[["early_drought"]] - SPEI.126.ext$stats$durations[["early_drought"]], 
  SPEI.585.mod$stats$durations[["early_drought"]] - SPEI.585.ext$stats$durations[["early_drought"]], 
  SPEI.126.mod$stats$durations[["late_drought"]] - SPEI.126.ext$stats$durations[["late_drought"]], 
  SPEI.585.mod$stats$durations[["late_drought"]] - SPEI.585.ext$stats$durations[["late_drought"]], 
  SPEI.126.ext$stats$durations[["early_drought"]] - SPEI.126.sev$stats$durations[["early_drought"]], 
  SPEI.585.ext$stats$durations[["early_drought"]] - SPEI.585.sev$stats$durations[["early_drought"]], 
  SPEI.126.ext$stats$durations[["late_drought"]] - SPEI.126.sev$stats$durations[["late_drought"]], 
  SPEI.585.ext$stats$durations[["late_drought"]] - SPEI.585.sev$stats$durations[["late_drought"]], 
  SPEI.126.sev$stats$durations[["early_drought"]], 
  SPEI.585.sev$stats$durations[["early_drought"]], 
  SPEI.126.sev$stats$durations[["late_drought"]], 
  SPEI.585.sev$stats$durations[["late_drought"]]), 
  nrow = 4, ncol = 3)

# Create bar plot

cols <- c("lightblue2",
          "tomato1")

bp <- barplot(values, beside = TRUE, col = cols,
              legend.text = c("SSP1-2.6","SSP5-8.5"), 
              args.legend = list(x = "topright", cex = 1.2), 
              ylim = c(0, 80))

mtext("Months of Drought", side = 2, line = 2)

xval <- numeric(6)
vec.12 <- as.vector(bp)
for (i in 1:6) {
  xval[i] <- mean(c(vec.12[2 * i],vec.12[2 * i -1]))
}

labval <- numeric(3)
for (i in 1:3) {
  labval[i] <- mean(c(xval[2 * i],xval[2 * i -1]))
}

mtext(rep(c("2015-2057", "2058-2100")), side = 1, line = 0, 
      at = xval, cex = 0.9, font = 2)

mtext(cats, side = 1, line = 1.2, 
      at = labval, cex = 1)

mtext("SPEI 6", side = 3, line = 0.2, cex = 1.4)

# 2


var <- "SPEI_12"

mid.date <- as.Date("2058-01-01")
SPEI.126.mod <- drought_indices(mod.mean.SSP126, mid.date, var, -1)
SPEI.585.mod <- drought_indices(mod.mean.SSP585, mid.date, var, -1)

SPEI.126.mod[["stats"]] <- event_stats(SPEI.126.mod)
SPEI.585.mod[["stats"]] <- event_stats(SPEI.585.mod)

SPEI.126.ext <- drought_indices(mod.mean.SSP126, mid.date, var, -1.5)
SPEI.585.ext <- drought_indices(mod.mean.SSP585, mid.date, var, -1.5)

SPEI.126.ext[["stats"]] <- event_stats(SPEI.126.ext)
SPEI.585.ext[["stats"]] <- event_stats(SPEI.585.ext)

SPEI.126.sev <- drought_indices(mod.mean.SSP126, mid.date, var, -2)
SPEI.585.sev <- drought_indices(mod.mean.SSP585, mid.date, var, -2)

SPEI.126.sev[["stats"]] <- event_stats(SPEI.126.sev)
SPEI.585.sev[["stats"]] <- event_stats(SPEI.585.sev)

cats <- c("Moderate drought", "Severe drought", "Extreme drought")
cat_names <- c("SSP1-2.6 2015-2058", "SSP5-8.5 2015-2058", 
               "SSP1-2.6 2058-2100", "SSP5-8.5 2058-2100")
values <- matrix(c(
  SPEI.126.mod$stats$durations[["early_drought"]] - SPEI.126.ext$stats$durations[["early_drought"]], 
  SPEI.585.mod$stats$durations[["early_drought"]] - SPEI.585.ext$stats$durations[["early_drought"]], 
  SPEI.126.mod$stats$durations[["late_drought"]] - SPEI.126.ext$stats$durations[["late_drought"]], 
  SPEI.585.mod$stats$durations[["late_drought"]] - SPEI.585.ext$stats$durations[["late_drought"]], 
  SPEI.126.ext$stats$durations[["early_drought"]] - SPEI.126.sev$stats$durations[["early_drought"]], 
  SPEI.585.ext$stats$durations[["early_drought"]] - SPEI.585.sev$stats$durations[["early_drought"]], 
  SPEI.126.ext$stats$durations[["late_drought"]] - SPEI.126.sev$stats$durations[["late_drought"]], 
  SPEI.585.ext$stats$durations[["late_drought"]] - SPEI.585.sev$stats$durations[["late_drought"]], 
  SPEI.126.sev$stats$durations[["early_drought"]], 
  SPEI.585.sev$stats$durations[["early_drought"]], 
  SPEI.126.sev$stats$durations[["late_drought"]], 
  SPEI.585.sev$stats$durations[["late_drought"]]), 
  nrow = 4, ncol = 3)

# Create bar plot

cols <- c("lightblue2",
          "tomato1")

bp <- barplot(values, beside = TRUE, col = cols,
              legend.text = c("SSP1-2.6","SSP5-8.5"), 
              args.legend = list(x = "topright", cex = 1.2), 
              ylim = c(0, 80))

mtext("Months of Drought", side = 2, line = 2)

xval <- numeric(6)
vec.12 <- as.vector(bp)
for (i in 1:6) {
  xval[i] <- mean(c(vec.12[2 * i],vec.12[2 * i -1]))
}

labval <- numeric(3)
for (i in 1:3) {
  labval[i] <- mean(c(xval[2 * i],xval[2 * i -1]))
}

mtext(rep(c("2015-2057", "2058-2100")), side = 1, line = 0, 
      at = xval, cex = 0.9, font = 2)

mtext(cats, side = 1, line = 1.2, 
      at = labval, cex = 1)

mtext("SPEI 12", side = 3, line = 0.2, cex = 1.4)

# close file
if(writeFile != 'off') {
  dev.off()
}

##############################################

############### Figure 6 pt 2 ##################

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figurept2.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figurept2.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

par(mfrow = c(2,1))
par(mar = c(2.5,3, 2, 2))

var <- "SPEI_6"

mid.date <- as.Date("2058-01-01")
SPEI.126.mod <- drought_indices(mod.mean.SSP126, mid.date, var, -1)
SPEI.585.mod <- drought_indices(mod.mean.SSP585, mid.date, var, -1)

SPEI.126.mod[["stats"]] <- event_stats(SPEI.126.mod)
SPEI.585.mod[["stats"]] <- event_stats(SPEI.585.mod)

SPEI.126.ext <- drought_indices(mod.mean.SSP126, mid.date, var, -1.5)
SPEI.585.ext <- drought_indices(mod.mean.SSP585, mid.date, var, -1.5)

SPEI.126.ext[["stats"]] <- event_stats(SPEI.126.ext)
SPEI.585.ext[["stats"]] <- event_stats(SPEI.585.ext)

SPEI.126.sev <- drought_indices(mod.mean.SSP126, mid.date, var, -2)
SPEI.585.sev <- drought_indices(mod.mean.SSP585, mid.date, var, -2)

SPEI.126.sev[["stats"]] <- event_stats(SPEI.126.sev)
SPEI.585.sev[["stats"]] <- event_stats(SPEI.585.sev)

cats <- c("Moderate drought", "Severe drought", "Extreme drought")
cat_names <- c("SSP1-2.6 2015-2058", "SSP5-8.5 2015-2058", 
               "SSP1-2.6 2058-2100", "SSP5-8.5 2058-2100")
values <- matrix(c(
  mean(SPEI.126.mod$early_drought$Duration), 
  mean(SPEI.585.mod$early_drought$Duration),  
  mean(SPEI.126.mod$late_drought$Duration),  
  mean(SPEI.585.mod$late_drought$Duration),  
  mean(SPEI.126.sev$early_drought$Duration),  
  mean(SPEI.585.sev$early_drought$Duration),  
  mean(SPEI.126.sev$late_drought$Duration),  
  mean(SPEI.585.sev$late_drought$Duration),  
  mean(SPEI.126.ext$early_drought$Duration),  
  mean(SPEI.585.ext$early_drought$Duration),  
  mean(SPEI.126.ext$late_drought$Duration),  
  mean(SPEI.585.ext$late_drought$Duration)),
  nrow = 4, ncol = 3)

errors <- c(sd(SPEI.126.mod$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.mod$early_drought$Duration)), 
            sd(SPEI.585.mod$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.mod$early_drought$Duration)),  
            sd(SPEI.126.mod$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.mod$late_drought$Duration)),  
            sd(SPEI.585.mod$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.mod$late_drought$Duration)),  
            sd(SPEI.126.sev$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.sev$early_drought$Duration)),  
            sd(SPEI.585.sev$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.sev$early_drought$Duration)),  
            sd(SPEI.126.sev$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.sev$late_drought$Duration)),  
            sd(SPEI.585.sev$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.sev$late_drought$Duration)),  
            sd(SPEI.126.ext$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.ext$early_drought$Duration)),  
            sd(SPEI.585.ext$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.ext$early_drought$Duration)),  
            sd(SPEI.126.ext$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.ext$late_drought$Duration)),  
            sd(SPEI.585.ext$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.ext$late_drought$Duration)))


# Create bar plot

cols <- c("lightblue2",
          "tomato1")

bp <- barplot(values, beside = TRUE, col = cols,
              legend.text = c("SSP1-2.6","SSP5-8.5"), 
              args.legend = list(x = "topright", cex = 1.2), 
              ylim = c(0, 6))

error_top <- as.vector(values) + errors
error_bottom <- as.vector(values) - errors

arrows(bp, error_bottom, bp, error_top, angle = 90, code = 3, length = 0.1)


mtext("Average Duration (Months)", side = 2, line = 2, cex = 0.8)

xval <- numeric(6)
vec.12 <- as.vector(bp)
for (i in 1:6) {
  xval[i] <- mean(c(vec.12[2 * i],vec.12[2 * i -1]))
}

labval <- numeric(3)
for (i in 1:3) {
  labval[i] <- mean(c(xval[2 * i],xval[2 * i -1]))
}

mtext(rep(c("2015-2057", "2058-2100")), side = 1, line = 0, 
      at = xval, cex = 0.9, font = 2)

mtext(cats, side = 1, line = 1.2, 
      at = labval, cex = 1)

mtext("SPEI 6", side = 3, line = 0.2, cex = 1.4)


## now SPEI 12
var <- "SPEI_12"

mid.date <- as.Date("2058-01-01")
SPEI.126.mod <- drought_indices(mod.mean.SSP126, mid.date, var, -1)
SPEI.585.mod <- drought_indices(mod.mean.SSP585, mid.date, var, -1)

SPEI.126.mod[["stats"]] <- event_stats(SPEI.126.mod)
SPEI.585.mod[["stats"]] <- event_stats(SPEI.585.mod)

SPEI.126.ext <- drought_indices(mod.mean.SSP126, mid.date, var, -1.5)
SPEI.585.ext <- drought_indices(mod.mean.SSP585, mid.date, var, -1.5)

SPEI.126.ext[["stats"]] <- event_stats(SPEI.126.ext)
SPEI.585.ext[["stats"]] <- event_stats(SPEI.585.ext)

SPEI.126.sev <- drought_indices(mod.mean.SSP126, mid.date, var, -2)
SPEI.585.sev <- drought_indices(mod.mean.SSP585, mid.date, var, -2)

SPEI.126.sev[["stats"]] <- event_stats(SPEI.126.sev)
SPEI.585.sev[["stats"]] <- event_stats(SPEI.585.sev)

cats <- c("Moderate drought", "Severe drought", "Extreme drought")
cat_names <- c("SSP1-2.6 2015-2058", "SSP5-8.5 2015-2058", 
               "SSP1-2.6 2058-2100", "SSP5-8.5 2058-2100")

values <- matrix(c(
  mean(SPEI.126.mod$early_drought$Duration), 
  mean(SPEI.585.mod$early_drought$Duration),  
  mean(SPEI.126.mod$late_drought$Duration),  
  mean(SPEI.585.mod$late_drought$Duration),  
  mean(SPEI.126.sev$early_drought$Duration),  
  mean(SPEI.585.sev$early_drought$Duration),  
  mean(SPEI.126.sev$late_drought$Duration),  
  mean(SPEI.585.sev$late_drought$Duration),  
  mean(SPEI.126.ext$early_drought$Duration),  
  mean(SPEI.585.ext$early_drought$Duration),  
  mean(SPEI.126.ext$late_drought$Duration),  
  mean(SPEI.585.ext$late_drought$Duration)),
  nrow = 4, ncol = 3)

errors <- c(sd(SPEI.126.mod$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.mod$early_drought$Duration)), 
            sd(SPEI.585.mod$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.mod$early_drought$Duration)),  
            sd(SPEI.126.mod$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.mod$late_drought$Duration)),  
            sd(SPEI.585.mod$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.mod$late_drought$Duration)),  
            sd(SPEI.126.sev$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.sev$early_drought$Duration)),  
            sd(SPEI.585.sev$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.sev$early_drought$Duration)),  
            sd(SPEI.126.sev$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.sev$late_drought$Duration)),  
            sd(SPEI.585.sev$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.sev$late_drought$Duration)),  
            sd(SPEI.126.ext$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.ext$early_drought$Duration)),  
            sd(SPEI.585.ext$early_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.ext$early_drought$Duration)),  
            sd(SPEI.126.ext$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.126.ext$late_drought$Duration)),  
            sd(SPEI.585.ext$late_drought$Duration, na.rm = T)/
              sqrt(length(SPEI.585.ext$late_drought$Duration)))


# Create bar plot

cols <- c("lightblue2",
          "tomato1")

bp <- barplot(values, beside = TRUE, col = cols,
              legend.text = c("SSP1-2.6","SSP5-8.5"), 
              args.legend = list(x = "topright", cex = 1.2), 
              ylim = c(0, 10))

error_top <- as.vector(values) + errors
error_bottom <- as.vector(values) - errors

arrows(bp, error_bottom, bp, error_top, angle = 90, code = 3, length = 0.1)

mtext("Average Duration (Months)", side = 2, line = 2, cex = 0.8)

xval <- numeric(6)
vec.12 <- as.vector(bp)
for (i in 1:6) {
  xval[i] <- mean(c(vec.12[2 * i],vec.12[2 * i -1]))
}

labval <- numeric(3)
for (i in 1:3) {
  labval[i] <- mean(c(xval[2 * i],xval[2 * i -1]))
}

mtext(rep(c("2015-2057", "2058-2100")), side = 1, line = 0, 
      at = xval, cex = 0.9, font = 2)

mtext(cats, side = 1, line = 1.2, 
      at = labval, cex = 1)

mtext("SPEI 12", side = 3, line = 0.2, cex = 1.4)

# close file
if(writeFile != 'off') {
  dev.off()
}
##############################################

############### Figure 7 #####################
# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(8, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/Figure7.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/Figure7.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


mid.date <- as.Date("2058-01-01")
cols = hcl.colors(10, palette = "Zissou 1")

par(mfrow = c(4,2))
par(mar = c(2.2,4.3, 2, 2))

var <- "SPEI_6"


mod <- mod.mean.SSP126
month_to_date <- as.Date(paste(mod$months, "01", sep = "-"))
early_indices <- mod[[var]][, , which(month_to_date < mid.date)]
late_indices <- mod[[var]][, , which(month_to_date > mid.date)]

dims <- c(length(mod[["lon"]]), length(mod[["lat"]]), dim(mod[[var]])[3])
lat_array <- mod[["lat"]]
lon_array <- mod[["lon"]]

#SSP126 early matrix
mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- early_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("SSP1-2.6", side = 2, line = 3, cex = 1, font = 2)
mtext("2015-2057", side = 3, line = 0, cex = 1, font = 2)

#SSP126 late matrix

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- late_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("2058-2100", side = 3, line = 0, cex = 1, font = 2)

# par(mar = c(3,4, 1.5, 2))

#SSP585 early matrix

mod <- mod.mean.SSP585
month_to_date <- as.Date(paste(mod$months, "01", sep = "-"))
early_indices <- mod[[var]][, , which(month_to_date < mid.date)]
late_indices <- mod[[var]][, , which(month_to_date > mid.date)]

dims <- c(length(mod[["lon"]]), length(mod[["lat"]]), dim(mod[[var]])[3])
lat_array <- mod[["lat"]]
lon_array <- mod[["lon"]]

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- early_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("SSP5-8.5", side = 2, line = 3, cex = 1, font = 2)

#SSP585 late matrix

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- late_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)


#### SPEI 12

var <- "SPEI_12"


mod <- mod.mean.SSP126
month_to_date <- as.Date(paste(mod$months, "01", sep = "-"))
early_indices <- mod[[var]][, , which(month_to_date < mid.date)]
late_indices <- mod[[var]][, , which(month_to_date > mid.date)]

dims <- c(length(mod[["lon"]]), length(mod[["lat"]]), dim(mod[[var]])[3])
lat_array <- mod[["lat"]]
lon_array <- mod[["lon"]]

#SSP126 early matrix
mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- early_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("SSP1-2.6", side = 2, line = 3, cex = 1, font = 2)
mtext("2015-2057", side = 3, line = 0, cex = 1, font = 2)

#SSP126 late matrix

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- late_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("2058-2100", side = 3, line = 0, cex = 1, font = 2)

# par(mar = c(3,4, 1.5, 2))

#SSP585 early matrix

mod <- mod.mean.SSP585 
month_to_date <- as.Date(paste(mod$months, "01", sep = "-"))
early_indices <- mod[[var]][, , which(month_to_date < mid.date)]
late_indices <- mod[[var]][, , which(month_to_date > mid.date)]

dims <- c(length(mod[["lon"]]), length(mod[["lat"]]), dim(mod[[var]])[3])
lat_array <- mod[["lat"]]
lon_array <- mod[["lon"]]

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- early_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)
mtext("SSP5-8.5", side = 2, line = 3, cex = 1, font = 2)

#SSP585 late matrix

mat <- matrix(NA, ncol = dims[2], nrow = dims[1])

#number of events grid
for (i in seq_along(lat_array)) {
  for (j in seq_along(lat_array)) {
    
    line <- late_indices[j, i, ]
    
    if(all(is.nan(line)) == FALSE) {
      vec <- d_ind(line, -1)
      val <- sum(vec$Duration[vec$Duration > 3])
      mat[j, i] <- val
    }
  }
}

mat[mat == 0] = NA

p <- raster(mat, 
            xmn=min(mod[["lon"]]), xmx=max(mod[["lon"]]),
            ymn=min(mod[["lat"]]), ymx=max(mod[["lat"]]), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(p, ylim = c(41.5, 43), col = cols, legend.args = list(text = '(months)', cex = 0.5))
mtext("Longitude", side = 1, line = 2, cex = 0.8)
mtext("Latitude", side = 2, line = 2, cex = 0.8)

# close file
if(writeFile != 'off') {
  dev.off()
}

##############################################


