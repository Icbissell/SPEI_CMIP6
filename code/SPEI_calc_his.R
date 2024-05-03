
##############################################
library(ncdf4) 
library(lubridate)
library(plotrix)
library(terra)

# function to setup netcdf files
read_netCDF <- function(file_path, var_name) {
  nc_data <- nc_open(file_path)
  data <- ncvar_get(nc_data, varid = var_name)
  fill <- ncatt_get(nc_data, var_name, "_FillValue")
  data[data == fill$value] <- NA
  nc_close(nc_data)
  return(data)
}

month_mean <- function(l, varname) {
  
  start_time = Sys.time()
  
  date <- l[["date"]]
  var <- l[[varname]]
  
  month <- format(date, "%Y-%m")
  uniq <- unique(month)
  
  ind <- which(month == unique(month)[1])
  
  month_var <-  apply(var[,,1:ind[length(ind)]], c(1, 2), mean)
  
  for (u in uniq[-1]) {
    ind <- which(month == u)
    up_var <- apply(var[,,ind[1]:ind[length(ind)]], c(1, 2), mean)
    month_var <- abind::abind(month_var, up_var, along = 3)
  }
  
  new_name <- paste0("month_", varname)
  l[[new_name]] <- month_var
  if (!("months" %in% names(l))) {
    l[["months"]] <- uniq 
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

month_sum <- function(l, varname) {
  
  start_time = Sys.time()
  
  date <- l[["date"]]
  var <- l[[varname]]
  
  month <- format(date, "%Y-%m")
  uniq <- unique(month)
  
  ind <- which(month == unique(month)[1])
  
  month_var <-  apply(var[,,1:ind[length(ind)]], c(1, 2), sum)
  
  for (u in uniq[-1]) {
    ind <- which(month == u)
    up_var <- apply(var[,,ind[1]:ind[length(ind)]], c(1, 2), sum)
    month_var <- abind::abind(month_var, up_var, along = 3)
  }
  
  new_name <- paste0("month_", varname)
  l[[new_name]] <- month_var
  if (!("months" %in% names(l))) {
    l[["months"]] <- uniq 
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

vec_month_mean <- function(vec, date_vec) {
  
  start_time = Sys.time()
  
  month <- format(date_vec, "%Y-%m")
  uniq <- unique(month)
  
  ind <- which(month == unique(month)[1])
  
  month_var <-  mean(vec[1:ind[length(ind)]])
  
  for (u in uniq[-1]) {
    ind <- which(month == u)
    month_var <- c(month_var, mean(vec[ind[1]:ind[length(ind)]]))
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(month_var)
}

vec_month_sum <- function(vec, date_vec) {
  
  start_time = Sys.time()
  
  month <- format(date_vec, "%Y-%m")
  uniq <- unique(month)
  
  ind <- which(month == unique(month)[1])
  
  month_var <-  sum(vec[1:ind[length(ind)]])
  
  for (u in uniq[-1]) {
    ind <- which(month == u)
    month_var <- c(month_var, sum(vec[ind[1]:ind[length(ind)]]))
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(month_var)
}

vec_year_mean <- function(vec, date_vec) {
  
  start_time = Sys.time()
  
  year <- format(date_vec, "%Y")
  uniq <- unique(year)
  
  ind <- which(year == unique(year)[1])
  
  year_var <-  mean(vec[1:ind[length(ind)]])
  
  for (u in uniq[-1]) {
    ind <- which(year == u)
    year_var <- c(year_var, mean(vec[ind[1]:ind[length(ind)]]))
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(year_var)
}

vec_year_sum <- function(vec, date_vec) {
  
  start_time = Sys.time()
  
  year <- format(date_vec, "%Y")
  uniq <- unique(year)
  
  ind <- which(year == unique(year)[1])
  
  year_var <-  sum(vec[1:ind[length(ind)]])
  
  for (u in uniq[-1]) {
    ind <- which(year == u)
    year_var <- c(year_var, sum(vec[ind[1]:ind[length(ind)]]))
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(year_var)
}

# function to get yearly sums (required for SPEI calculation)
year_mean <- function(l, varname) {
  
  start_time = Sys.time()
  
  date <- l[["date"]]
  var <- l[[varname]]
  
  year <- format(date, "%Y")
  uniq <- unique(year)
  
  ind <- which(year == unique(year)[1])
  
  year_var <-  apply(var[,,1:ind[length(ind)]], c(1, 2), mean)
  
  for (u in uniq[-1]) {
    ind <- which(year == u)
    up_var <- apply(var[,,ind[1]:ind[length(ind)]], c(1, 2), mean)
    year_var <- abind::abind(year_var, up_var, along = 3)
  }
  
  new_name <- paste0("year_", varname)
  l[[new_name]] <- year_var
  
  if (!("years" %in% names(l))) {
    l[["years"]] <- uniq 
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# function to get yearly sums (required for SPEI calculation)
year_sum <- function(l, varname) {
  
  start_time = Sys.time()
  
  date <- l[["date"]]
  var <- l[[varname]]
  
  year <- format(date, "%Y")
  uniq <- unique(year)
  
  ind <- which(year == unique(year)[1])
  
  year_var <-  apply(var[,,1:ind[length(ind)]], c(1, 2), sum)
  
  for (u in uniq[-1]) {
    ind <- which(year == u)
    up_var <- apply(var[,,ind[1]:ind[length(ind)]], c(1, 2), sum)
    year_var <- abind::abind(year_var, up_var, along = 3)
  }
  
  new_name <- paste0("year_", varname)
  l[[new_name]] <- year_var
  
  if (!("years" %in% names(l))) {
    l[["years"]] <- uniq 
  }
  
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# function to call in variables
NEX_GDDP_fill_list <- function(nc.file.path, nc.file.vec, model.name, 
                               start.date, nc.var.vec) {
  l <- list()
  l[["name"]] <- model.name
  nc_data <- nc_open(file.path(nc.file.path, nc.file.vec[1]))
  lon <- ncvar_get(nc_data, "lon")
  lon[lon > 180] <- lon[lon > 180] - 360
  l[["lon"]] <- lon
  l[["lat"]] <- ncvar_get(nc_data, "lat", verbose = F)
  l[["date"]] <- as.Date(format(ncdf4.helpers::nc.get.time.series(f = nc_data, time.dim.name = "time"), 
                                "%Y-%m-%d"))
  nc_close(nc_data)
  for (i in 1:length(nc.var.vec)) {
    l[[nc.var.vec[i]]] <- read_netCDF(file.path(nc.file.path, nc.file.vec[i]), nc.var.vec[i])
    if(nc.var.vec[i] == "rlds") {
      l[["rlds"]] <- 8.64*10^-2*l[["rlds"]]
    }
  }
  
  return(l)
}

# function to get monthly means (required for SPEI calculation)
NEX_GDDP_var_means <- function(l, var_name_vec) {
  
  start_time = Sys.time()
  date <- l[["date"]]
  num_months <- as.numeric(interval(date[1], date[length(date)]) %/% months(1))
  
  for (var in var_name_vec) {
    if (var == "pr") {
      l <- month_sum(l, var)
      l <- year_sum(l, var)
    } else {
      l <- month_mean(l, var)
      l <- year_mean(l, var)
    }
  }
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# function to calculate PET
calc_PET <- function(l) {
  start_time = Sys.time()
  p_month_tasmin <- aperm(l[["month_tasmin"]], c(3, 1, 2))
  p_month_tasmax <- aperm(l[["month_tasmax"]], c(3, 1, 2))
  p_month_pr <- aperm(l[["month_pr"]], c(3, 1, 2))
  p_month_rad <- aperm(l[["month_rlds"]], c(3, 1, 2))
  p_lat <- matrix(rep(l[["lat"]], each = 22), nrow = 22)
  
  PET <- SPEI::hargreaves(Tmin = p_month_tasmin, Tmax = p_month_tasmax,
                          Pre = p_month_pr, lat = p_lat, Ra = p_month_rad, na.rm = TRUE, verbose = FALSE)
  
  PET <- aperm(PET, c(2, 3, 1))
  l[["month_PET"]] <- PET
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# function to calculate SPI
calc_SPI <- function(l, tscale) {
  start_time = Sys.time()
  pr <- aperm(l[["month_pr"]], c(3, 1, 2))
  spi_t <- SPEI::spi(pr, tscale, na.rm = TRUE, verbose = FALSE)
  spi <- aperm(spi_t$fitted, c(2, 3, 1))
  name <- paste0("SPI_", as.character(tscale))
  l[[name]] <- spi
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# function to calculate SPEI
calc_SPEI <- function(l, tscale) {
  start_time = Sys.time()
  pr <- aperm(l[["month_pr"]], c(3, 1, 2))
  PET <- aperm(l[["month_PET"]], c(3, 1, 2))
  SPEI <- SPEI::spei(pr - PET , tscale, na.rm = TRUE, verbose = FALSE)
  SPEI <- aperm(SPEI$fitted, c(2, 3, 1))
  name <- paste0("SPEI_", as.character(tscale))
  l[[name]] <- SPEI
  end_time = Sys.time()
  # print(end_time - start_time)
  return(l)
}

# get and plot means over time        
mean_time <- function(l, var_name) {
  
  mt <- c()
  
  if (class(l) == "list") {
    var <- l[[var_name]]
    for (i in 1:dim(var)[3]) {
      mt[i] <- mean(var[,,i], na.rm = TRUE)
    }
    
  } else if(class(l) == "array"){
    for (i in 1:dim(l)[3]) {
      mt[i] <- mean(l[,,i], na.rm = TRUE)
    }
  }
  
  return(mt)
}

ave_array <- function(arrays) {
  
  if(!all(sapply(arrays[-1], function(arr) identical(dim(arr), dim(arrays[[1]]))))) {
    stop("Arrays must have the same dimensions.")
  }
  
  avg <- array(NA, dim = dim(arrays[[1]]))
  
  for (i in seq_along(arrays[[1]])) {
    avg[i] <- mean(sapply(arrays, function(arr) arr[i]))
  }
  
  return(avg)
}

#start date in "2000-01-01" format
#order of nc.var.vec and nc.fil.vec must match

#IPSL model
IPSL.file.path <- "data/IPSL-CM6A-LR_historical"
IPSL.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                   "combo_tasmin.nc", "combo_tasmax.nc")
IPSL.var.vec <- c("pr", "rlds", 
                  "tasmin", "tasmax")

mod.IPSL <- NEX_GDDP_fill_list(nc.file.path = IPSL.file.path, 
                               nc.file.vec = IPSL.file.vec, 
                               model.name = "IPSL-CM6A-LR", 
                               start.date = "1980-01-01", 
                               nc.var.vec = IPSL.var.vec)
mod.IPSL <- NEX_GDDP_var_means(l = mod.IPSL, 
                               var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))


mod.IPSL <- calc_PET(mod.IPSL)
for (period in c(1, 6, 12)) {
  mod.IPSL <- calc_SPI(mod.IPSL, period)
  mod.IPSL <- calc_SPEI(mod.IPSL, period)
}



#GFDL model
GFDL.file.path <- "data/GFDL_ESM4_historical"
GFDL.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                   "combo_tasmin.nc", "combo_tasmax.nc")
GFDL.var.vec <- c("pr", "rlds", 
                  "tasmin", "tasmax")

mod.GFDL <- NEX_GDDP_fill_list(nc.file.path = GFDL.file.path, 
                               nc.file.vec = GFDL.file.vec, 
                               model.name = "GFDL_ESM4", 
                               start.date = "1980-01-01", 
                               nc.var.vec = GFDL.var.vec)
mod.GFDL <- NEX_GDDP_var_means(l = mod.GFDL, 
                               var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))
mod.GFDL <- calc_PET(mod.GFDL)
for (period in c(1, 6, 12)) {
  mod.GFDL <- calc_SPI(mod.GFDL, period)
  mod.GFDL <- calc_SPEI(mod.GFDL, period)
}

#MIROC6 model
MIR.file.path <- "data/MIROC6_historical"
MIR.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                  "combo_tasmin.nc", "combo_tasmax.nc")
MIR.var.vec <- c("pr", "rlds", 
                 "tasmin", "tasmax")

mod.MIR <- NEX_GDDP_fill_list(nc.file.path = MIR.file.path, 
                              nc.file.vec = MIR.file.vec, 
                              model.name = "MIROC6", 
                              start.date = "1980-01-01", 
                              nc.var.vec = MIR.var.vec)

mod.MIR <- NEX_GDDP_var_means(l = mod.MIR, 
                              var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))

mod.MIR <- calc_PET(mod.MIR)
for (period in c(1, 6, 12)) {
  mod.MIR <- calc_SPI(mod.MIR, period)
  mod.MIR <- calc_SPEI(mod.MIR, period)
}

#BCC-CSM2-MR model
BCC.file.path <- "data/BCC-CSM2-MR_historical"
BCC.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                  "combo_tasmin.nc", "combo_tasmax.nc")
BCC.var.vec <- c("pr", "rlds", 
                 "tasmin", "tasmax")

mod.BCC <- NEX_GDDP_fill_list(nc.file.path = BCC.file.path, 
                              nc.file.vec = BCC.file.vec, 
                              model.name = "BCC-CSM2-MR", 
                              start.date = "1980-01-01", 
                              nc.var.vec = BCC.var.vec)

mod.BCC <- NEX_GDDP_var_means(l = mod.BCC, 
                              var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))
mod.BCC <- calc_PET(mod.BCC)
for (period in c(1, 6, 12)) {
  mod.BCC <- calc_SPI(mod.BCC, period)
  mod.BCC <- calc_SPEI(mod.BCC, period)
}

#GISS-E2-1-G  model
GISS.file.path <- "data/GISS-E2-1-G_historical"
GISS.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                  "combo_tasmin.nc", "combo_tasmax.nc")
GISS.var.vec <- c("pr", "rlds", 
                 "tasmin", "tasmax")

mod.GISS <- NEX_GDDP_fill_list(nc.file.path = GISS.file.path, 
                              nc.file.vec = GISS.file.vec, 
                              model.name = "GISS-E2-1-G", 
                              start.date = "1980-01-01", 
                              nc.var.vec = GISS.var.vec)

mod.GISS <- NEX_GDDP_var_means(l = mod.GISS, 
                              var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))
mod.GISS <- calc_PET(mod.GISS)
for (period in c(1, 6, 12)) {
  mod.GISS <- calc_SPI(mod.GISS, period)
  mod.GISS <- calc_SPEI(mod.GISS, period)
}

#CanESM5 model
CAN.file.path <- "data/CanESM5_historical"
CAN.file.vec <- c("combo_pr.nc", "combo_rlds.nc", 
                   "combo_tasmin.nc", "combo_tasmax.nc")
CAN.var.vec <- c("pr", "rlds", 
                  "tasmin", "tasmax")

mod.CAN <- NEX_GDDP_fill_list(nc.file.path = CAN.file.path, 
                               nc.file.vec = CAN.file.vec, 
                               model.name = "CanESM5", 
                               start.date = "1980-01-01", 
                               nc.var.vec = CAN.var.vec)

mod.CAN <- NEX_GDDP_var_means(l = mod.CAN, 
                               var_name_vec = c("pr", "tasmin", "tasmax", "rlds"))
mod.CAN <- calc_PET(mod.CAN)
for (period in c(1, 6, 12)) {
  mod.CAN <- calc_SPI(mod.CAN, period)
  mod.CAN <- calc_SPEI(mod.CAN, period)
}

############ Create model average list ####

mod.list <- list("mod.GFDL" = mod.GFDL, "mod.IPSL" = mod.IPSL, 
                 "mod.BCC" = mod.BCC, "mod.MIR" = mod.MIR, 
                 "mod.GISS" = mod.GISS, "mod.CAN" = mod.CAN)

model_names <- names(mod.list)

# extract common dates
dates_char <- lapply(mod.list, function(x) as.character(x$date))
com_dates <- as.Date(Reduce(intersect, dates_char))

mod.mean <- list(
  "name" = "mod.mean",
  "lon" = mod.list[[1]]$lon,
  "lat" = mod.list[[1]]$lat,
  "date" = com_dates
)

var.vec <- c("rlds", "pr", "tasmin", "tasmax")

for (i in seq_along(mod.list)) {
  for (vec in var.vec) {
    mod.list[[i]][[vec]] <- mod.list[[i]][[vec]][,,mod.list[[i]]$date %in% com_dates]
  }
}

for (var in var.vec) {
  var.mod <- lapply(mod.list, `[[`, var)
  mod.mean[[var]] <- ave_array(var.mod)
}

mod.mean <- NEX_GDDP_var_means(l = mod.mean, 
                               var_name_vec = var.vec)

mod.mean <- calc_PET(mod.mean)

for (period in c(1, 6, 12)) {
  mod.mean <- calc_SPI(mod.mean, period)
  mod.mean <- calc_SPEI(mod.mean, period)
}


##### read in historical data ###################

gridmet <- readRDS(file = "data/gridmet.rds")
gridmet <- lapply(gridmet, terra::rast)
hist <- list()
hist[["date"]] <- as.array(time(gridmet$precipitation_amount))

gridmet$precipitation_amount <- aggregate(gridmet$precipitation_amount, fact=4)
gridmet$daily_mean_shortwave_radiation_at_surface <- aggregate(gridmet$daily_mean_shortwave_radiation_at_surface, fact=4)
gridmet$daily_minimum_temperature <- aggregate(gridmet$daily_minimum_temperature, fact=4)
gridmet$daily_maximum_temperature <- aggregate(gridmet$daily_maximum_temperature, fact=4)

PET <- readRDS(file = "data/PET.rds")
PET <- lapply(PET, terra::rast)
PET$daily_mean_reference_evapotranspiration_grass <- aggregate(PET$daily_mean_reference_evapotranspiration_grass, fact=4)

hist[["pr"]] <- as.array(gridmet$precipitation_amount)
hist[["rlds"]] <- as.array(gridmet$daily_mean_shortwave_radiation_at_surface)
hist[["tasmin"]] <- as.array(gridmet$daily_minimum_temperature)
hist[["tasmax"]] <- as.array(gridmet$daily_maximum_temperature)
hist[["lon"]] <- xFromCol(gridmet$precipitation_amount[[1]])
hist[["lat"]] <- yFromRow(gridmet$precipitation_amount[[1]])
hist[["PET"]] <- as.array(PET$daily_mean_reference_evapotranspiration_grass)

rm(gridmet)

hist[["tasmin"]] <- hist[["tasmin"]] - 273.15
hist[["tasmax"]] <- hist[["tasmax"]] - 273.15

#convert W m-2 to MJ m-2 d-1 incoming radiation
hist[["rlds"]] <- 8.64*10^-2*hist[["rlds"]] #only includes long (infrared)

hist <- month_sum(hist, "pr")
hist <- month_mean(hist, "rlds")
hist <- month_mean(hist, "tasmin")
hist <- month_mean(hist, "tasmax")
hist <- month_sum(hist, "PET")

for (period in c(1, 6, 12)) {
  hist <- calc_SPI(hist, period)
  hist <- calc_SPEI(hist, period)
}

####### calibrate entire grid ##########

source("fhempel.R")
var.vec <- c("pr", "tasmin", "tasmax", "rlds")

mod <- mod.mean
dims <- c(length(mod[["lon"]]), length(mod[["lat"]]), dim(mod[[vec]])[3])
lat_array <- mod[["lat"]]
lon_array <- mod[["lon"]]

cal.list <- list()
cal.list[["name"]] <- mod[["name"]]
cal.list[["date"]] <- mod[["date"]]
cal.list[["lat"]] <- mod[["lat"]]
cal.list[["lon"]] <- mod[["lon"]]


for (vec in var.vec) {
  temp.mod <- mod[[vec]]
  mat <- array(NaN, dim = dims)
  for (lat in lat_array) {
    for (lon in lon_array) {
        line <- temp.mod[which(abs(mod[["lon"]] - lon) == min(abs(mod[["lon"]] - lon)))[1], 
                         which(abs(mod[["lat"]] - lat) == min(abs(mod[["lat"]] - lat)))[1], ]

      mods <- data.frame(date = com_dates, var = line)
      
      obs <- data.frame(date = as.Date(hist$date), 
                        var = hist[[vec]][which(abs(hist[["lat"]] - lat) == min(abs(hist[["lat"]] - lat))), 
                                    which(abs(hist[["lon"]] - lon) == min(abs(hist[["lon"]] - lon))), ])
  
      if(all(is.nan(obs$var)) == FALSE) {
        
        cal <- fhempel(obs, mods)
        mat[which(lon_array == lon), which(lat_array == lat),] <- cal$var
        
      }
    }
  }
  cal.list[[vec]] <- mat
}

cal.list <- NEX_GDDP_var_means(l = cal.list, 
                               var_name_vec = var.vec)

cal.list <- calc_PET(cal.list)

for (period in c(1, 6, 12)) {
  cal.list <- calc_SPI(cal.list, period)
  cal.list <- calc_SPEI(cal.list, period)
}




