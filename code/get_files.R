
###################################

## example input into getvar:
# gcm_download_data(location = getwd(),
#                   model = 'CESM2',
#                   scenario = 'ssp126',
#                   variable = 'pr',
#                   years = c(2020),
#                   roi = c(-75.25, -69.75, 40, 43.25),
#                   method = 'curl')

getvar <- function(vars, yrs, bounds, scn, mdl){
  for (var in vars) {
    for (s in scn) {
      RClimChange::gcm_download_data(location = getwd(), 
                      model = mdl, 
                      scenario = s, 
                      variable = var, 
                      years = yrs, 
                      roi = bounds, 
                      method = 'curl')
    }
  }
}


variables <- c('pr', 'tasmin', 'tasmax', 'rlds')


getvar(vars = 'rlds', yrs = c(1980:1985),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('historical'),
       mdl = 'CanESM5'
)

getvar(vars = c('tasmax', 'rlds'), yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'GISS-E2-1-G'
)

getvar(vars = variables, yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'CanESM5'
)

getvar(vars = variables, yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'IPSL-CM6A-LR'
)

getvar(vars = variables, yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'GFDL-ESM4'
)

getvar(vars = variables, yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'BCC-CSM2-MR'
)

getvar(vars = variables, yrs = c(2051:2100),
       bounds = c(-75.25, -69.75, 40, 43.25), 
       scn = c('ssp126','ssp585'),
       mdl = 'MIROC6'
)

# continue with tasmax

######Get Historical data ###################

# Get historic data for comparison
lat_min <- 40  # Minimum latitude
lat_max <- 43.25  # Maximum latitude
lon_min <- -75.25 # Minimum longitude
lon_max <- -69.75 # Maximum longitude

bbox <- matrix(c(lon_min, lat_min,
                 lon_max, lat_min,
                 lon_max, lat_max,
                 lon_min, lat_max,
                 lon_min, lat_min), ncol = 2, byrow = TRUE)

bbox_sf <- sf::st_polygon(list(bbox))
bbox_sf <- sf::st_sfc(bbox_sf, crs = 4326)

gridmet = climateR::getGridMET(AOI = bbox_sf,
                     varname = c("pr", "tmmn", "tmmx", "srad"),
                     startDate = "1972-01-01",
                     endDate  = "2014-12-31")

s <- lapply(gridmet, wrap)
saveRDS(s, 'gridmet.rds')
# # x <- readRDS("wrap.rds")
# # x <- lapply(x, rast)
# 
# # 
# # 
# # ####
PET = climateR::getGridMET(AOI = bbox_sf,
                     varname = c("pet"),
                     startDate = "1972-01-01",
                     endDate  = "2014-12-31")

s <- lapply(PET, wrap)
saveRDS(s, 'PET.rds')
rm(s)