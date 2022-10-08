noaa_data <- function() {
  noaa <- meteo_tidy_ghcnd(
    stationid='USW00014737',
    var=c('prcp','tavg','tmax','tmin','snow','snwd')
  )
  
  noaa <- noaa %>% drop_na(tmax)
  
  return(noaa)
}