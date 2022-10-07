noaa_data <- function() {
  #noaa.df  <- read_csv('data/abe_daily.csv', show_col_types=FALSE)
  # noaa.df  <- file %>%
  #   read_csv(show_col_types=FALSE) %>%
  #   drop_na(tmax)
  
  #date_min <- noaa.df %>% pull(date) %>% last()
  
  noaa <- meteo_tidy_ghcnd(
    stationid='USW00014737',
    var=c('prcp','tavg','tmax','tmin','snow','snwd')
  )
  
  noaa <- noaa %>% drop_na(tmax)
  
  return(noaa)
  
  # noaa.df %>%
  #   bind_rows(noaa)
}