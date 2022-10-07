update_data <- function(file) {
  #noaa.df  <- read_csv('data/abe_daily.csv', show_col_types=FALSE)
  noaa.df  <- file %>%
    read_csv(show_col_types=FALSE) %>%
    drop_na(tmax)
  
  date_min <- noaa.df %>% pull(date) %>% last()
  
  noaa <- meteo_tidy_ghcnd(
    stationid='USW00014737',
    var=c('prcp','tavg','tmax','tmin','snow','snwd'),
    date_min=date_min
  )
  
  noaa <- noaa %>% drop_na(tmax)
  
  noaa.df %>%
    bind_rows(noaa)
}