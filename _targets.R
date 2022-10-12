library(targets)
library(rnoaa)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse','rnoaa','lubridate','rdrop2','patchwork','ggrepel'))

source("src/noaa_data.R")
source("src/noaa_plot.R")
source("src/snowfall_plot.R")
source("src/temperature_plot.R")
source("src/dropbox.R")

tar_delete(noaa.df)
meteo_clear_cache(force = TRUE)

list(
  tar_target(
    noaa.df,
    noaa_data()
    ),
  tar_target(
    temperature.plot,
    temperature_plot(noaa.df),
    format = 'rds'
  ),
  tar_target(
    legend.plot, {
      temperature.plot
      legend_plot()
    }
  ),
  tar_target(
    combined.plot,
    combine_plots(temperature.plot,legend.plot,'temperature.jpeg')
  ),
  tar_target(
    snowfall.plot,
    snowfall_plot(noaa.df),
    format = 'rds'
  ),
  tar_target(
    snowfall.output,{
      snowfall.plot
      output_snow_plot('snowfall.jpeg')
    }
  ),
  tar_target(
    noaa.plot,
    noaa_plot(noaa.df),
    format='rds'
  ),
  tar_target(
    noaa.output,{
      noaa.plot
      output_noaa_plot('noaa_plot.png')
    }
  ),
  tar_target(
    dropbox.transfer,{
      combined.plot
      snowfall.output
      noaa.output
      transfer_plots('noaa_plot.png','snowfall.jpeg','temperature.jpeg')
    }
  )
)

