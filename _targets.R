library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse','rnoaa','lubridate','rdrop2','patchwork','ggrepel'))

source("src/noaa_data.R")
source("src/noaa_plot.R")
source("src/snowfall_plot.R")
source("src/temperature_plot.R")

list(
  tar_target(
    noaa.df,
    noaa_data()
    ),
  tar_target(
    temperature.plot,
    temperature_plot(noaa.df)
  ),
  tar_target(
    legend.plot,
    legend_plot()
  ),
  tar_target(
    combined.plot,
    combine_plots(plot_left=temperature.plot,
                  plot_right=legend.plot,
                  'temperature_plot.jpeg')
  ),
  tar_target(
    snowfall.plot,
    snowfall_plot(noaa.df)
  ),
  tar_target(
    snowfall.output,
    output_snow_plot('snowfall_plot.jpeg')
  ),
  tar_target(
    noaa.plot,
    noaa_plot(noaa.df)
  ),
  tar_target(
    noaa.output,
    output_noaa_plot('noaa_plot.jpeg')
  )
)