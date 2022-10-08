## Recreating NOAA temperature and snowfall plots
  
This repository uses the [`targets` package](https://docs.ropensci.org/targets/) to build the plots. Run `tar_make()` to execute `_targets.R` .  

Install all packages needed to run: 
```
install.packages(c('tidyverse','rnoaa','lubridate','rdrop2','patchwork'))
```

Running `tar_make()` will produce: 

![temperature_plot](temperature_plot.jpeg)
