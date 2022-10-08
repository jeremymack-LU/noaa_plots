library(tidyverse)
library(lubridate)
library(ggrepel)

noaa_plot <- function(data) {
  data[data$date==as.Date("1954-05-20"),3] <- NA
  
  df <- data %>%
    mutate(year=year(date),
           month=month(date)) %>%
    # Convert from tenths of degrees C
    mutate(tmax=tmax/10,
           tmin=tmin/10) %>%
    # Convert from C to F
    mutate(dmt=(tmax+tmin)/2,
           doy=yday(date),
           tmin=tmin* 1.8 + 32,
           tmax=tmax* 1.8 + 32,
           dmt=dmt* 1.8 + 32)
  
  year.to.plot <- max(df$year)
  
  this.year <- df %>%
    filter(year == year.to.plot)
  
  this.year <- this.year[!is.na(this.year$tmax), ]
  
  last.date <- max(this.year$date)
  
  daily.summary.stats <- df %>%
    filter(year != year.to.plot) %>%
    select(doy, prcp, tmax, tmin) %>%
    pivot_longer(cols = -doy) %>%
    group_by(doy, name) %>%
    summarise(max = max(value, na.rm = T),
              min = min(value, na.rm = T),
              x5 = quantile(value, 0.05, na.rm = T),
              x10 = quantile(value, 0.1, na.rm = T),
              x20 = quantile(value, 0.2, na.rm = T),
              x40 = quantile(value, 0.4, na.rm = T),
              x60 = quantile(value, 0.6, na.rm = T),
              x80 = quantile(value, 0.8, na.rm = T),
              x90 = quantile(value, 0.9, na.rm = T),
              x95 = quantile(value, 0.95, na.rm = T)) %>%
    ungroup()
  
  # month breaks
  month.breaks <- df %>%
    filter(year == 2019) %>%
    group_by(month) %>%
    slice_min(order_by = doy, n = 1) %>%
    ungroup() %>%
    select(month, doy) %>%
    mutate(month_name = month.abb)
  
  record.status.this.year <- this.year %>%
    select(doy, prcp, tmax, tmin) %>%
    pivot_longer(cols = -doy, values_to = "this_year") %>%
    inner_join(daily.summary.stats %>% select(-starts_with("x"))) %>%
    mutate(record_status = case_when(
      this_year > max ~ "max",
      this_year < min ~ "min",
      TRUE ~ "none"
    )) %>%
    filter(record_status != "none")
  
  max.graph <- daily.summary.stats %>%
    filter(name == "tmax") %>%
    ggplot(aes(x = doy)) +
    # draw vertical lines for the months
    geom_vline(xintercept = c(month.breaks$doy, 365),
               linetype = "dotted", lwd = 0.2) +
    # ribbon between the lowest and 5th, 95th and max percentiles
    geom_ribbon(aes(ymin = min, ymax = max),
                fill = "#bdc9e1") +
    # ribbon between the 10th and 20th, 80th to 95th percentiles
    geom_ribbon(aes(ymin = x10, ymax = x90),
                fill = "#74a9cf") +
    # line for this year's values
    geom_line(data = this.year,
              aes(y = tmax), lwd = 0.5) +
    # points for maximum records set this year
    geom_point(data = filter(record.status.this.year, 
                             name == "tmax",
                             record_status == "max"),
               aes(y = this_year), color = "red") +
    # points for minimum records set this year
    geom_point(data = filter(record.status.this.year,
                             name == "tmax",
                             record_status == "min"),
               aes(y = this_year), color = "blue") +
    scale_y_continuous(breaks = seq(-10, 100, 10),
                       labels = scales::unit_format(suffix = "°"),
                       expand = expansion(0.01),
                       name = NULL,
                       sec.axis = dup_axis()) +
    scale_x_continuous(expand = expansion(0),
                       breaks = month.breaks$doy + 15,
                       labels = month.breaks$month_name,
                       name = NULL) +
    labs(title = "Daily High Temperature at the Lehigh Valley International Airport",
         subtitle = paste("The line shows daily highs for",
                          paste0(lubridate::year(last.date), "."),
                          "The ribbons cover the",
                          "historical range. The last date shown is", 
                          format(last.date, "%b %d, %Y.")),
         caption = paste("\nRecords begin on January 1, 1948.",
                         "This graph was last updated on", format(Sys.Date(), "%B %d, %Y."))) +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "linen",
                                         colour = "linen"),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 9),
          plot.subtitle=element_text(size=8),
          plot.caption=element_text(size=6),
          axis.ticks = element_blank())
  
  legend.df <- daily.summary.stats %>%
    filter(doy %in% 165:201,
           name == "tmax") %>%
    mutate(max = max - 60,
           min = min - 60,
           x5 = x5 - 60,
           x10 = x10 - 60,
           x20 = x20 - 60,
           x40 = x40 - 60,
           x60 = x60 - 60,
           x80 = x80 - 60,
           x90 = x90 - 60,
           x95 = x95 - 60)
  
  legend.line.df <- tibble(
    doy = 165:201,
    temp = case_when(
      doy == 165 ~ legend.df$x40[legend.df$doy == 165],
      doy == 168 ~ legend.df$x40[legend.df$doy == 165] + 3,
      doy == 172 ~ legend.df$x40[legend.df$doy == 165] - 4,
      doy == 177 ~ legend.df$min[legend.df$doy == 177] - 1,
      doy == 180 ~ legend.df$x20[legend.df$doy == 180] - 1,
      doy == 182 ~ legend.df$x60[legend.df$doy == 182] + 1,
      doy == 185 ~ legend.df$x60[legend.df$doy == 185] - 6,
      doy == 189 ~ legend.df$max[legend.df$doy == 189] + 1,
      doy == 194 ~ legend.df$x60[legend.df$doy == 194],
      doy == 198 ~ legend.df$x40[legend.df$doy == 198],
      doy == 201 ~ legend.df$x60[legend.df$doy == 201],
      TRUE ~ NA_real_
    )
  ) %>%
    filter(!is.na(temp))
  
  legend.labels <- legend.df %>%
    pivot_longer(cols = c(max, min, starts_with("x")),
                 names_to = "levels") %>%
    mutate(label = case_when(
      levels == "max" ~ "max",
      levels == "min" ~ "min",
      levels == "x95" ~ "95th percentile of past years",
      TRUE ~ paste0(str_sub(levels, 2, -1), "th percentile")
    )) %>%
    mutate(filter_day = ifelse(
      levels %in% c("max", "x80", "x40", "x5"),
      min(doy),
      max(doy)
    )) %>%
    filter(doy == filter_day) %>%
    filter(levels=='x10'|levels=='x90')
  
  ##  Add legend
  max.graph2 <- max.graph +
    # ribbon between the lowest and 5th percentiles
    geom_ribbon(data = legend.df,
                aes(ymin = min, ymax = max),
                fill = "#bdc9e1") +
    # ribbon between the lowest and 5th percentiles
    geom_ribbon(data = legend.df,
                aes(ymin = x10, ymax = x90),
                fill = "#74a9cf") +
    geom_line(data = legend.line.df, aes(y = temp), lwd = 0.7) +
    geom_point(aes(x = 177, y = legend.line.df$temp[legend.line.df$doy == 177]),
               color = "blue") +
    geom_point(aes(x = 189, y = legend.line.df$temp[legend.line.df$doy == 189]),
               color = "red") +
    geom_text(aes(x = 177, y = legend.line.df$temp[legend.line.df$doy == 177] - 4,
                  label = "all-time record low set this year"),
              hjust = 0, size = 2) +
    geom_text(aes(x = 189, y = legend.line.df$temp[legend.line.df$doy == 189] + 3,
                  label = "all-time record high set this year"),
              hjust = 0, size = 2) +
    ggrepel::geom_text_repel(data = filter(legend.labels,
                                           filter_day == max(filter_day)),
                             aes(y = value, label = label),
                             min.segment.length = 0, size = 2,
                             direction = "y", hjust = 0, nudge_x = 5)
}

output_noaa_plot <- function(file_out){
  tar_load(noaa.plot)
  
  print(noaa.plot)
  
  ggsave(file_out,
         width=8,
         height=4,
         units='in',
         dpi=600)
}

