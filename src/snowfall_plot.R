library(tidyverse)
library(lubridate)

snowfall_plot <- function(data) {
  wy.months <- c(10:12)
  
  wy <- tibble(
    xdate = seq.Date(as.Date('2022-10-01'),
                     as.Date('2023-04-30'),
                     'day'),
    dowy = row_number(xdate)
  )
  
  snow <- data %>%
    dplyr::select(date,snow) %>%
    mutate(doy=yday(date)) %>%
    filter(date >= as.Date('1948-10-01')) %>%
    mutate(wy=case_when(
      month(date) %in% wy.months ~ year(date) + 1,
      TRUE ~ year(date))) %>%
    group_by(wy) %>%
    mutate(snow=case_when(
      is.na(snow) ~ 0,
      TRUE ~ snow
    )) %>%
    mutate(dowy=row_number(),
           snow_in=cumsum(snow/25.4)) %>%
    tibble()
  
  snow.current <- snow %>%
    filter(wy==2022) %>%
    mutate(period='Current season (2022-23)') %>%
    select(dowy,snow_in,period) %>%
    left_join(wy,by='dowy') %>%
    mutate(snow_in=round(snow_in,1)) %>%
    filter(xdate <= Sys.Date()-1) %>%
    tibble()
  
  snow.max <- snow %>%
    filter(wy==1996) %>%
    filter(dowy<=212) %>%
    mutate(period='High season (1995-96)') %>%
    select(dowy,snow_in,period) %>%
    left_join(wy,by='dowy') %>%
    mutate(snow_in=round(snow_in,1)) %>%
    tibble()
  
  snow.min <- snow %>%
    filter(wy==2020) %>%
    filter(dowy<=212) %>%
    mutate(period='Low season (2019-20)') %>%
    select(dowy,snow_in,period) %>%
    left_join(wy,by='dowy') %>%
    mutate(snow_in=round(snow_in,1)) %>%
    tibble()
  
  snow.avg <- snow %>%
    group_by(dowy) %>%
    filter(dowy<=212) %>%
    summarize(snow_in=round(mean(snow_in, na.rm=TRUE),1)) %>%
    mutate(period='Average season') %>%
    left_join(wy,by='dowy') %>%
    tibble()
  
  snow.df <- snow.max %>%
    add_row(snow.min) %>%
    add_row(snow.avg) %>%
    add_row(snow.current)
  
  snow.max <- snow.max %>% rename(max_in=snow_in)
  snow.min <- snow.min %>% rename(min_in=snow_in)
  snow.avg <- snow.avg %>% rename(avg_in=snow_in)
  snow.current <- snow.current %>% rename(current_in=snow_in)
  
  snow.df2 <- snow.max %>%
    left_join(snow.avg,by='dowy') %>%
    left_join(snow.min,by='dowy') %>%
    left_join(snow.current,by='dowy') %>%
    dplyr::select(dowy,max_in,min_in,avg_in,current_in) %>%
    filter(dowy <= 212) %>%
    left_join(wy,by='dowy') %>%
    mutate(percent_avg=round((current_in/avg_in)*100,1))
  
  #pts <- snow.df2 %>% slice_tail(n=1)
  pts <- snow.df2 %>% filter(xdate==Sys.Date()-1)
  #pts <- snow.df2 %>% filter(xdate==as.Date('2023-05-01')-1)
  
  
  pts <- pts %>% 
    pivot_longer(cols=2:5,names_to='measure',values_to='snow_in') %>%
    select(xdate,measure,snow_in)
  
  avg <- pts$snow_in[pts$measure=='avg_in']
  
  pts <- pts %>% 
    mutate(per_avg=round((snow_in/avg)*100,1)) %>%
    mutate(measure=case_when(
      measure == 'max_in' ~ 'High season (1995-96)',
      measure == 'min_in' ~ 'Low season (2019-20)',
      measure == 'avg_in' ~ 'Average season',
      TRUE ~ 'Current season (2022-23)'
    )) %>%
    mutate(
      snow_in=case_when(
        is.na(snow_in) ~ 0,
        TRUE ~ snow_in),
      per_avg=case_when(
        is.na(per_avg) ~ 0,
        TRUE ~ snow_in
      ))
  
  p.snow <- ggplot() +
    geom_hline(yintercept=c(20,40,60,80),size=0.25,alpha=0.2) +
    geom_ribbon(data=snow.df2,
                aes(x=xdate,
                    ymin=case_when(
                      max_in <= avg_in ~ 0,
                      TRUE ~ avg_in),
                    ymax=max_in),
                alpha=0.3,
                fill='#1c4e80') +
    geom_ribbon(data=snow.df2,
                aes(x=xdate,
                    ymin=case_when(
                      avg_in <= min_in ~ 0,
                      TRUE ~ min_in),
                    ymax=avg_in),
                alpha=0.3,
                fill='#5c9cdb') +
    geom_ribbon(data=snow.df2,
                aes(x=xdate,
                    ymin=0,
                    ymax=min_in),
                alpha=0.3,
                fill='white') +
    geom_line(data=snow.df,
              aes(x=xdate,
                  y=snow_in,
                  color=period,
                  size=period,
                  group=factor(period,
                               levels=c('High season (1995-96)',
                                        'Average season',
                                        'Low season (2019-20)',
                                        'Current season (2022-23)')))) +
    geom_point(data=pts,aes(xdate,snow_in,color=measure),size=2) +
    geom_text(data=pts[pts$measure=='Current season (2022-23)',],
              aes(#x=as.Date("2023-04-05"),
                x=xdate,
                y=snow_in,
                label=paste0(per_avg,'% of average')),
              size=7*0.36,
              hjust=1,
              nudge_y=3) +
    labs(x='Date',
         y='Total snow (inches)',
         title='Lehigh Valley Seasonal Snowfall',
         subtitle=paste("How the Lehigh Valley's cumulative 2022-23 seasonal snowfall, measured at LVIA, compares to the historical mean,\nrecord high, and record low years since consistent records began in 1948. Updated on ", format(Sys.Date(), "%B %d, %Y."))) +
    scale_x_date(date_labels="%b %d",
                 date_breaks='1 month',
                 expand=c(0.05,0.05)) +
    scale_color_manual(values=c('Current season (2022-23)'='black',
                                'High season (1995-96)'='#1c4e80',
                                'Average season'='#737475',
                                'Low season (2019-20)'='#5c9cdb')) +
    scale_size_manual(values=c('Current season (2022-23)'=1,
                               'High season (1995-96)'=0.25,
                               'Average season'=0.25,
                               'Low season (2019-20)'=0.25)) +
    scale_y_continuous(limits=c(0,80),
                       expand=c(0.01,0.01)) +
    theme(panel.background=element_blank(),
          panel.grid=element_blank(),
          plot.title=element_text(size=9, hjust=0, color="#4e4d47", 
                                  margin=margin(b = 0.2, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0, color="#4e4d47", face="italic",
                                     margin=margin(b = 0.4, t = 0.1, l = 2, unit = "cm")),
          strip.background=element_rect(color="black", size=0.25),
          axis.line=element_line(size=0.25),
          axis.ticks=element_line(size=0.25),
          axis.text=element_text(size=7),
          axis.title=element_text(size=8),
          axis.title.x=element_blank(),
          plot.caption=element_text(size=6),
          legend.justification=c(0,0.8),
          legend.title=element_blank(),
          legend.background=element_blank(),
          legend.position=c(0,0.97),
          legend.direction='vertical',
          legend.text=element_text(size=6, color="black"),
          legend.key=element_blank(),
          legend.key.width=unit(2,"line"),
          legend.key.size = unit(1, 'lines'))
}

output_snow_plot <- function(file_out){
  tar_load(snowfall.plot)
  
  print(snowfall.plot)
  
  ggsave(file_out,
         width=7.8,
         height=4.5,
         units="in",
         dpi=600)
}

# Update data on Dropbox --------------------------------------------------
snowfall_drop <- function(){
  # Authenticate dropbox with stored RDS token
  drop_auth(rdstoken='drop.RDS')
  
  # Write updated data to dropbox
  drop_upload('snowfall.jpeg', path='/r/projects/climate/output')
}

