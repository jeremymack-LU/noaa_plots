library(patchwork)

temperature_plot <- function(data) {
  df <- data %>%
    dplyr::select(date,tmax,tmin)
  
  df[df$date==as.Date("1954-05-20"),3] <- NA
  
  df <- df %>%
    # Convert from tenths of degrees C
    mutate(tmax=tmax/10,
           tmin=tmin/10) %>%
    # Convert from C to F
    mutate(dmt=(tmax+tmin)/2,
           doy=yday(date),
           tmin=tmin* 1.8 + 32,
           tmax=tmax* 1.8 + 32,
           dmt=dmt* 1.8 + 32)
  
  monthly.means <- df %>%
    group_by(month(date), year(date)) %>%
    summarize(avg.dmt=mean(dmt, na.rm=TRUE),
              avg.max=mean(tmax, na.rm=TRUE),
              avg.min=mean(tmin, na.rm=TRUE)) %>%
    drop_na()
  
  colnames(monthly.means)[1:2] <- c("month","year")
  
  monthly.avg <- monthly.means %>%
    group_by(month) %>%
    summarize(avg.dmt=mean(avg.dmt, na.rm=TRUE),
              avg.max=mean(avg.max, na.rm=TRUE),
              avg.min=mean(avg.min, na.rm=TRUE))
  
  # Monthly records
  monthly.max.rec <- df %>%
    group_by(month(date)) %>%
    drop_na(tmax) %>%
    arrange(tmax) %>%
    slice(n()) %>%
    as_tibble() %>%
    mutate(month=month(date)) %>%
    dplyr::select(month,tmax)
  
  monthly.min.rec <- df %>%
    group_by(month(date)) %>%
    drop_na(tmin) %>%
    arrange(desc(tmin)) %>%
    slice(n()) %>%
    as_tibble() %>%
    mutate(month=month(date)) %>%
    dplyr::select(month,tmin)
  
  monthly.sum <- monthly.avg %>%
    left_join(monthly.max.rec, by="month") %>%
    left_join(monthly.min.rec, by="month")
  
  current.means <- monthly.means %>% filter(year==2022)
  
  labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  temp.p <- ggplot(monthly.sum, aes(x=month)) +
    geom_hline(yintercept=c(0,25,50,75,100),size=0.25,alpha=0.2) +
    geom_crossbar(aes(ymin=avg.min,ymax=avg.max,y=avg.dmt),
                  width=0.5,
                  size=0.25,
                  fill="lightgray") +
    geom_point(aes(y=tmax),
               shape=21,
               fill="#d1ce08") +
    geom_point(aes(y=tmin),
               shape=21,
               fill="blue") +
    geom_point(data=current.means,
               aes(x=month,y=avg.dmt),
               shape=21,
               fill="orange") +
    scale_x_continuous(breaks=seq(1,12,1),
                       labels=labels,
                       expand=c(0.02,0)) +
    labs(x="Month",
         y="Temperature (°F)",
         title='Lehigh Valley Monthly Temperature',
         subtitle="How the Lehigh Valley's mean monthly temperature, measured at LVIA, compares to the\nhistorical mean, record high, and record low years since consistent records began in 1948.") +
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
          legend.position=c(0,0.98),
          legend.direction='vertical',
          legend.text=element_text(size=6, color="black"),
          legend.key=element_blank(),
          legend.key.width=unit(2,"line"),
          legend.key.size = unit(1, 'lines'))
}

legend_plot <- function() {
  legend <- ggplot() +
    annotate(geom="rect", xmin=1.5, xmax=2.52, ymin=0, ymax=4,
             color="black", fill="lightgray", size=0.25) +
    annotate(geom="text",
             label="Mean daily\nmaximum temperature",
             x=2.6, hjust=0, y=4.2, vjust=1, size=7*0.36) +
    annotate(geom="text",
             label="Mean daily\nminimum temperature",
             x=2.6, hjust=0, y=-0.2, vjust=0, size=7*0.36) +
    annotate(geom="text",
             label="Mean daily\ntemperature",
             x=2.6, hjust=0, y=2, vjust=0.5, size=7*0.36) +
    annotate("segment", x=1.5, xend=2.52, y=2, yend=2) +
    annotate("point", x=2,y=-0.5,shape=21,fill="blue") +
    annotate(geom="text",
             label="Record minimum temperature",
             x=2.1, hjust=0, y=-0.5, vjust=0.5, size=7*0.36) +
    annotate("point", x=2,y=4.5,shape=21,fill="#d1ce08") +
    annotate(geom="text",
             label="Record maximum temperature",
             x=2.1, hjust=0, y=4.5, vjust=0.5, size=7*0.36) +
    annotate("segment", x=2, xend=2.58, y=3, yend=3, size=0.25) +
    annotate("point", x=2,y=3,shape=21,fill="orange") +
    annotate(geom="text",
             label="Mean daily\ntemperature in 2022",
             x=2.6, hjust=0, y=3, vjust=0.5, size=7*0.36) +
    scale_x_continuous(limits=c(1.45,4.5)) +
    theme_void() +
    theme(text=element_text(color="#22211d"),
          plot.background=element_rect(fill="white", color=NA),
          panel.background=element_rect(fill="white", color=NA),
          legend.background=element_rect(fill="white", color=NA))
}

combine_plots <- function(plot_left, plot_right, file_out){
  plot_left + plot_right + plot_layout(widths=c(3,1))
  
  ggsave(file_out,
         width=7.8,
         height=4.5,
         units="in",
         dpi=600)
}

# Update data on Dropbox --------------------------------------------------
temperature_drop <- function(file){
  # Authenticate dropbox with stored RDS token
  drop_auth(rdstoken='drop.RDS')
  
  # Write updated data to dropbox
  drop_upload('temperature.jpeg', path='/r/projects/climate/output')
}
