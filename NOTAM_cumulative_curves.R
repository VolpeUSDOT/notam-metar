# Setup ----
# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists

working_dir <- ifelse(grepl('lylet', path.expand('~/')),
                      'C:/Users/lylet/OneDrive/Documents/NOTAM/input_output',
                      '~/OneDrive/Documents/NOTAM/input_output')

setwd(working_dir)

if(!file.exists("NOTAM_Freq_w_busy_days.RData")){
  cat('Attempting to source the script to generate busy_periods data frame')
  source('NOTAM_analysis_busy_periods.R')
}

load("NOTAM_Freq_w_busy_days.RData")

library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(dplyr)
library(reshape2)

# Define functions ----

compute_staff_reqd <- function(day, processing_time_in_minutes = 3, hourly_staff_model) {
  
  #begin: staffing paramaters. These are now arguments in the function
  average_NOTAM_processing_time_in_minutes <- processing_time_in_minutes # Defaults to 3 min, per Bruce W, each processor can process 480 NOTAMs per day
  # day = as.Date("2019-08-07", tz = "UTC") #testing
  #end: staffing parameters
  
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_processed <- staff_required <- vector(length = minutes_per_day)
  arrivals_on_day <- UniqueInteractions[UniqueInteractions$date == day,]
  # ordered_arrivals <- arrivals_on_day[order(arrivals_on_day$datetimes),]
  
  minute <- day_as_datetime + (minute-1)*60
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / average_NOTAM_processing_time_in_minutes 
  minutes_per_hour = 60L
  minute_by_minute_staff_model = rep(hourly_staff_model, minutes_per_hour)

  s_df <- data.frame(minute, msgs_in_queue = -1, arrivals, msgs_processed, staff_required = NA, staff_available = minute_by_minute_staff_model, capacity = -1)
  
  s_df$capacity = s_df$staff_available * capacity_in_NOTAMs_per_minute_per_staff
  
  for(n in 1:minutes_per_day){
    s_df$arrivals[n] = sum(arrivals_on_day$datetimes == s_df$minute[n])
    s_df$cml_arrivals[n] = sum(arrivals_on_day$datetimes <= s_df$minute[n])
    if (n==1){
      s_df$msgs_in_queue[n] = max(0, s_df$arrivals[n] - s_df$capacity[n])
      s_df$msgs_processed[n] = s_df$cml_msgs_processed[n] = min(s_df$arrivals[n], s_df$capacity[n])
    }
  }
  
  for(n in 2:minutes_per_day){
    s_df$msgs_processed[n] = min(s_df$capacity[n], s_df$arrivals[n] + s_df$msgs_in_queue[n-1])
    s_df$cml_msgs_processed[n] = s_df$cml_msgs_processed[n-1] + s_df$msgs_processed[n]  
    s_df$msgs_in_queue[n] = s_df$cml_arrivals[n] - s_df$cml_msgs_processed[n] 
  }
  
  s_df$staff_required = ceiling(s_df$arrivals / average_NOTAM_processing_time_in_minutes)
  
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_processed"))
  
  # print(head(s_df_long))
  #begin: plot cumulative curves with estimated instantaneous staffing level
  p <- ggplot(data = s_df_long) + xlab("minute") 
  
  pc <- p + 
    aes(minute, value, colour = variable) +
    aes(minute, value, linetype = variable) +    
    geom_step() + 
    guides(colour = "legend", linetype = FALSE) +
    scale_color_discrete(name = NULL, labels = c("NOTAMs Arrived for Processing", "NOTAMs Processed")) +
    scale_linetype_discrete() + # scale_linetype_discrete(name = NULL, labels = c(NULL, NULL)) +
    theme(legend.position = "top") +
    ylab("Cumulative NOTAMs") +
    ggtitle(day)
    
  print(pc)
  
  # ps <- p + 
  #   geom_point(aes(x = minute, y = staff_required)) + 
  #   ylab("instantaneous staffing requirement")
  
  # g1 <- ggplotGrob(pc)
  # g2 <- ggplotGrob(ps)
  # g <- rbind(g1, g2, size = "first")
  # g$widths <- unit.pmax(g1$widths, g2$widths)
  # grid.newpage()
  # grid.draw(g)
  #end: plot cumulative curves with estimated instantaneous staffing level
  }

# Run busy day analysis ----

number_of_busy_days_to_analyze <- 1 # top_block_list_size

# save to PDF
pdf('NOTAM_Cumulative_Curves.pdf', width = 8, height = 8)

for(k in number_of_busy_days_to_analyze:1){
  # k = 1
  busy_periods$cumulative_count[k] = sum(busy_periods$block_count <= busy_periods$block_count[k])
  y <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$year + 1900
  m <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mon + 1
  d <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mday
  
  # Create beginning-of-day and end-of-day values for this day
  bod <- as.POSIXct(
    # print(
    paste(y, m, d, sep = "-")
    # )
    , tz = "UTC")
  eod <- bod + 60*60*24

  gp <- ggplot(data = orig_df[orig_df$hourly_bin_start > bod & orig_df$hourly_bin_start < eod, ],
         aes(x = hourly_bin_start,
             y = block_count)) +
    geom_point() +
    geom_line() +
    ggtitle(paste(bod, ' Rank:', busy_periods$block_rank_descending[k])) +
    theme_bw()
                    
  print(gp)
  
  day = as.Date(bod)
  hourly_staff_models = c("dynam_model_1", "dynam_model_2", "dynam_model_3", "uniform_5", "uniform_2")
  for(x in 1:length(staff_models)){
    compute_staff_reqd(day, processing_time_in_minutes = 3, orig_df[orig_df$hourly_bin_start > bod & orig_df$hourly_bin_start < eod,hourly_staff_models[x]] )  
  }
}

dev.off(); system('open NOTAM_Cumulative_Curves.pdf')

p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per 4-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest 4-hour blocks as a Cumulative Distribution Function")
)
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)

