# Setup ----
# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists

# Adapted for FNS reports of NOTAMs, with service area added

if(!file.exists("FNS_NOTAM_Freq_w_busy_days.RData")){
  cat('Attempting to source the script to generate busy_periods data frame')
  source('NOTAM_FNS_Analysis.Rmd')
}

load("FNS_NOTAM_Freq_w_busy_days.RData")

library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(dplyr)
library(reshape2)
library(tidyr)

# Find percentile days ----
day_sums = orig_dt_hr %>%
  ungroup() %>%
  group_by(Region, date) %>%
  summarize(daily_count = sum(hourly_count))

# Now find the quantile days for each region

pctile_days = day_sums %>%
                ungroup() %>%
                group_by(Region) %>%
                summarize(quantile = seq(0, 1, 0.1),
                          daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))

ninetieth = pctile_days %>% filter(quantile == 0.9)

# Which days are the 90th percentile demand day by service area?

ninetieth_days = vector()

for(r in unique(day_sums$Region)){
  dr = day_sums %>% 
    filter(Region == r) 
  
  pctile_days_r = pctile_days %>% 
    filter(Region == r) 
  
  ninetieth_r = pctile_days_r %>% filter(quantile == 0.9)
  hundredth_r = pctile_days_r %>% filter(quantile == 1)  
  
  dr = dr %>%
    filter(daily_count >=  ninetieth_r$daily_count & 
             daily_count <  hundredth_r$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_r$daily_count) %>%
    filter(proximity_to_ninetieth == min(proximity_to_ninetieth))
  
  class(dr) = 'data.frame'
  
  ninetieth_days = rbind(dr, ninetieth_days)

  }

ninetieth_days


# Define functions ----

compute_staff_reqd <- function(day, processing_time_in_minutes = 3, hourly_staff_model, study_period_midpoint_utc = mean(dt$yr_hour), study_length_in_minutes = 7*24*60,
                               Region = 'Western') {
  
  # day = as.Date("2019-04-29")
  # hourly_staff_model = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9, 9, 9, 9, 9, 3, 3, 3)
  # hourly_staff_model = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 9, 3, 3, 3)
  
  # processing_time_in_minutes = 3
  # study_period_midpoint_utc = mean(dt$yr_hour)
  # study_length_in_minutes = 7*24*60
  # Region = 'Western'
  
  #rounding to a whole minute
  y <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$year + 1900
  m <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$mon + 1
  d <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$mday
  h <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$hour
  mi <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$min
  
  # begin: staffing parameters. These are now arguments in the function
  average_NOTAM_processing_time_in_minutes <- processing_time_in_minutes # Defaults to 3 min, per Bruce W, each processor can process 480 NOTAMs per day
  # end: staffing parameters
  
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  # Enforce UTC time zone. 
  day_as_datetime = lubridate::with_tz(day_as_datetime, tzone = "UTC")
  
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_departed_from_queue <- staff_required <- vector(length = minutes_per_day)
  
  arrivals_on_day <- dt[dt$Action.Date >= bod & dt$Action.Date < eod & dt$Region == Region,]
  
  print(paste("Total NOTAMS in Period: ", count(arrivals_on_day) ))
  minute <- day_as_datetime + (minute-1)*60
  
  # Convert minute to UTC for consistency
  minute <- as.POSIXct(minute, origin = "1970-01-01", tz = "UTC")
  
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / average_NOTAM_processing_time_in_minutes 
  minutes_per_hour = 60L
  seconds_per_minute = 60L
  minute_by_minute_staff_model = rep(hourly_staff_model, each = minutes_per_hour)
  
  s_df <- data.frame(minute, msgs_in_queue = NaN, arrivals, msgs_departed_from_queue, staff_required = NaN, staff_available = minute_by_minute_staff_model, capacity = -1)
  
  s_df$capacity = s_df$staff_available * capacity_in_NOTAMs_per_minute_per_staff
  
  for(n in 1:minutes_per_day){
    s_df$arrivals[n] = sum(arrivals_on_day$Action.Date == s_df$minute[n])
    s_df$cml_arrivals[n] = sum(arrivals_on_day$Action.Date <= s_df$minute[n])
    if (n==1){
      s_df$msgs_in_queue[n] = max(0, s_df$arrivals[n] - s_df$capacity[n])
      s_df$msgs_departed_from_queue[n] = s_df$cml_msgs_departed_from_queue[n] = min(s_df$arrivals[n], s_df$capacity[n])
    }
  }
  
  for(n in 2:minutes_per_day){
    s_df$msgs_departed_from_queue[n] = min(s_df$capacity[n], s_df$arrivals[n] + s_df$msgs_in_queue[n-1])
    s_df$cml_msgs_departed_from_queue[n] = s_df$cml_msgs_departed_from_queue[n-1] + s_df$msgs_departed_from_queue[n]  
    s_df$msgs_in_queue[n] = s_df$cml_arrivals[n] - s_df$cml_msgs_departed_from_queue[n] 
  }
  
  # s_df$staff_required = ceiling(s_df$arrivals / average_NOTAM_processing_time_in_minutes)
  # s_df$staff_doing_non_NOTAM_tasks = max(0, s_df$staff_available - s_df$staff_required)
  s_df$cml_staff_minutes_processing_NOTAMS = s_df$cml_msgs_departed_from_queue * average_NOTAM_processing_time_in_minutes
  total_staff_minutes = sum(s_df$staff_available)

  # begin: build up time-in-system distribution
  message_count = data.frame(message_count = seq(1:max(s_df$cml_arrivals)))
  
  s_df_arrivals = s_df %>% select(cml_arrivals, minute) %>% group_by(cml_arrivals) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_msgs_departed_from_queue = s_df %>% mutate(cml_msgs_departed_from_queue = ceiling(cml_msgs_departed_from_queue)) %>% select(cml_msgs_departed_from_queue, minute) %>% group_by(cml_msgs_departed_from_queue) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_new = message_count %>% 
    left_join(s_df_arrivals, by = c("message_count" = "cml_arrivals")) %>% 
    left_join(s_df_msgs_departed_from_queue, by = c("message_count" = "cml_msgs_departed_from_queue"), suffix = c(".arrivals", ".msgs_departed_from_queue")) %>%
    fill(minute.arrivals, minute.msgs_departed_from_queue, .direction = "up")
  s_df_new$minute.msgs_processed = s_df_new$minute.msgs_departed_from_queue + average_NOTAM_processing_time_in_minutes*seconds_per_minute
  s_df_new$delay = s_df_new$minute.msgs_departed_from_queue - s_df_new$minute.arrivals
  s_df_new$time_in_system = s_df_new$minute.msgs_processed - s_df_new$minute.arrivals
  print(s_df_new[which.max(s_df_new$time_in_system),])
  # print(s_df_new[is.na(s_df_new$time_in_system),]) # verification
  # print(s_df_new[c(496, 523, 1615), ]) # testing
  average_minutes_in_system = round(sum(s_df_new$time_in_system) / max(s_df_new$message_count))
  average_minutes_delay = round(sum(s_df_new$delay/seconds_per_minute) / max(s_df_new$message_count)) 
  max_minutes_in_system = max(s_df_new$time_in_system)
  max_minutes_delay = max(s_df_new$delay/seconds_per_minute)
  staff_minutes_doing_non_NOTAM_tasks = total_staff_minutes - max(s_df$cml_staff_minutes_processing_NOTAMS)
  # end: build up time-in-system distribution
    
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_departed_from_queue", "staff_available"))
  # print(head(s_df_long))
  # begin: plot cumulative curves
  p <- ggplot(data = s_df_long) + xlab("minute") 
  
  pc <- p + 
    geom_step(data = s_df_long %>% filter(variable != "staff_available") , aes(minute, value / 100, colour = variable, linetype = variable)) +
    geom_line(data = s_df_long %>% filter(variable == "staff_available") , aes(minute, value      , colour = variable)) +
    guides(colour = "legend", linetype = FALSE) +
    scale_color_discrete(name = NULL, labels = c("NOTAMs Arrived for Processing", "NOTAMs Departing Queue", "Staffing Level")) +
    scale_linetype_discrete() +
    theme(legend.position = "top") +
    ylab("Cumulative NOTAMs (100s) / Staffing Level") +
    labs(
      subtitle = (
        paste(              
              "Avg. delay-minutes: ", average_minutes_delay,               
              "; Max delay-minutes: ", max_minutes_delay, 
              # "; Avg. minutes in system: ", average_minutes_in_system, 
              # "; Max minutes in system: ", max_minutes_in_system, 
              "; Staff hours doing non-NOTAM tasks: ", round(staff_minutes_doing_non_NOTAM_tasks / 60, 1), 
              "; Staff total hours: ", round(total_staff_minutes / 60, 1)
        )
      )
    )
  return(pc)
  # end: plot cumulative curves
  }


# Alternative plotting function, with three panels, and optional 'focus' plot to zoom in on one hour 
compute_staff_reqd_2 <- function(day, 
                                 model_name,
                                 processing_time_in_minutes = 3, 
                                 hourly_staff_model,
                                 Region = 'Western',
                                 focus_inset = TRUE,
                                 focus_start = "11:00:00",
                                 focus_range = c(550, 650)) {
  # For testing: processing_time_in_minutes = 3; study_length_in_minutes = 7*24*60; focus_inset = TRUE;  focus_start = "11:00:00"; Region = 'Western'

  # begin: staffing parameters. These are now arguments in the function
  average_NOTAM_processing_time_in_minutes <- processing_time_in_minutes # Defaults to 3 min, per Bruce W, each processor can process 480 NOTAMs per day
  # end: staffing parameters
  
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  # Enforce UTC time zone
  day_as_datetime = lubridate::with_tz(day_as_datetime, tzone = "UTC")
  
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_departed_from_queue <- staff_required <- vector(length = minutes_per_day)
  
  arrivals_on_day <- dt[dt$Action.Date >= bod & dt$Action.Date < eod & dt$Region == Region,]

  
  print(paste("Total NOTAMS in Period: ",count(arrivals_on_day) ))
  minute <- day_as_datetime + (minute-1)*60
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / average_NOTAM_processing_time_in_minutes 
  minutes_per_hour = 60L
  seconds_per_minute = 60L
  minute_by_minute_staff_model = rep(hourly_staff_model, each = minutes_per_hour)
  
  s_df <- data.frame(minute, msgs_in_queue = NaN, arrivals, msgs_departed_from_queue, staff_required = NaN, staff_available = minute_by_minute_staff_model, capacity = -1)
  
  s_df$capacity = s_df$staff_available * capacity_in_NOTAMs_per_minute_per_staff
  
  for(n in 1:minutes_per_day){
    s_df$arrivals[n] = sum(arrivals_on_day$Action.Date == s_df$minute[n])
    s_df$cml_arrivals[n] = sum(arrivals_on_day$Action.Date <= s_df$minute[n])
    if (n==1){
      s_df$msgs_in_queue[n] = max(0, s_df$arrivals[n] - s_df$capacity[n])
      s_df$msgs_departed_from_queue[n] = s_df$cml_msgs_departed_from_queue[n] = min(s_df$arrivals[n], s_df$capacity[n])
    }
  }
  
  for(n in 2:minutes_per_day){
    s_df$msgs_departed_from_queue[n] = min(s_df$capacity[n], s_df$arrivals[n] + s_df$msgs_in_queue[n-1])
    s_df$cml_msgs_departed_from_queue[n] = s_df$cml_msgs_departed_from_queue[n-1] + s_df$msgs_departed_from_queue[n]  
    s_df$msgs_in_queue[n] = s_df$cml_arrivals[n] - s_df$cml_msgs_departed_from_queue[n] 
  }
  
  s_df$cml_staff_minutes_processing_NOTAMS = s_df$cml_msgs_departed_from_queue * average_NOTAM_processing_time_in_minutes
  total_staff_minutes = sum(s_df$staff_available)
  
  # Adding staff time available for non-NOTAM tasks per minute.
  s_df <- s_df %>%
    mutate(staff_minutes_available = (capacity - msgs_departed_from_queue )* average_NOTAM_processing_time_in_minutes,
           cml_staff_hours_available = cumsum(staff_minutes_available) / 60)
  
  # begin: build up time-in-system distribution
  message_count = data.frame(message_count = seq(1:max(s_df$cml_arrivals)))
  s_df_arrivals = s_df %>% select(cml_arrivals, minute) %>% group_by(cml_arrivals) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_msgs_departed_from_queue = s_df %>% mutate(cml_msgs_departed_from_queue = ceiling(cml_msgs_departed_from_queue)) %>% select(cml_msgs_departed_from_queue, minute) %>% group_by(cml_msgs_departed_from_queue) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_new = message_count %>% 
    left_join(s_df_arrivals, by = c("message_count" = "cml_arrivals")) %>% 
    left_join(s_df_msgs_departed_from_queue, by = c("message_count" = "cml_msgs_departed_from_queue"), suffix = c(".arrivals", ".msgs_departed_from_queue")) %>%
    fill(minute.arrivals, minute.msgs_departed_from_queue, .direction = "up")
  
  s_df_new$minute.msgs_processed = s_df_new$minute.msgs_departed_from_queue + average_NOTAM_processing_time_in_minutes*seconds_per_minute
  s_df_new$delay = s_df_new$minute.msgs_departed_from_queue - s_df_new$minute.arrivals
  s_df_new$time_in_system = s_df_new$minute.msgs_processed - s_df_new$minute.arrivals
  print(s_df_new[which.max(s_df_new$time_in_system),])
  
  average_minutes_in_system = round(sum(s_df_new$time_in_system, na.rm = T) / 
                                      max(s_df_new$message_count, na.rm = T))
  average_minutes_delay = round(sum(s_df_new$delay/seconds_per_minute, na.rm = T) /
                                  max(s_df_new$message_count, na.rm = T)) 
  max_minutes_in_system = max(s_df_new$time_in_system, na.rm = T)
  max_minutes_delay = max(s_df_new$delay/seconds_per_minute, na.rm = T)
  staff_minutes_doing_non_NOTAM_tasks = total_staff_minutes - max(s_df$cml_staff_minutes_processing_NOTAMS)
  # end: build up time-in-system distribution
  
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_departed_from_queue", "staff_available", "staff_minutes_available", "cml_staff_hours_available"))
  # print(head(s_df_long))
  # begin: plot cumulative curves
  # Idea: two panels, one showing just the cumulative curve and the other showing staff count and staff time available for non-NOTAM tasks 
  p <- ggplot(data = s_df_long) + xlab("Time (UTC)")

  pc <- p + 
    geom_step(data = s_df_long %>% filter(variable %in% c("cml_arrivals", "cml_msgs_departed_from_queue")),
              aes(minute, value, colour = variable),
                  size = 1.25) +
    
    guides(colour = "legend", linetype = FALSE) +
    scale_color_discrete(name = NULL, labels = c("NOTAMs Arrived for Processing", "NOTAMs Departing Queue")) +
    scale_linetype_discrete() +
    theme(legend.position = "top",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          plot.caption = element_text(hjust = 0.5)) +
    ylab("Cumulative NOTAMs") +
    labs(
      caption = (
        paste(              
          "Avg. delay-minutes: ", average_minutes_delay,               
          "; Max delay-minutes: ", max_minutes_delay,
          "\n Staff hours doing non-NOTAM tasks: ", round(staff_minutes_doing_non_NOTAM_tasks / 60, 1),
          "; Staff total hours: ", round(total_staff_minutes / 60, 1)
        )
      )
    )
  
  pc_top <- p + 
    geom_line(data = s_df_long %>% filter(variable == "staff_available") , 
              aes(minute, value),
              size = 1.25, colour = 'darkblue') +
    theme(legend.position = "none", # Remove the legend
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10)) +   
  ylab("Staffing Level \n")
  
  pc_top_2 <- p + 
    geom_line(data = s_df_long %>% filter(variable == "cml_staff_hours_available") , 
              aes(minute, value),
              size = 1.25, colour = 'darkgreen') +
    theme(legend.position = "none", # Remove the legend
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10)) +   
    ylab("Cumulative hours avail. \n for non-NOTAM tasks")
  
  # Put these together. egg::ggarrange is better than grid.arrange, because it preserves common axes correctly.

  ga <- egg::ggarrange(pc_top,
                       pc_top_2,
                       pc,
                       ncol = 1,
                       heights = c(1, 1, 2.5))
  
  
  ggsave(filename = paste0(model_name, '_', day, '.jpeg'),
         plot = ga,
         width = 6, height = 7)
  
  # Focus plot
  if(focus_inset){
    
    start_min = s_df %>% select(minute) %>% filter(grepl(focus_start, format(s_df$minute, '%H:%M:%S')))
    end_min = start_min + 60*60*1 # 1 hour
    
    focus_pc <- pc +
      xlim(unclass(start_min)$minute, unclass(end_min)$minute) +
      ylim(focus_range[1], focus_range[2])
    
    ggsave(filename = paste0('Focus_', model_name, '_', day, '.jpeg'),
           plot = focus_pc,
           width = 5, height = 4)
    
     
  }
  
  # end: plot cumulative curves
}



# Configure busy day analysis ----
# demand_days_of_interest = as.Date("2017-12-13")
# demand_days_of_interest = as.Date(c("2019-02-20", "2019-01-31", "2019-01-17", "2018-12-06")) #, "2018-12-09")) # year 2
# demand_days_of_interest = as.Date(c("2017-12-13", "2018-03-08", "2018-07-25", "2018-07-27")) # year 1

demand_days_of_interest = as.Date('2019-04-29') # 90th day for Western 

use_cumulative_curves_to_estimate_delay <- TRUE
start_with_slowest_block <- FALSE
create_surged_staff_models <- TRUE

# save to PDF
pdf('NOTAM_Cumulative_Curves.pdf', width = 10, height = 8)

# Run busy day analysis ----
distinct_days = day_sums$date

if(exists("demand_days_of_interest")){
  if(length(demand_days_of_interest > 0)){
    distinct_days = unique(distinct_days[distinct_days %in% demand_days_of_interest])
  }
}

number_of_busy_days_to_analyze <- length(distinct_days)

index_vector = 1:number_of_busy_days_to_analyze

# !!! only for Western region now, will have to add facet grouping for service area / region
for(d in index_vector){
  # d = 1

  day = as.Date(distinct_days[d])
  print(day)
  
  bod <- day
  eod <- day + 1  
  gp <- ggplot(data = orig_dt_hr %>% filter(date == bod & Region == 'Western'),
               aes(x = yr_hour,
                   y = hourly_count)) +
    geom_point() +
    geom_text(aes(label = hourly_count), nudge_y = 8) +
    geom_line() +
    ggtitle(paste(bod, ', Busy Period Percentile:', 100* 0.9 ) ) +
    theme_bw()

  print(gp)
  

  other_staff_model_names = "naive_ninetieth_percentile_day"

  # build up some local, naive models; we use data frames but would guess this is suited better to tibbles 
  other_hourly_staff_models = data.frame(name = other_staff_model_names, model_type = "local naive", staff_list = NA, stringsAsFactors = FALSE)
  shift_length = 8L
  
  # staff model 90 ----    
  #populate the base model  
  base_model_name = "naive_ninetieth_percentile_day"
  high = 2
  medium = 1
  low = 1
  shift_start_hour = 5L  
  hourly_staff_model = c(rep(medium, shift_start_hour), rep(low, shift_length), rep(high, shift_length), rep(medium, shift_length - shift_start_hour))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == base_model_name][[1]] = list(hourly_staff_model)
  
  if(create_surged_staff_models & base_model_name %in% other_staff_model_names){ 
    #copy the base model
    hourly_surge_staff = c(rep(3, shift_start_hour), rep(3, shift_length), rep(4, shift_length), rep(3, shift_length - shift_start_hour)) #calibrated to max delay of two minutes on the design day
    surge_model_name = paste(base_model_name, "_with_surge", sep = "")  
    frame_length = nrow(other_hourly_staff_models)
    other_hourly_staff_models[frame_length + 1, c("name", "model_type", "staff_list")] = 
      c(surge_model_name, "local naive", NA)
    other_hourly_staff_models$staff_list[other_hourly_staff_models$name == surge_model_name][[1]] = list(hourly_staff_model + hourly_surge_staff)
  }  
    
  # run cumulative curves with external and/or local staffing models ----
  hourly_staff_models_df = other_hourly_staff_models
  
  # uncomment this line if you want to add the external staff models to cumulative curves analysis
  # hourly_staff_models_df = rbind(external_hourly_staff_models_df, hourly_staff_models_df)
  
  if(use_cumulative_curves_to_estimate_delay){
    for(x in 1:nrow(hourly_staff_models_df)){
      print(as.character(hourly_staff_models_df$name[x]))
      if(hourly_staff_models_df$model_type[x] == "local naive"){
        hourly_staff_model = as.vector(hourly_staff_models_df$staff_list[x][[1]][[1]])
      } else
      {
        hourly_staff_model = as.vector(orig_df[orig_df$hourly_bin_start >= bod & orig_df$hourly_bin_start < eod, as.character(hourly_staff_models_df$name[x])])
      }
      print(hourly_staff_model)
      pc = compute_staff_reqd(day = day, processing_time_in_minutes = 3, hourly_staff_model = hourly_staff_model)
      pc = pc + labs(title = paste(day, ", staff_model: " ,hourly_staff_models_df$name[x], ", busy period percentile: ", 100* 0.9 ) )
      print(pc)
    }
  }
}

dev.off(); system('open NOTAM_Cumulative_Curves.pdf')


### Additional plots and tables for report ----

# Table showing staffing models
# Take hourly_staff_models_df and represent staff_list in columns.

staff_df <- as.data.frame(hourly_staff_models_df$staff_list)

names(staff_df) = hourly_staff_models_df$name

write.csv(staff_df, file = 'NOTAM_Staff_Models_by_hour.csv')

# Identify the specific days and counts of NOTAMs, compared to daily averages for that same month

write.csv(distinct_days %>% select(bod, block_count, percentile),
          file = 'Distinct_Day_Table.csv',
          row.names = F)


save(list = ls(), file = 'Processed_NOTAM_Curve_Data.RData')

## Re-do 75th and 90th percentile day figures ---- 
# with some better formatting
# d = 3 # 3rd of distinct_days is the 75th percentile day
 d = 2 # for 90th

day = as.Date(distinct_days)

bod <- distinct_days
eod <- bod + 1  
  

  for(x in c(1, 2)){ 
    # x = 1
    
    print(model_name <- as.character(hourly_staff_models_df$name[x]))
    
    if(hourly_staff_models_df$model_type[x] == "local naive"){
      hourly_staff_model = as.vector(hourly_staff_models_df$staff_list[x][[1]][[1]])
    } else {
      hourly_staff_model = as.vector(orig_df[orig_df$hourly_bin_start >= bod & orig_df$hourly_bin_start < eod, as.character(hourly_staff_models_df$name[x])])
    }
    
    print(hourly_staff_model)
    
    # Plot. Function will save .jpeg
    compute_staff_reqd_2(day = day, processing_time_in_minutes = 3, 
                         hourly_staff_model = hourly_staff_model, 
                         model_name = model_name,
                         focus_inset = TRUE,
                         focus_start = "17:30:00", # "15:30:00",
                         focus_range = c(100, 200) # c(500, 650) 
                         )
    
  }

