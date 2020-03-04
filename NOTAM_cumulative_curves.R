# Setup ----
# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists

working_dir <- ifelse(grepl('lylet', path.expand('~/')),
                      'C:/Users/lylet/OneDrive/Documents/NOTAM/input_output',
                      '~/OneDrive/Documents/NOTAM/input_output')

setwd(working_dir)

if(!file.exists("NOTAM_Freq_w_busy_days.RData")){
  cat('Attempting to source the script to generate busy_periods data frame')
  source('C:/Users/lylet/Documents/GitHub/notam-metar/NOTAM_Freq/NOTAM_analysis_busy_periods.R')
}

load("NOTAM_Freq_w_busy_days.RData")

library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(dplyr)
library(reshape2)
library(tidyr)

# Define functions ----

compute_staff_reqd <- function(day, processing_time_in_minutes = 3, hourly_staff_model, study_period_midpoint_utc = as.POSIXct("2019-12-14 23:40:12", tz = "UTC"), study_length_in_minutes = 7*24*60) {
  
  
  #rounding to a whole minute
  y <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$year + 1900
  m <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$mon + 1
  d <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$mday
  h <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$hour
  mi <- as.POSIXlt.POSIXct(study_period_midpoint_utc)$min
  
  # Create beginning-of-period and end-of-period values for this period
  mp_string = paste( paste(y, m, d, sep = "-"), paste(h, mi, "00", sep = ":"), sep = " ")
  mp <- as.POSIXct(mp_string
    , tz = "UTC")

  bod <- as.POSIXct(day, tz = "utc")
  eod <- bod + 60*60*24
  
  bop <- mp - round(0.5*study_length_in_minutes*60)
  eop <- mp + round(0.5*study_length_in_minutes*60)
    
    
  #begin: staffing paramaters. These are now arguments in the function
  average_NOTAM_processing_time_in_minutes <- processing_time_in_minutes # Defaults to 3 min, per Bruce W, each processor can process 480 NOTAMs per day
  #end: staffing parameters
  
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_departed_from_queue <- staff_required <- vector(length = minutes_per_day)
  arrivals_on_day <- UniqueInteractions[UniqueInteractions$datetimes >= bod & UniqueInteractions$datetimes < eod,]
  # ordered_arrivals <- arrivals_on_day[order(arrivals_on_day$datetimes),]
  print(paste("Total NOTAMS in Period: ",count(arrivals_on_day) ))
  minute <- day_as_datetime + (minute-1)*60
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / average_NOTAM_processing_time_in_minutes 
  minutes_per_hour = 60L
  seconds_per_minute = 60L
  minute_by_minute_staff_model = rep(hourly_staff_model, each = minutes_per_hour)
  
  s_df <- data.frame(minute, msgs_in_queue = NaN, arrivals, msgs_departed_from_queue, staff_required = NaN, staff_available = minute_by_minute_staff_model, capacity = -1)
  
  s_df$capacity = s_df$staff_available * capacity_in_NOTAMs_per_minute_per_staff
  
  for(n in 1:minutes_per_day){
    s_df$arrivals[n] = sum(arrivals_on_day$datetimes == s_df$minute[n])
    s_df$cml_arrivals[n] = sum(arrivals_on_day$datetimes <= s_df$minute[n])
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
  # print(s_df[900:1080,]) #testing purposes
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

# Configure busy day analysis ----
# demand_days_of_interest = as.Date("2017-12-13")
# demand_days_of_interest = as.Date(c("2019-02-20", "2019-01-31", "2019-01-17", "2018-12-06")) #, "2018-12-09")) # year 2
demand_days_of_interest = as.Date(c("2017-12-13", "2018-03-08", "2018-07-25", "2018-07-27")) # year 1
use_cumulative_curves_to_estimate_delay <- TRUE
start_with_slowest_block <- FALSE
create_surged_staff_models <- TRUE

# save to PDF
pdf('NOTAM_Cumulative_Curves.pdf', width = 10, height = 8)

# Run busy day analysis ----
distinct_days = distinct(busy_periods, bod, .keep_all = TRUE)
if(exists("demand_days_of_interest")){
  if(length(demand_days_of_interest > 0)){
    distinct_days = distinct_days[as.Date(distinct_days$bod) %in% demand_days_of_interest,]
  }
}

number_of_busy_days_to_analyze <- nrow(distinct_days)

if(start_with_slowest_block){
  index_vector = nrow(distinct_days):(nrow(distinct_days)-number_of_busy_days_to_analyze+1)
} else
{
  index_vector = 1:number_of_busy_days_to_analyze
}

for(d in index_vector){

  day = as.Date(distinct_days$bod[d])
  print(day)
  
  bod <- distinct_days$bod[d]
  eod <- distinct_days$eod[d]  
  gp <- ggplot(data = orig_df[orig_df$hourly_bin_start >= bod & orig_df$hourly_bin_start < eod, ],
               aes(x = hourly_bin_start,
                   y = hourly_count)) +
    geom_point() +
    geom_text(aes(label = hourly_count), nudge_y = 8) +
    geom_line() +
    ggtitle(paste(bod, ', Busy Period Percentile:', 100*round(distinct_days$percentile[d],3) )) +
    theme_bw()

  print(gp)
  
  #build list of external staffing models to assess
  hourly_staff_models = ("uniform_5")
  
  # TODO: Look at why the dynamic models here are not lining up with correct intra-day variation (should be peaking in later part of the day)
  # hourly_staff_models = c(hourly_staff_models, "dynam_model_1", "dynam_model_2", "dynam_model_3", "uniform_2")
  
  external_hourly_staff_models_df = data.frame(name = hourly_staff_models, model_type = "external (non-naive)")
  external_hourly_staff_models_df$staff_list[[1]] = rep(list(rep(-1, 24)), length(hourly_staff_models))
  
  #list the local naive models you would like to run
  other_staff_model_names = "naive_fiftieth_percentile_day"
  other_staff_model_names = c(other_staff_model_names, "naive_seventy-fifth_percentile_day")
  other_staff_model_names = c(other_staff_model_names, "naive_ninetieth_percentile_day")
  other_staff_model_names = c(other_staff_model_names, "naive_hundreth_percentile_day")
  # other_staff_model_names = c(other_staff_model_names, "naive_worst_hour")
  # other_staff_model_names = c(other_staff_model_names, "naive_ninetieth_percentile_hour")
  
  #build up some local, naive models; we use data frames but would guess this is suited better to tibbles 
  other_hourly_staff_models = data.frame(name = other_staff_model_names, model_type = "local naive", staff_list = NA, stringsAsFactors = FALSE)
  shift_length = 8L
  
  # staff model 50 ----
  #populate the base model
  base_model_name = "naive_fiftieth_percentile_day"
  high = 4
  medium = 4
  low = 4
  shift_start_hour = 1L
  hourly_staff_model = c(rep(high, shift_start_hour), rep(low, shift_length), rep(medium, shift_length), rep(high, shift_length - shift_start_hour))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == base_model_name][[1]] = list(hourly_staff_model)
  
  if(create_surged_staff_models & base_model_name %in% other_staff_model_names){ 
    #copy the base model
    hourly_surge_staff = c(rep(6, shift_start_hour), rep(6, shift_length), rep(6, shift_length), rep(6, shift_length - shift_start_hour)) # calibrated to max delay of 2 minutes
    surge_model_name = paste(base_model_name, "_with_surge", sep = "")  
    frame_length = nrow(other_hourly_staff_models)
    other_hourly_staff_models[frame_length + 1, c("name", "model_type", "staff_list")] = 
      c(surge_model_name, "local naive", NA)
    other_hourly_staff_models$staff_list[other_hourly_staff_models$name == surge_model_name][[1]] = list(hourly_staff_model + hourly_surge_staff)
  }
  
  # staff model 75 ----
  #populate the base model  
  base_model_name = "naive_seventy-fifth_percentile_day"  
  high = 6
  medium = 4
  low = 3
  shift_start_hour = 3L  
  hourly_staff_model = c(rep(medium, shift_start_hour), rep(low, shift_length), rep(high, shift_length), rep(medium, shift_length - shift_start_hour))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == base_model_name][[1]] = list(hourly_staff_model)

  if(create_surged_staff_models & base_model_name %in% other_staff_model_names){ 
    #copy the base model
    hourly_surge_staff = c(rep(7, shift_start_hour), rep(3, shift_length), rep(35, shift_length), rep(7, shift_length - shift_start_hour)) #calibrated
    surge_model_name = paste(base_model_name, "_with_surge", sep = "")  
    frame_length = nrow(other_hourly_staff_models)
    other_hourly_staff_models[frame_length + 1, c("name", "model_type", "staff_list")] = 
      c(surge_model_name, "local naive", NA)
    other_hourly_staff_models$staff_list[other_hourly_staff_models$name == surge_model_name][[1]] = list(hourly_staff_model + hourly_surge_staff)
  }

  # staff model 90 ----    
  #populate the base model  
  base_model_name = "naive_ninetieth_percentile_day"
  high = 9
  medium = 3
  low = 3
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
    
  # staff model 100 ----
  #populate the base model  
  base_model_name = "naive_hundreth_percentile_day"
  high = 14
  medium = 10
  low = 5
  shift_start_hour = 4L  
  hourly_staff_model = c(rep(medium, shift_start_hour), rep(low, shift_length), rep(high, shift_length), rep(medium, shift_length - shift_start_hour))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == base_model_name][[1]] = list(hourly_staff_model)
  
  if(create_surged_staff_models & base_model_name %in% other_staff_model_names){ 
    #copy the base model
    hourly_surge_staff = c(rep(5, shift_start_hour), rep(5, shift_length), rep(7, shift_length), rep(5, shift_length - shift_start_hour)) #calibrated to max delay of two minutes on the design day
    surge_model_name = paste(base_model_name, "_with_surge", sep = "")  
    frame_length = nrow(other_hourly_staff_models)
    other_hourly_staff_models[frame_length + 1, c("name", "model_type", "staff_list")] = 
      c(surge_model_name, "local naive", NA)
    other_hourly_staff_models$staff_list[other_hourly_staff_models$name == surge_model_name][[1]] = list(hourly_staff_model + hourly_surge_staff)
  }
  
  # staff models not currently being tested ----
  high = 6
  medium = 4
  low = 2
  ninetieth_percentile_hour_hourly_staff_model = c(rep(medium, 3), rep(low, 8), rep(high, 8), rep(medium, 5))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_ninetieth_percentile_hour"][[1]] = list(ninetieth_percentile_hour_hourly_staff_model)
  
  high = 20
  medium = 12
  low = 2
  worst_hour_hourly_staff_model = c(rep(medium, 4), rep(low, 8), rep(high, 8), rep(medium, 4))
  
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_worst_hour"][[1]] = list(worst_hour_hourly_staff_model)
  
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
      pc = pc + labs(title = paste(day, ", staff_model: " ,hourly_staff_models_df$name[x], ", busy period percentile: ", 100*round(distinct_days$percentile[d],3) ) )
      print(pc)
    }
  }
}

dev.off(); system('open NOTAM_Cumulative_Curves.pdf')



