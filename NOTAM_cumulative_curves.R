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

  bop <- mp - round(0.5*study_length_in_minutes*60)
  eop <- mp + round(0.5*study_length_in_minutes*60)
    
    
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
  seconds_per_minute = 60L
  minute_by_minute_staff_model = rep(hourly_staff_model, each = minutes_per_hour)
  
  s_df <- data.frame(minute, msgs_in_queue = NaN, arrivals, msgs_processed, staff_required = NaN, staff_available = minute_by_minute_staff_model, capacity = -1)
  
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
  s_df$staff_doing_non_NOTAM_tasks = max(0, s_df$staff_available - s_df$staff_required)

  # begin: build up time-in-system distribution
  message_count = data.frame(message_count = seq(1:max(s_df$cml_arrivals)))
  # print(s_df[900:1080,]) #testing purposes
  s_df_arrivals = s_df %>% select(cml_arrivals, minute) %>% group_by(cml_arrivals) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_msgs_processed = s_df %>% mutate(cml_msgs_processed = ceiling(cml_msgs_processed)) %>% select(cml_msgs_processed, minute) %>% group_by(cml_msgs_processed) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_new = message_count %>% 
    left_join(s_df_arrivals, by = c("message_count" = "cml_arrivals")) %>% 
    left_join(s_df_msgs_processed, by = c("message_count" = "cml_msgs_processed"), suffix = c(".arrivals", ".msgs_processed")) %>%
    fill(minute.arrivals, minute.msgs_processed, .direction = "up")
  s_df_new$time_in_system = s_df_new$minute.msgs_processed - s_df_new$minute.arrivals
  print(s_df_new[which.max(s_df_new$time_in_system),]) # testing
  # print(s_df_new[is.na(s_df_new$time_in_system),])  
  # print(s_df_new[850:870, ]) #testing
  average_minutes_in_system = round(sum(s_df_new$time_in_system) / max(s_df_new$message_count) / seconds_per_minute)
  max_minutes_in_system = max(s_df_new$time_in_system) / seconds_per_minute
  staff_minutes_doing_non_NOTAM_tasks = sum(s_df$staff_doing_non_NOTAM_tasks)

  # end: build up time-in-system distribution
    
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_processed", "staff_available"))
  # print(head(s_df_long))
  # begin: plot cumulative curves
  p <- ggplot(data = s_df_long) + xlab("minute") 
  
  pc <- p + 
    geom_step(data = s_df_long %>% filter(variable != "staff_available") , aes(minute, value / 100, colour = variable, linetype = variable)) +
    geom_line(data = s_df_long %>% filter(variable == "staff_available") , aes(minute, value      , colour = variable)) +
    guides(colour = "legend", linetype = FALSE) +
    scale_color_discrete(name = NULL, labels = c("NOTAMs Arrived for Processing", "NOTAMs Processed", "Staffing Level")) +
    scale_linetype_discrete() +
    theme(legend.position = "top") +
    ylab("Cumulative NOTAMs (100s) / Staffing Level") +
    labs(
      subtitle = (paste("Average minutes in system: ", average_minutes_in_system + average_NOTAM_processing_time_in_minutes, "; Max minutes in system: ", max_minutes_in_system + average_NOTAM_processing_time_in_minutes, "; Staff minutes doing non-NOTAM tasks: ", staff_minutes_doing_non_NOTAM_tasks)))
  return(pc)
  # end: plot cumulative curves
  }

<<<<<<< Updated upstream
top_block_list_size = nrow(busy_periods)

# Augment busy periods frame----
for (i in 1:top_block_list_size){ 
  busy_periods$cumulative_count[i] = sum(busy_periods$block_count <= busy_periods$block_count[i])
}

# Create beginning-of-day and end-of-day values for this day
busy_periods$bod <- as.POSIXct( paste(
  as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$year + 1900
  , as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$mon + 1
  , as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$mday
  , sep = "-"), tz = "UTC")
busy_periods$eod <- busy_periods$bod + 60*60*24
busy_periods$percentile = busy_periods$cumulative_count / top_block_list_size

p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per x-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest x-hour blocks as a Cumulative Distribution Function")
)
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)
=======
# Run busy day analysis ----
demand_days_of_interest = as.Date("2018-12-06")
# demand_days_of_interest = as.Date(c("2019-02-20", "2019-01-31", "2019-01-17", "2018-12-06")) #, "2018-12-09")) # year 2
# demand_days_of_interest = as.Date(c("2017-12-13", "2018-03-08", "2018-07-25", "2018-07-27")) # year 1
use_cumulative_curves_to_estimate_delay <- TRUE
start_with_slowest_block <- FALSE
create_surged_staff_models <- TRUE
>>>>>>> Stashed changes

# save to PDF
pdf('NOTAM_Cumulative_Curves.pdf', width = 10, height = 8)

# Run busy day analysis ----
distinct_days = distinct(busy_periods, bod, .keep_all = TRUE)
if(exists("demand_days_of_interest")){
  if(length(demand_days_of_interest > 0)){
    distinct_days = distinct_days[as.Date(distinct_days$bod) %in% demand_days_of_interest,]
  }
}

<<<<<<< Updated upstream
number_of_busy_days_to_analyze <- nrow(distinct_days)
use_cumulative_curves_to_estimate_delay <- TRUE
start_with_slowest_block <- FALSE
=======
number_of_days_to_analyze <- nrow(distinct_days)
>>>>>>> Stashed changes

if(start_with_slowest_block){
  index_vector = nrow(distinct_days):(nrow(distinct_days)-number_of_busy_days_to_analyze+1)
} else
{
  index_vector = 1:number_of_busy_days_to_analyze
}

for(d in index_vector){

  day = as.Date(distinct_days$bod[d])
  print(day)
  # gp <- ggplot(data = orig_df[orig_df$hourly_bin_start > bod & orig_df$hourly_bin_start < eod, ],
  #              aes(x = hourly_bin_start,
  #                  y = block_count)) +
  #   geom_point() +
  #   geom_text(aes(label = block_count), nudge_y = 8) +
  #   geom_line() +
  #   ggtitle(paste(bod, ' Rank:', busy_periods$block_rank_descending[k])) +
  #   theme_bw()
  # 
  # print(gp)
  
  #build list of staffing models to assess
  hourly_staff_models = ("uniform_5")
  
  # TODO: Look at why the dynamic models here are not lining up with correct intra-day variation (should be peaking in later part of the day)
  hourly_staff_models = c(hourly_staff_models, "dynam_model_1", "dynam_model_2", "dynam_model_3", "uniform_2")
  
  hourly_staff_models_df = data.frame(name = hourly_staff_models, model_type = "external (non-naive)")
  hourly_staff_models_df$staff_list[[1]] = rep(list(rep(-1, 24)), nrow(hourly_staff_models_df))
  
  #build up some local, naive models; we use data frames but would guess this is suited better to tibbles
<<<<<<< Updated upstream
  high = 20
  medium = 12
  low = 2
  worst_hour_hourly_staff_model = c(rep(medium, 4), rep(low, 8), rep(high, 8), rep(medium, 4))
=======
  other_staff_model_names = "naive_fiftieth_percentile_day"
  # other_staff_model_names = c(other_staff_model_names, "naive_seventy-fifth_percentile_day")  
  # other_staff_model_names = c(other_staff_model_names, "naive_ninetieth_percentile_day")
  # other_staff_model_names = c(other_staff_model_names, "naive_hundreth_percentile_day")
  # other_staff_model_names = c(other_staff_model_names, "naive_worst_hour")
  # other_staff_model_names = c(other_staff_model_names, "naive_ninetieth_percentile_hour")

  other_hourly_staff_models = data.frame(name = other_staff_model_names, model_type = "local naive", staff_list = NA, stringsAsFactors = FALSE)
  shift_length = 8L
  
  #populate the base model
  base_model_name = "naive_fiftieth_percentile_day"
  high = 4
  medium = 4
  low = 4
  shift_start_hour = 3L
  hourly_staff_model = c(rep(medium, shift_start_hour), rep(low, shift_length), rep(high, shift_length), rep(medium, shift_length - shift_start_hour))
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == base_model_name][[1]] = list(hourly_staff_model)
  
  if(create_surged_staff_models & base_model_name %in% other_staff_model_names){ 
  #copy the base model
  hourly_surge_staff = c(rep(3, shift_start_hour), rep(3, shift_length), rep(6, shift_length), rep(3, shift_length - shift_start_hour))
  surge_model_name = paste(base_model_name, "_with_surge", sep = "")  
  frame_length = nrow(other_hourly_staff_models)
  other_hourly_staff_models[frame_length + 1, c("name", "model_type", "staff_list")] = 
      c(surge_model_name, "local naive", NA)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == surge_model_name][[1]] = list(hourly_staff_model + hourly_surge_staff)
  }
  
  high = 6
  medium = 4
  low = 3
  seventy_fifth_percentile_day_hourly_staff_model = c(rep(medium, 3), rep(low, 8), rep(high, 8), rep(medium, 5))
  
  high = 9
  medium = 3
  low = 3
  ninetieth_percentile_day_hourly_staff_model = c(rep(medium, 5), rep(low, 8), rep(high, 8), rep(medium, 3))
>>>>>>> Stashed changes
  
  high = 14
  medium = 10
  low = 5
  hundreth_percentile_day_hourly_staff_model = c(rep(medium, 4), rep(low, 8), rep(high, 8), rep(medium, 4))
  
  high = 6
  medium = 4
  low = 2
  ninetieth_percentile_hour_hourly_staff_model = c(rep(medium, 3), rep(low, 8), rep(high, 8), rep(medium, 5))
  
<<<<<<< Updated upstream
  other_hourly_staff_models = data.frame(name = c("naive_worst_hour", "naive_ninetieth_percentile"), model_type = "local naive", staff_list = NA)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_worst_hour"][[1]] = list(worst_hour_hourly_staff_model)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_ninetieth_percentile"][[1]] = list(ninetieth_percentile_hour_hourly_staff_model)
=======
  high = 20
  medium = 12
  low = 2
  worst_hour_hourly_staff_model = c(rep(medium, 4), rep(low, 8), rep(high, 8), rep(medium, 4))
  
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_hundreth_percentile_day"][[1]] = list(hundreth_percentile_day_hourly_staff_model)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_ninetieth_percentile_day"][[1]] = list(ninetieth_percentile_day_hourly_staff_model)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_seventy-fifth_percentile_day"][[1]] = list(seventy_fifth_percentile_day_hourly_staff_model)

  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_worst_hour"][[1]] = list(worst_hour_hourly_staff_model)
  other_hourly_staff_models$staff_list[other_hourly_staff_models$name == "naive_ninetieth_percentile_hour"][[1]] = list(ninetieth_percentile_hour_hourly_staff_model)
>>>>>>> Stashed changes
  
  hourly_staff_models_df = rbind(other_hourly_staff_models, hourly_staff_models_df)
  
  if(use_cumulative_curves_to_estimate_delay){
    for(x in 1:nrow(hourly_staff_models_df)){
      print(as.character(hourly_staff_models_df$name[x]))
      if(hourly_staff_models_df$model_type[x] == "local naive"){
        hourly_staff_model = as.vector(hourly_staff_models_df$staff_list[x][[1]][[1]])
      } else
      {
        hourly_staff_model = as.vector(orig_df[orig_df$hourly_bin_start > distinct_days$bod[d] & orig_df$hourly_bin_start < distinct_days$eod[d], as.character(hourly_staff_models_df$name[x])])
      }
      print(hourly_staff_model)
      pc = compute_staff_reqd(day = day, processing_time_in_minutes = 3, hourly_staff_model = hourly_staff_model)
      pc = pc + labs(title = paste(day, ", staff_model: " ,hourly_staff_models_df$name[x], ", busy period percentile: ", distinct_days$percentile[d]) )
      print(pc)
    }
  }
  
  # #try to speed things up by not re-examining this day
  # busy_periods = busy_periods[busy_periods$hourly_bin_end < bod || busy_periods$hourly_bin_start > eod,]
}

dev.off(); system('open NOTAM_Cumulative_Curves.pdf')



