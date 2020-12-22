# Setup ----
# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists

# Adapted for FNS reports of NOTAMs, with service area added

# Adding a step to optimize the staffing needed

# Setup ----

if(grepl(getwd(), 'notam-metar$')){
  setwd('./FNS_Reports')
}

if(!file.exists("FNS_NOTAM_Freq_w_busy_days.RData")){
  cat('Attempting to source the script to generate busy_periods data frame')
  source('NOTAM_FNS_Analysis.Rmd')
}

load("FNS_NOTAM_Freq_w_busy_days.RData")

library(ggplot2)
library(grid)
library(gridExtra)
# library(gtable)
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

# Across the whole NAS, by weekend and season ----

day_sum_NAS = orig_dt_hr %>%
  mutate(season_we = paste(season, weekend)) %>%
  ungroup() %>%
  group_by(date, season, weekend, season_we) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days 

pctile_day_NAS = day_sum_NAS %>%
  ungroup() %>%
  mutate(season_we = paste(season, weekend)) %>%
  group_by(season, weekend, season_we) %>%
  summarize(quantile = seq(0, 1, 0.1),
            daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))

ninetieth = pctile_day_NAS %>% filter(quantile == 0.9)

# Which days are the 90th percentile demand day?

ninetieth_r = pctile_day_NAS %>% filter(quantile == 0.9)
hundredth_r = pctile_day_NAS %>% filter(quantile == 1)  

ninetieth_day_NAS  = vector()

for(sw in unique(pctile_day_NAS$season_we)){
  # sw = 'Fall Weekday'
  ninetieth_sw = ninetieth_r %>% filter(season_we == sw)
  hundredth_sw = hundredth_r %>% filter(season_we == sw)
  
  ninetieth_day_NASsw = day_sum_NAS %>%
    filter(season_we == sw) %>%
    filter(daily_count >=  ninetieth_sw$daily_count & 
             daily_count <  hundredth_sw$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_sw$daily_count)
  
  ninetieth_day_NASsw = ninetieth_day_NASsw %>% 
    filter(proximity_to_ninetieth == min(ninetieth_day_NASsw$proximity_to_ninetieth))
  
  class(ninetieth_day_NASsw) = 'data.frame'
  
  ninetieth_day_NAS = rbind(ninetieth_day_NAS, ninetieth_day_NASsw)
}

ninetieth_day_NAS

# Define functions ----

compute_staff_reqd <- function(day, 
                               hourly_staff_model,
                               processing_time_in_minutes = 3, 
                               Region = 'Western') {
  
  # day = as.Date("2019-02-24")
  # hourly_staff_model = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9, 9, 9, 9, 9, 3, 3, 3)
  # hourly_staff_model = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 9, 3, 3, 3)
  
  # processing_time_in_minutes = 3
  # Region = 'Western'

  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  # Enforce UTC time zone. 
  day_as_datetime = lubridate::with_tz(day_as_datetime, tzone = "UTC")
  
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_departed_from_queue <- staff_required <- vector(length = minutes_per_day)
  
  arrivals_on_day <- dt[dt$Action.Date >= day_as_datetime &
                          dt$Action.Date < day_as_datetime + 60*60*24 &
                          dt$Region == Region,]
  
  print(paste("Total NOTAMS in Period: ", count(arrivals_on_day) ))
  minute <- day_as_datetime + (minute-1)*60
  
  # Convert minute to UTC for consistency
  minute <- as.POSIXct(minute, origin = "1970-01-01", tz = "UTC")
  
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / processing_time_in_minutes 
  minutes_per_hour = 60L
  seconds_per_minute = 60L
  
  # Begin implementing staffing model
  # Can start while loop here for delay calcs
  minute_by_minute_staff_model = rep(hourly_staff_model, each = minutes_per_hour)
  
  # Set up data frame for processing
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
  
  s_df$cml_staff_minutes_processing_NOTAMS = s_df$cml_msgs_departed_from_queue * processing_time_in_minutes
  total_staff_minutes = sum(s_df$staff_available)

  # Adding staff time available for non-NOTAM tasks per minute.
  s_df <- s_df %>%
    mutate(staff_minutes_available = (capacity - msgs_departed_from_queue )* processing_time_in_minutes,
           cml_staff_hours_available = cumsum(staff_minutes_available) / 60)
  
  # begin: build up time-in-system distribution
  message_count = data.frame(message_count = seq(1:max(s_df$cml_arrivals)))
  
  s_df_arrivals = s_df %>% select(cml_arrivals, minute) %>% group_by(cml_arrivals) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_msgs_departed_from_queue = s_df %>% mutate(cml_msgs_departed_from_queue = ceiling(cml_msgs_departed_from_queue)) %>% select(cml_msgs_departed_from_queue, minute) %>% group_by(cml_msgs_departed_from_queue) %>% mutate(earliest = rank(minute) == 1) %>% filter(earliest == TRUE)
  s_df_new = message_count %>% 
    left_join(s_df_arrivals, by = c("message_count" = "cml_arrivals")) %>% 
    left_join(s_df_msgs_departed_from_queue, by = c("message_count" = "cml_msgs_departed_from_queue"), suffix = c(".arrivals", ".msgs_departed_from_queue")) %>%
    fill(minute.arrivals, minute.msgs_departed_from_queue, .direction = "up")
  s_df_new$minute.msgs_processed = s_df_new$minute.msgs_departed_from_queue + processing_time_in_minutes*seconds_per_minute
  s_df_new$delay = s_df_new$minute.msgs_departed_from_queue - s_df_new$minute.arrivals
  s_df_new$time_in_system = s_df_new$minute.msgs_processed - s_df_new$minute.arrivals
  print(s_df_new[which.max(s_df_new$time_in_system),])
  
  average_minutes_in_system = round(sum(s_df_new$time_in_system, na.rm=T) / max(s_df_new$message_count, na.rm=T))
  average_minutes_delay = round(sum(s_df_new$delay/seconds_per_minute, na.rm=T) / max(s_df_new$message_count, na.rm=T)) 
  max_minutes_in_system = max(s_df_new$time_in_system, na.rm=T)
  max_minutes_delay = as.numeric(max(s_df_new$delay/seconds_per_minute, na.rm=T))
  
  hour_of_max_delay = s_df_new %>% filter(delay == max(delay, na.rm = T)) %>% select(minute.arrivals) 
  
  hour_of_max_delay = format(hour_of_max_delay[,1][1], format = "%H")
  
  staff_minutes_doing_non_NOTAM_tasks = total_staff_minutes - max(s_df$cml_staff_minutes_processing_NOTAMS, na.rm=T)
  # end: build up time-in-system distribution
    
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_departed_from_queue", "staff_available"))
 
  return(list(staff_df = s_df,
              max_minutes_delay = max_minutes_delay,
              average_minutes_delay = average_minutes_delay,
              max_minutes_in_system = max_minutes_in_system,
              hour_of_max_delay = hour_of_max_delay,
              staff_minutes_doing_non_NOTAM_tasks = staff_minutes_doing_non_NOTAM_tasks,
              total_staff_minutes = total_staff_minutes))
  }


plot_staffing_model_simple <- function(s_df_long){
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
      title = paste(Region, 'Service Area -', day),
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

plot_staffing_model_focus <- function(day, staff_reqd,
                                 Region,
                                 focus_inset = TRUE,
                                 focus_start = "11:00:00",
                                 focus_range = c(550, 650)) {
  # For testing: 
  # day = as.Date('2019-02-24'); processing_time_in_minutes = 3; focus_inset = TRUE;  focus_start = "11:00:00"; Region = 'Western'

  # staff_reqd is a list, output from compute_staff_reqd() function.
  s_df = staff_reqd$staff_df
  
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", "cml_msgs_departed_from_queue", "staff_available", "staff_minutes_available", "cml_staff_hours_available"))
  
  # print(head(s_df_long))
  # begin: plot cumulative curves
  # two panels, one showing just the cumulative curve and the other showing staff count and staff time available for non-NOTAM tasks 
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
      title = paste(Region, 'Service Area -', day),
      caption = (
        paste(              
          "Avg. delay-minutes: ", staff_reqd$average_minutes_delay,               
          "; Max delay-minutes: ", staff_reqd$max_minutes_delay,
          "\n Staff hours doing non-NOTAM tasks: ", round(staff_reqd$staff_minutes_doing_non_NOTAM_tasks / 60, 1),
          "; Staff total hours: ", round(staff_reqd$total_staff_minutes / 60, 1)
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
  
  
  ggsave(filename = paste0(Region, '_ninetieth_', day, '_', season, '_', weekend, '_', delay_target_x, 'min_target.jpeg'),
         plot = ga,
         width = 6, height = 7)
  
  # Focus plot
  if(focus_inset){
    
    start_min = s_df %>% select(minute) %>% filter(grepl(focus_start, format(s_df$minute, '%H:%M:%S')))
    end_min = start_min + 60*60*1 # 1 hour
    
    focus_pc <- pc +
      xlim(unclass(start_min)$minute, unclass(end_min)$minute) +
      ylim(focus_range[1], focus_range[2])
    
    ggsave(filename = paste0('Focus_', Region, '_ninetieth_', day, '.jpeg'),
           plot = focus_pc,
           width = 5, height = 4)
    
     
  }
  
  # end: plot cumulative curves
}



# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Configure busy day analysis ----
# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>

# Data frame with the 90th percentile days
ninetieth_days

ninetieth_day_NAS


max_delay_target = c(2, 5, 15)

# Loop over season x weekend combinations

for(sw in 1:nrow(ninetieth_day_NAS)){
  # sw = 1
  season = ninetieth_day_NAS[sw, 'season']
  weekend = ninetieth_day_NAS[sw, 'weekend']
  
  # Loop over targets 
  
  for(delay_target_x in max_delay_target){
    # delay_target_x = 15    
    staff_models = vector()
    
    # save to PDF
    fn = paste0('NOTAM_Staffing_', season, '_', weekend, '_', formatC(delay_target_x, width = 2, flag = 0),'min_target.pdf')
    
    pdf(fn, width = 10, height = 8)
    
    for(Region_x in unique(ninetieth_days$Region)){
      
      # Region_x = 'Eastern'

      demand_day_of_interest = ninetieth_day_NAS[ninetieth_day_NAS$season == season &
                                                    ninetieth_day_NAS$weekend == weekend, 'date']
        # as.Date(ninetieth_days[ninetieth_days$Region == Region,'date'])
      
      # Count in the region on the demand day
      count_on_demand_day = ninetieth_days[ninetieth_days$Region == Region_x,'daily_count']
  
      # Run busy day analysis ----
      day = demand_day_of_interest
      print(day)
      
      bod <- day
      eod <- day + 1  
      
      this_day = orig_dt_hr %>% filter(date == bod & Region == Region_x)
      
      gp <- ggplot(data = this_day,
                   aes(x = yr_hour,
                       y = hourly_count)) +
        geom_point() +
        geom_text(aes(label = hourly_count), nudge_y = 4) +
        geom_line() +
        xlab('Hour (UTC)') + ylab('Hourly NOTAM count') +
        ggtitle(paste(Region_x, bod, ', Busy Period Percentile:', 100* 0.9, '\n', season, weekend)) +
        theme_bw()
      
      print(gp)
      
      # staff model 90 ----    
      # populate the base model  
    
      # Make an guess about high and low staffing based on total NOTAM count, then optimize
      # 1. high med low are set up using a rule of thumb, with floors set
      # 2. The identity of which third of the day is set to high, med, or low is set here
      # 3. Then we use a while loop to get to the target max_delay value
      
      shift_length = 8L
      
      high = floor(count_on_demand_day / 150); high = ifelse(high < 2, 2, high)
      med = floor(count_on_demand_day / 250); med = ifelse(med < 1, 1, med)
      low = floor(count_on_demand_day / 300); low = ifelse(low < 1, 1, low)
      
      shift_hour_starts <- this_day %>%
        ungroup() %>%
        mutate(day_thirds = cut(as.numeric(this_day$hour), 3)) %>%
        group_by(day_thirds) %>%
        summarize(sum_thirds = sum(hourly_count)) %>%
        ungroup() %>%
        summarize(
                  high = c('00', '08', '16')[which(sum_thirds == max(sum_thirds))],
                  med = c('00', '08', '16')[which(sum_thirds != max(sum_thirds) & sum_thirds != min(sum_thirds))],
                  low = c('00', '08', '16')[which(sum_thirds == min(sum_thirds))])
      
      # Order of shifts
      first_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) == min(as.numeric(shift_hour_starts)))]
      second_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) != min(as.numeric(shift_hour_starts)) & 
                                               as.numeric(shift_hour_starts) != max(as.numeric(shift_hour_starts)))]
      third_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) == max(as.numeric(shift_hour_starts)))]
      
      # Establish the base model. This will get optimized to the max delay
      hourly_staff_model = c(rep(get(names(first_shift)), shift_length),
                             rep(get(names(second_shift)), shift_length),
                             rep(get(names(third_shift)), shift_length))
                             
      
    ### Calculate, plot, and report ----
    
      staff_reqd <- compute_staff_reqd(day = day,
                                       processing_time_in_minutes = 3,
                                       hourly_staff_model = hourly_staff_model,
                                       Region = Region_x)
      
      max_delay = round(staff_reqd$max_minutes_delay)
      
      while(max_delay > delay_target_x){
        # In which shift does max delay occurs in? Add staff there
        
        add_to_this_shift = ifelse(as.numeric(staff_reqd$hour_of_max_delay) < as.numeric(second_shift), 'first_shift',
                                   ifelse(as.numeric(staff_reqd$hour_of_max_delay) >= as.numeric(third_shift), 'third_shift',
                                          'second_shift'))
        add_to_this_shift <- names(get(add_to_this_shift))
        # add one staff to the shift to add to 
        
        assign(add_to_this_shift, get(add_to_this_shift) + 1)
                                   
        
         hourly_staff_model = c(rep(get(names(first_shift)), shift_length),
                                rep(get(names(second_shift)), shift_length),
                                rep(get(names(third_shift)), shift_length))
         
        staff_reqd <- compute_staff_reqd(day = day,
                                         processing_time_in_minutes = 3,
                                         hourly_staff_model = hourly_staff_model,
                                         Region = Region_x)
        
        max_delay = round(staff_reqd$max_minutes_delay)
        
        
      } # end while
      
      plot_staffing_model_focus(day, staff_reqd, Region_x,
                                focus_inset = F)
      
      # Save staffing model for this target
      hourly_staff_df_x = data.frame(hourly_staff_model = hourly_staff_model,
                                      hour = formatC(0:23, width = 2, flag = 0))
      staff_model_df_x = this_day %>% select(-hourly_count_rank, -peak2, -peak3)
      staff_model_df_x = left_join(hourly_staff_df_x, staff_model_df_x, by = 'hour')
      
      staff_models = rbind(staff_models, as.data.frame(staff_model_df_x))
      
      
    } # end loop over regions
    
    write.csv(staff_models,
              file = paste('Staff_models_90th_', season, '_', weekend, '_', delay_target_x, 'min_target.csv'),
              row.names = F)
    
    dev.off(); system(paste('open', fn))
    
  } # end loop over targets
  
} # end loop over season x weekend combinations