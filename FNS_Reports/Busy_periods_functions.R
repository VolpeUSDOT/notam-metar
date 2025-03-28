# Define functions for NOTAM busy period analysis

compute_staff_reqd <- function(day, 
                               hourly_staff_model,
                               processing_time_in_minutes = 3, 
                               Region = 'Western') {
  
  # day = as.Date("2019-02-24")
  # hourly_staff_model = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9, 9, 9, 9, 9, 3, 3, 3)
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
                          dt$Region %in% Region,]
  
  # print(paste("Total NOTAMS in Period: ", count(arrivals_on_day) ))
  minute <- day_as_datetime + (minute-1)*60
  
  # Convert minute to UTC for consistency
  minute <- as.POSIXct(minute, origin = "1970-01-01", tz = "UTC")
  
  capacity_in_NOTAMs_per_minute_per_staff <- 1 / processing_time_in_minutes 
  minutes_per_hour = 60L
  seconds_per_minute = 60L
  
  # Begin implementing staffing model
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
  
  # print(s_df_new[which.max(s_df_new$time_in_system),])
  
  average_minutes_in_system = round(as.numeric(mean(s_df_new$time_in_system, na.rm=T)), 2)
  average_minutes_delay = round(as.numeric(mean(s_df_new$delay/seconds_per_minute, na.rm=T)), 2) 
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


# Plotting function, with three panels, and optional 'focus' plot to zoom in on one hour 

plot_staffing_model_focus <- function(day, staff_reqd,
                                      Region,
                                      focus_inset = TRUE,
                                      focus_start = "11:00:00",
                                      focus_range = c(550, 650)) {
  # For testing: 
  # First, run compute_staff_reqd() function with a staffing model on a specific day. Then test this function using these or similar values:
  # day = as.Date('2019-02-24'); processing_time_in_minutes = 3; focus_inset = TRUE;  focus_start = "11:00:00"; Region = 'Eastern'
  
  region_set_name = ifelse(length(Region) > 1,
                           paste(Region, collapse = ' + '), Region_x)
  
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
      title = paste(region_set_name, 'Service Area -', day),
      caption = (
        paste(              
          "Avg. delay-minutes: ", staff_reqd$average_minutes_delay,               
          "; Max delay-minutes: ", staff_reqd$max_minutes_delay,
          "\n Staff hours available for non-NOTAM tasks: ", round(staff_reqd$staff_minutes_doing_non_NOTAM_tasks / 60, 1),
          "; Staff total hours: ", round(staff_reqd$total_staff_minutes / 60, 1)
        )
      )
    )
  
  staff_range = range(s_df_long %>% filter(variable == 'staff_available') %>% select(value))
  
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
    ylab("Staffing level \n")
  
  if(diff(staff_range) > 0){
    pc_top <- pc_top + scale_y_continuous(n.breaks = diff(staff_range)+1) 
  }
  
  pc_top_2 <- p + 
    theme(legend.position = "none", # Remove the legend
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10)) +   
    ylab("Staff available \n per minute")
  
  if(diff(staff_range) > 0){
    pc_top_2 <- pc_top_2 + geom_smooth(data = s_df_long %>% filter(variable == "staff_available") , 
                                       aes(minute, value),
                                       size = 1.25, colour = 'darkgreen') 
  } else {
    pc_top_2 <- pc_top_2 + geom_line(data = s_df_long %>% filter(variable == "staff_available") , 
                                     aes(minute, value),
                                     size = 1.25, colour = 'darkgreen') 
  }
  
  # Put these together. egg::ggarrange is better than grid.arrange, because it preserves common axes correctly.
  
  ga <- egg::ggarrange(pc_top,
                       pc_top_2,
                       pc,
                       ncol = 1,
                       heights = c(1, 1, 2.5))
  

  
  ggsave(filename = file.path(output_dir, paste0(region_set_name, '_ninetieth_', day, '_', season, '_', weekend, '_', delay_target_x, 'min_target.jpeg')),
         plot = ga,
         width = 6, height = 7)
  
  # Focus plot
  if(focus_inset){
    
    start_min = s_df %>% select(minute) %>% filter(grepl(focus_start, format(s_df$minute, '%H:%M:%S')))
    end_min = start_min + 60*60*1 # 1 hour
    
    focus_pc <- pc +
      xlim(unclass(start_min)$minute, unclass(end_min)$minute) +
      ylim(focus_range[1], focus_range[2])
    
    ggsave(filename = paste0('Focus_', region_set_name, '_ninetieth_', day, '.jpeg'),
           plot = focus_pc,
           width = 5, height = 4)
    
    
  }
  
  # end: plot cumulative curves
}

