# Additional plotting experiments for Bruce
# 1. Put red over blue
# 2. Take processed away and make line for active queue available 

# Setup ----

# Start from processed data. 
load('Processed_NOTAM_Curve_Data.RData')


# Alternative plotting function, with three panels, and optional 'focus' plot to zoom in on one hour
# Also now has options for 
compute_staff_reqd_3 <- function(day, 
                                 model_name,
                                 processing_time_in_minutes = 3, 
                                 hourly_staff_model, 
                                 focus_inset = TRUE,
                                 focus_start = "11:00:00",
                                 focus_range = c(550, 650),
                                 red_top = F,
                                 plot_queue = F,
                                 ylim = NULL) {
  # For testing: processing_time_in_minutes = 3; study_length_in_minutes = 7*24*60; focus_inset = TRUE;  focus_start = "11:00:00"; focus_range = c(550, 650); red_top = T; plot_queue = T
  
  bod <- as.POSIXct(day, tz = "UTC")
  eod <- bod + 60*60*24
  
  # begin: staffing paramaters. These are now arguments in the function
  average_NOTAM_processing_time_in_minutes <- processing_time_in_minutes # Defaults to 3 min, per Bruce W, each processor can process 480 NOTAMs per day
  # end: staffing parameters
  
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  # Enforce UTC time zone
  day_as_datetime = lubridate::with_tz(day_as_datetime, tzone = "UTC")
  
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_departed_from_queue <- staff_required <- vector(length = minutes_per_day)
  arrivals_on_day <- UniqueInteractions[UniqueInteractions$datetimes >= bod & UniqueInteractions$datetimes < eod,]
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
  
  average_minutes_in_system = round(sum(s_df_new$time_in_system) / max(s_df_new$message_count))
  average_minutes_delay = round(sum(s_df_new$delay/seconds_per_minute) / max(s_df_new$message_count)) 
  max_minutes_in_system = max(s_df_new$time_in_system)
  max_minutes_delay = max(s_df_new$delay/seconds_per_minute)
  staff_minutes_doing_non_NOTAM_tasks = total_staff_minutes - max(s_df$cml_staff_minutes_processing_NOTAMS)
  # end: build up time-in-system distribution
  
  s_df_long = melt(s_df, id = "minute", measure = c("cml_arrivals", 
                                                    "cml_msgs_departed_from_queue", 
                                                    "msgs_in_queue",
                                                    "staff_available", 
                                                    "staff_minutes_available", 
                                                    "cml_staff_hours_available"))
  # print(head(s_df_long))
  # begin: plot cumulative curves
  # Idea: two panels, one showing just the cumulative curve and the other showing staff count and staff time available for non-NOTAM tasks 
  p <- ggplot(data = s_df_long) + xlab("Time (UTC)")
  
  if(red_top){
    pc <- p + 
      geom_step(data = s_df_long %>% filter(variable %in% c("cml_msgs_departed_from_queue", "cml_arrivals")),
                aes(minute, value, colour = variable),
                size = 1.25) +
      
      guides(colour = "legend", linetype = FALSE) +
      scale_colour_manual(name = NULL, 
                          labels = c("NOTAMs Departing Queue", "NOTAMs Arrived for Processing"),
                          values = c("#00BFC4", "#F8766D")) +
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
  } else {
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
  }
  
  # Add ylim if not null ---
  # Edit: have added in scale_y_continuous limits right now
  if(!is.null(ylim)){
    pc <- pc + ylim(ylim)
  }
  
  # Optional: also add line for NOTAMs in queue currently to PC
  if(plot_queue){
    pc <- pc + geom_line(data = s_df_long %>% filter(variable %in% c("msgs_in_queue")),
                         aes(minute, value * 70)) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 70,
                                             name = "NOTAMs in queue"),
                         limits = ylim,
                         ) +
      guides(colour = "legend", linetype = FALSE) 
    
  }
  
  
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
  
  if(red_top) { model_name <- paste0('Red_', model_name)}
  if(plot_queue) {model_name <- paste0('Queue_', model_name)}
     
  ggsave(filename = paste0(model_name, '_', day, '.jpeg'),
         plot = ga,
         width = 6, height = 7)
  
  # Focus plot
  if(focus_inset){
    
    start_min = s_df %>% select(minute) %>% filter(grepl(focus_start, as.character(s_df$minute)))
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


## Alternative with red over blue ---- 
# with some better formatting
# d = 3 # 3rd of distinct_days is the 75th percentile day
 d = 2 # for 90th

day = as.Date(distinct_days$bod[d])

bod <- distinct_days$bod[d]
eod <- distinct_days$eod[d]  

# For these figures, only need rows 2 and 6 from hourly_staff_models_df for 75th; 3 and 7 for 90th

for(x in  c(3, 7)){ # c(2, 6)){ #
  # x = 3
  
  print(model_name <- as.character(hourly_staff_models_df$name[x]))
  
  if(hourly_staff_models_df$model_type[x] == "local naive"){
    hourly_staff_model = as.vector(hourly_staff_models_df$staff_list[x][[1]][[1]])
  } else {
    hourly_staff_model = as.vector(orig_df[orig_df$hourly_bin_start >= bod & orig_df$hourly_bin_start < eod, as.character(hourly_staff_models_df$name[x])])
  }
  
  print(hourly_staff_model)
  
  # Plot. Function will save .jpeg
  compute_staff_reqd_3(day = day, processing_time_in_minutes = 3, 
                       hourly_staff_model = hourly_staff_model, 
                       model_name = model_name,
                       red_top = T,
                       plot_queue = F,
                       ylim = c(0, 1250),
                       focus_inset = TRUE,
                       focus_start =  "16:30:00", #"15:30:00",#
                       focus_range = c(700, 900) # c(500, 650) #
  )
 
  # Option 2: plot queue and secondary axis ----
  compute_staff_reqd_3(day = day, processing_time_in_minutes = 3, 
                       hourly_staff_model = hourly_staff_model, 
                       model_name = model_name,
                       red_top = T,
                       plot_queue = T,
                       ylim = c(0, 1250),
                       focus_inset = TRUE,
                       focus_start =  "16:30:00", #"15:30:00",#
                       focus_range = c(700, 900) # c(500, 650) #
  )
}


# TODO: figure out a way to add to legend when plotting queue 




