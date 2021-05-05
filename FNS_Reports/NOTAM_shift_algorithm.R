# STAFFING ALGORITHM - 2021/04/05
# Result: 19 shifts
# - Start with zero people on staff.
# - Keep track of the number of unprocessed NOTAMs as well as the average
#   delay among processed NOTAMs.
# - Unprocessed NOTAMs are defined as those ignored just before a period of
#   time when 0 people are on staff and all incoming NOTAMs during that same
#   period.
# - Use a 24-vector (similar to the removals vector in Test Algorithm 1) to
#   track how many shifts begin at each hour during the day.
# - While the number of unprocessed NOTAMs is greater than 0, use the sliding
#   window method to place a person in the shift which results in the highest
#   decrease of the number of processed NOTAMs. Ties are broken by the shift
#   which results in the largest decrease in average delay.
# - While there are unstaffed hours, add a shift in the window containing an
#   unstaffed hour with the largest impact on average delay.
#   - This shouldn't happen very often at all; it seems unlikely that a
#     particularly busy day would have no NOTAMs coming in for an entire hour.
# - While the average delay is greater than or equal to 2 minutes, use the
#   sliding window method to place a person in the shift which results in the
#   largest decrease in average delay.
# - While the average delay is less than or equal to 2 minutes, use the sliding
#   window method to remove a person from the shift which results in the
#   smallest increase in average delay.
# - If there are 0 people starting a shift during a given hour, skip that
#   hour.

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)

# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists
if(grepl('notam-metar$', getwd())){
  setwd('./FNS_Reports')
}

output_dir <- "Results"

# Get dependencies if not already installed
source('Utility/get_packages.R')

# Find the percentile days. This results in data frames of the 90th percentile
# busiest days, either by service area or across the whole NAS by season
source('Find_percentile_days.R')

hours_in_day <- 24
minutes_in_hour <- 60
shift_length <- 8
notam_processing_time <- 3
max_delay_target <- 2
minutes_in_day <- hours_in_day * minutes_in_hour

# - v is the numerical vector to pad
# - block_length is a positive integer representing the length of the original
#   element and the repeated zeros that follow
# - example: pad_vector_with_zeros(c(1,4), 3) becomes c(1,0,0,4,0,0)
pad_vector_with_zeros <- function(v, block_length) {
  result <- c()
  for (i in v) {
    result <- append(result, i)
    result <- append(result, integer(block_length - 1))
  }
  result
}

# - v is the vector input
# - n is a positive integer representing the distance between elements in the
#   input vector appearing in the output
# - offset is a positive integer representing the starting position of the
#   collection process
get_every_nth_element <- function(v, n, offset = 1) {
  output <- c()
  n_elements <- ceiling((length(v) - offset + 1) / n)
  for (k in (1:n_elements) - 1) {
    output <- append(output, v[n*k + offset])
  }
  output
}

# - on_staff is a vector where each entry represents the number of people on
#     staff at that hour.
# - notams_by_minute is a tibble with two columns: minutes_elapsed and n, where
#     minutes_elapsed is the number of minutes from whatever starting time and
#     n is the number of NOTAMs coming in at that minute.
# - notam_processing_time is an integer representing how long it takes for a
#   staff member to process a NOTAM once they get around to it.
get_daily_processing_stats <- function(on_staff, notams_by_minute,
                                    notam_processing_time) {
  # initialize variables ----
  available_staff <- on_staff[1]
  max_available <- available_staff
  total_notams <- sum(notams_by_minute$incoming_notams)
  notams_delayed <- 0
  # explanation of this vector: let's say a NOTAM takes 3 minute to process, if
  # this vector is (2 1 3), that means 2 NOTAMS have just been added, 1 has been
  # processing for one minute, and 3 have been processing for 2 minutes.
  notams_processing <- integer(notam_processing_time)
  m <- 0
  length_of_analysis_period <- minutes_in_hour * length(on_staff)
  
  # columns for tibble ====
  on_staff_column <- c()
  available_column <- c()
  incoming_column <- c()
  delayed_column <- c()
  processed_column <- c()
  unprocessed_column <- c()
  column <- c()
  
  # process NOTAMs ----
  # handle all incoming NOTAMs for the hour, then process all leftover NOTAMs to
  # calculate average delay for all NOTAMs which came in during that hour
  while (m < length_of_analysis_period || sum(notams_processing) > 0) {
    nearest_hour_index <- (m %/% minutes_in_hour) %% hours_in_day + 1
    # if (reducing_delay && m == length_of_analysis_period) {
    #   browser()
    # }
    if (m%%minutes_in_hour == 0 && on_staff[nearest_hour_index] == 0) {
      on_staff_column <- append(on_staff_column, integer(minutes_in_hour))
      available_column <- append(available_column, integer(minutes_in_hour))
      incoming_column <- append(incoming_column, integer(minutes_in_hour))
      delayed_column <- append(delayed_column, integer(minutes_in_hour))
      processed_column <- append(processed_column, integer(minutes_in_hour))
      unprocessed_column <- append(unprocessed_column, integer(minutes_in_hour))
      
      minutes_for_hour <- m:(m-1+minutes_in_hour)
      # delayed & currently processing NOTAMs are unprocessed
      max_available <- 0
      
      unprocessed_column[m+1] <- unprocessed_column[m+1] + notams_delayed +
        sum((notams_by_minute %>%
              filter(minutes_elapsed %in% minutes_for_hour))$incoming_notams)
      # retroactively have nobody working on those notams (this also terminates
      # the loop if it's been simulating over 24 hours)
      notams_delayed <- 0
      if (m > 0) for (j in 1:(notam_processing_time - 1)) {
        i <- m+1
        unprocessed_column[i] <- unprocessed_column[i] + notams_processing[j]
        delayed_column[i-j] <- delayed_column[i-j] - notams_processing[j]
        remove(i)
      }
      notams_processing <- integer(notam_processing_time)
      
      m <- m + minutes_in_hour
      next
    }
    previous_max_available <- max_available
    max_available <- on_staff[nearest_hour_index]
    available_staff <- min(max_available, available_staff +
                             notams_processing[notam_processing_time])
    if (previous_max_available < max_available) {
      available_staff <- available_staff + max_available - previous_max_available
    }
    remove(previous_max_available)
    
    # processing NOTAMs on the list
    processed_column <- append(processed_column,
                               notams_processing[notam_processing_time])
    notams_processing <- c(0, notams_processing[1:notam_processing_time - 1])
    
    # placing all available staff on delayed NOTAMs
    notams_processing[1] <- min(notams_delayed, available_staff)
    notams_delayed_interim <- notams_delayed
    available_staff_interim <- available_staff
    notams_delayed <- max(0, notams_delayed_interim - available_staff_interim)
    available_staff <- max(0, available_staff_interim - notams_delayed_interim)
    remove(notams_delayed_interim, available_staff_interim)
    
    notams_to_process <- if (m %in% notams_by_minute$minutes_elapsed) {
      (notams_by_minute %>% filter(minutes_elapsed == m))$incoming_notams
    } else 0
    
    notams_accepted <- if(available_staff >= notams_to_process) {
      notams_to_process
    } else available_staff
    
    notams_ignored <- notams_to_process - notams_accepted
    available_staff <- available_staff - notams_accepted
    
    notams_processing[1] <- notams_processing[1] + notams_accepted
    notams_delayed <- notams_delayed + notams_ignored
    
    # populate tibble columns (except processed which was done earlier)
    on_staff_column <- append(on_staff_column, on_staff[nearest_hour_index])
    available_column <- append(available_column, available_staff)
    incoming_column <- append(incoming_column, notams_to_process)
    delayed_column <- append(delayed_column, notams_delayed)
    unprocessed_column <- append(unprocessed_column, 0)
    
    m <- m+1
  }
  # return results ----
  if (m > length_of_analysis_period) {
    incoming_column[(length_of_analysis_period+1):m] <- 0
  }
  return_value <- tibble(
    on_staff = on_staff_column,
    available = available_column,
    incoming_notams = incoming_column,
    delayed_notams = delayed_column,
    processed_notams = processed_column,
    unprocessed_notams = unprocessed_column
  )
}

convert_shift_start_to_on_staff <- function(shift_hour_starts,
                                            shift_length = 8) {
  on_staff <- integer(length(shift_hour_starts))
  for (i in 1:length(shift_hour_starts)) {
    to_add <- shift_hour_starts[i]
    for (w in (1:shift_length) - 1) {
      full_index <- (i+w-1) %% length(on_staff) + 1
      on_staff[full_index] <- on_staff[full_index] + to_add
    }
  }
  on_staff
}

# helper function, output is dt_busy_day
get_notams_for_day <- function(day, regions) {
  dt_busy_day <- dt %>%
    filter(Action.Date >= day &
             Action.Date < day + 1 &
             Region %in% regions) %>%
    group_by(Action.Date) %>%
    summarize(incoming_notams=n()) %>%
    mutate(minutes_elapsed = (60 * hour(Action.Date)) + minute(Action.Date),
           cumulative_notams = cumsum(incoming_notams)) %>%
    select(minutes_elapsed, incoming_notams, cumulative_notams) %>%
    arrange(minutes_elapsed)
}

# helper function, output is simulated_day
eliminate_unprocessed_notams <- function(dt_busy_day) {
  on_staff <- integer(hours_in_day)
  shift_hour_starts <- integer(hours_in_day)
  average_delay <- Inf
  unprocessed_notams <- sum(dt_busy_day$incoming_notams)
  window_start <- (1:hours_in_day) - 1
  acceptable_increase <- T
  
  print("Getting to 0 unprocessed NOTAMs...")
  while (unprocessed_notams > 0 || 0 %in% on_staff) {
    if (unprocessed_notams == 0 && acceptable_increase) {
      average_delay <- Inf
      acceptable_increase <- F
    }
    fewest_unprocessed <- Inf
    shortest_delay <- Inf # tiebreaker
    best_hour <- hours_in_day
    best_model <- on_staff
    
    for (w in window_start) {
      if (unprocessed_notams == 0 && !(0 %in% on_staff[(w+1):(w+shift_length)]))
        next
      candidate_model <- on_staff
      for (i in (1:shift_length) - 1) {
        h = (w + i) %% hours_in_day
        candidate_model[h+1] <- candidate_model[h+1] + 1
      }
      
      simulation <- get_daily_processing_stats(candidate_model,
                                               dt_busy_day,
                                               notam_processing_time)
      total_unprocessed <- sum(simulation$unprocessed_notams)
      average_delay <- sum(simulation$delayed_notams) /
        sum(simulation$processed_notams)
      better_result <- total_unprocessed < fewest_unprocessed ||
        (total_unprocessed == fewest_unprocessed &&
           average_delay < shortest_delay)
      if (better_result) {
        fewest_unprocessed <- total_unprocessed
        shortest_delay <- average_delay
        best_hour <- w
        best_model <- candidate_model
        best_sim <- simulation
      }
    }
    
    if (best_hour < hours_in_day) {
      on_staff <- best_model
      shift_hour_starts[best_hour + 1] <- shift_hour_starts[best_hour + 1] + 1
      average_delay <- shortest_delay
      unprocessed_notams <- fewest_unprocessed
      simulated_day <- best_sim
      
      print(on_staff)
      print(c(unprocessed_notams, average_delay))
    }
    else stop("could not reach 0 unprocessed NOTAMs; something has gone very wrong")
  }
  
  print("Done")
  
  staff_starting <- pad_vector_with_zeros(shift_hour_starts,
                                          minutes_in_hour)
  
  staff_starting <- append(staff_starting,
                           integer(length(simulated_day$on_staff) -
                                     minutes_in_day))
  
  simulated_day <- simulated_day %>%
    mutate(staff_starting = staff_starting)
  
  simulated_day
}

# - day is as.Date("YYYY-MM-DD")
# - regions is a vector
# - delay_target is an int representing the maximum allowable average delay
# - notam_processing_time is an integer representing how long it takes for a
#   staff member to process a NOTAM once they get around to it.
# - shift_length is an int representing the length of a shift in hours
compute_staff_model <- function(day,
                                regions = c("Western", "Central", "Eastern"),
                                delay_target = 2,
                                notam_processing_time = 3,
                                shift_length = 8) {
  dt_busy_day <- get_notams_for_day(day, regions)
  print(paste(sum(dt_busy_day$incoming_notams), "NOTAMs to process."))
  
  simulated_day <- eliminate_unprocessed_notams(dt_busy_day)
  
  on_staff <- get_every_nth_element(simulated_day$on_staff,
                                    minutes_in_hour)[1:hours_in_day]
  shift_hour_starts <- get_every_nth_element(simulated_day$staff_starting,
                                             minutes_in_hour)[1:hours_in_day]
  
  average_delay <- Inf
  window_start <- (1:hours_in_day) - 1
  
  # get under max average delay ----
  print(paste("Getting under", max_delay_target, "minutes average delay..."))
  while (average_delay > max_delay_target) {
    shortest_delay <- Inf
    best_hour <- hours_in_day
    best_model <- on_staff
    
    for (w in window_start) {
      candidate_model <- on_staff
      for (i in 1:shift_length - 1) {
        h = (w + i) %% hours_in_day
        candidate_model[h+1] <- candidate_model[h+1] + 1
      }
      
      simulation <- get_daily_processing_stats(candidate_model,
                                                  dt_busy_day,
                                                  notam_processing_time)
      test_delay <- sum(simulation$delayed_notams) /
        sum(simulation$incoming_notams)
      if (test_delay < shortest_delay) {
        shortest_delay <- test_delay
        best_hour <- w
        best_model <- candidate_model
        best_sim <- simulation
      }
    }
    
    print(best_model)
    print(shortest_delay)
    
    if (best_hour < hours_in_day) {
      on_staff <- best_model
      shift_hour_starts[best_hour + 1] <- shift_hour_starts[best_hour + 1] + 1
      average_delay <- shortest_delay
      simulated_day <- best_sim
    }
    else stop("could not reduce average delay further; something has gone very wrong")
  }
  
  # get as close to max average delay without going over ----
  print("Removing unnecessary staff...")
  while (average_delay < max_delay_target) {
    shortest_delay <- Inf
    best_hour <- hours_in_day
    best_model <- on_staff
    
    for (w in window_start) {
      if (shift_hour_starts[w+1] <= 1) next
      
      candidate_model <- on_staff
      for (i in (1:shift_length) - 1) {
        h = (w + i) %% hours_in_day
        candidate_model[h+1] <- candidate_model[h+1] - 1
      }
      
      simulation <- get_daily_processing_stats(candidate_model,
                                                  dt_busy_day,
                                                  notam_processing_time)
      test_delay <- sum(simulation$delayed_notams) /
        sum(simulation$incoming_notams)
      if (test_delay < shortest_delay) {
        shortest_delay <- test_delay
        best_hour <- w
        best_model <- candidate_model
        best_sim <- simulation
      }
      # we're looking for the lowest increase in delay. in this case the increase
      # is zero
      if (shortest_delay == average_delay) break
    }
    
    print(best_model)
    print(shortest_delay)
    
    if (shortest_delay >= max_delay_target) break
    else {
      on_staff <- best_model
      shift_hour_starts[best_hour+1] <- shift_hour_starts[best_hour+1] - 1
      average_delay <- shortest_delay
      simulated_day <- best_sim
    }
  }
  
  staff_starting <- pad_vector_with_zeros(shift_hour_starts,
                                         minutes_in_hour)
  staff_starting <- append(staff_starting,
                           integer(length(simulated_day$on_staff) -
                                     minutes_in_day))
  simulated_day <- simulated_day %>%
    mutate(staff_starting = staff_starting)
  
  simulated_day
}

# - day is as.Date("YYYY-MM-DD")
# - number_of_staff is the number of staff to allocate shifts for
# - regions is a vector
# - notam_processing_time is an integer representing how long it takes for a
#   staff member to process a NOTAM once they get around to it.
# - shift_length is an int representing the length of a shift in hours
optimize_staff_allocation <- function(day, number_of_staff,
                                      regions = c("Western", "Central",
                                                  "Eastern"),
                                      notam_processing_time = 3,
                                      shift_length = 8) {
  dt_busy_day <- get_notams_for_day(day, regions)
  print(paste(sum(dt_busy_day$incoming_notams), "NOTAMs to process."))
  
  simulated_day <- eliminate_unprocessed_notams(dt_busy_day)
  
  on_staff <- get_every_nth_element(simulated_day$on_staff,
                                    minutes_in_hour)[1:hours_in_day]
  shift_hour_starts <- get_every_nth_element(simulated_day$staff_starting,
                                             minutes_in_hour)[1:hours_in_day]
  
  average_delay <- Inf
  window_start <- (1:hours_in_day) - 1
  
  while (sum(shift_hour_starts) < number_of_staff) {
    shortest_delay <- Inf
    best_hour <- hours_in_day
    best_model <- on_staff
    
    for (w in window_start) {
      candidate_model <- on_staff
      for (i in 1:shift_length - 1) {
        h = (w + i) %% hours_in_day
        candidate_model[h+1] <- candidate_model[h+1] + 1
      }
      
      simulation <- get_daily_processing_stats(candidate_model,
                                                  dt_busy_day,
                                                  notam_processing_time)
      test_delay <- sum(simulation$delayed_notams) /
        sum(simulation$incoming_notams)
      if (test_delay < shortest_delay) {
        shortest_delay <- test_delay
        best_hour <- w
        best_model <- candidate_model
        best_sim <- simulation
      }
    }
    
    print(best_model)
    print(shortest_delay)
    
    if (best_hour < hours_in_day) {
      on_staff <- best_model
      shift_hour_starts[best_hour + 1] <- shift_hour_starts[best_hour + 1] + 1
      average_delay <- shortest_delay
      simulated_day <- best_sim
    }
    else stop("could not reduce average delay further; something has gone very wrong")
  }
  
  staff_starting <- pad_vector_with_zeros(shift_hour_starts,
                                         minutes_in_hour)
  staff_starting <- append(staff_starting,
                           integer(length(simulated_day$on_staff) -
                                     minutes_in_day))
  simulated_day <- simulated_day %>%
    mutate(staff_starting = staff_starting)
  
  simulated_day
}

# helper function for get_maximum_delay
get_queue_clearing_time <- function(delayed_notams, on_staff, starting_index,
                                    notam_processing_time) {
  m <- ((starting_index - 1) %% minutes_in_day)
  max_available <- Inf
  available_staff <- max_available
  notams_processing <- integer(notam_processing_time)
  result <- 0
  while (delayed_notams > 0) {
    # if (starting_index == 1429) browser()
    # remove processed notams from queue
    notams_processed <- notams_processing[notam_processing_time]
    
    notams_processing <- c(0, notams_processing[1:notam_processing_time - 1])
    
    nearest_hour_index <- m %/% minutes_in_hour + 1
    previous_max_available <- max_available
    max_available <- on_staff[nearest_hour_index]
    
    available_staff <- min(max_available,
                           available_staff + notams_processed)
    if (previous_max_available < max_available) {
      available_staff <- available_staff + max_available - previous_max_available
    }
    remove(previous_max_available)
    
    # placing all available staff on delayed NOTAMs
    notams_processing[1] <- min(delayed_notams, available_staff)
    available_staff <- available_staff - notams_processing[1]
    delayed_notams <- delayed_notams - notams_processing[1]
    
    result <- result + 1
    m <- (m+1) %% minutes_in_day
  }
  result
}

get_maximum_delay <- function(staff_model, notam_processing_time) {
  delay <- 0
  for (i in 1:minutes_in_day) {
    os <- get_every_nth_element(staff_model$on_staff, minutes_in_hour)
    candidate <- get_queue_clearing_time(staff_model$delayed_notams[i],
                                         os,
                                         i,
                                         notam_processing_time)
    delay <- max(delay, candidate)
  }
  delay
}

# Plotting function with three panels
plot_staff_model <- function(day, staff_model,
                                Region, season = "",
                                focus_inset = F,
                                focus_start = "11:00:00",
                                focus_range = c(550, 650)) {
  # For testing: 
  # First, run compute_staff_reqd() function with a staffing model on a specific
  # day. Then test this function using these or similar values:
  # day = as.Date('2019-02-24'); processing_time_in_minutes = 3;
  # focus_inset = TRUE;  focus_start = "11:00:00"; Region = 'Eastern'
  
  region_set_name = ifelse(length(Region) > 1,
                           paste(Region, collapse = ' + '), Region)
  plural = ifelse(length(Region) > 1, "s", "")
  colors <- c('#fb8072', '#80b1d3', "#b3de69")
  
  for_melting <- staff_model %>%
    mutate(hour = (1:length(staff_model$on_staff) - 1) / minutes_in_hour,
           .before = 1)
  for_melting$incoming_notams <- cumsum(for_melting$incoming_notams)
  for_melting$processed_notams <- cumsum(for_melting$processed_notams)
  for_melting$available <- cumsum(for_melting$available) / 60
  
  melted_model = melt(for_melting, id = "hour",
                      measure = c("on_staff",
                                  "staff_starting",
                                  "available",
                                  "incoming_notams",
                                  "processed_notams"))
  remove(for_melting)
  
  # print(head(s_df_long))
  # begin: plot cumulative curves
  # two panels, one showing just the cumulative curve and the other showing
  # staff count and staff time available for non-NOTAM tasks 
  p <- ggplot(data = melted_model) + xlab("Zulu time (hours)")
  
  pc <- p + 
    geom_step(data = melted_model %>% filter(variable %in%
                                            c("incoming_notams",
                                              "processed_notams")),
              aes(hour, value, colour = variable),
              size = 1.25) +
    scale_x_continuous(breaks = 0:(hours_in_day / 4) * 4) + 
    guides(colour = "legend", linetype = FALSE) +
    scale_color_discrete(name = NULL, labels = c("NOTAMs Arrived for Processing",
                                                 "NOTAMs Departing Queue")) +
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
          "Avg. delay-minutes: ",
            round(sum(staff_model$delayed_notams) /
                    sum(staff_model$incoming_notams), 2),
          "; Max delay-minutes: ",
            get_maximum_delay(staff_model, notam_processing_time),
          "\n Staff hours available for non-NOTAM tasks: ",
            round(sum(staff_model$available) / 60, 1),
          "; Staff total hours: ",
            sum(staff_model$on_staff[1:minutes_in_day] / 60)
        )
      )
    )
  
  staff_range = range(melted_model %>% filter(variable == 'staff_starting') %>%
                        select(value))
  
  pc_top <- p + 
    geom_col(data = melted_model %>%
               filter(variable == "staff_starting" &
                        hour == floor(hour) &
                        hour < hours_in_day) %>%
               select(hour, value),
             aes(hour, value),
             size = 1.25,
             fill = colors[1]) +
    scale_x_continuous(breaks = (1:hours_in_day) - 1) +
    theme(legend.position = "none", # Remove the legend
          # axis.title.x = element_blank(),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10)) +   
    ylab("Staff starting shifts\n") +
    labs(title = paste0(region_set_name, ' Service Area', plural, ' - ', day))
  
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
    scale_x_continuous(breaks = 0:(hours_in_day / 4) * 4) + 
    scale_y_continuous(
      "Cumulative downtime\n(hours)", 
      sec.axis = sec_axis(~ .* 100 * minutes_in_hour /
                            sum(staff_model$on_staff[1:minutes_in_day]),
                          name = "Cumulative downtime\n(percent of total specialist hours)")
    )
  
  if(diff(staff_range) > 0){
    pc_top_2 <- pc_top_2 + geom_smooth(data = melted_model %>%
                                         filter(variable == "available") , 
                                       aes(hour, value),
                                       size = 1.25, colour = colors[3]) 
  } else {
    pc_top_2 <- pc_top_2 + geom_line(data = melted_model %>%
                                       filter(variable == "available") , 
                                     aes(hour, value),
                                     size = 1.25, colour = colors[3]) 
  }
  
  # Put these together. egg::ggarrange is better than grid.arrange, because it
  # preserves common axes correctly.
  
  ga <- egg::ggarrange(pc_top,
                       pc_top_2,
                       pc,
                       ncol = 1,
                       heights = c(1, 1, 1))
  
  region_set_short <- paste(substr(Region, 1, 1), collapse="")
  
  ggsave(filename = file.path(output_dir, paste0(region_set_short, '_ninetieth_',
                                                 day, '_', season, '_',
                                                 max_delay_target,
                                                 'min_target.png')),
         plot = ga,
         width = 6, height = 7)
  
  # # Focus plot
  # if(focus_inset){
  #   
  #   start_min = s_df %>% select(minute) %>% filter(grepl(focus_start,
  #                                                        format(s_df$minute,
  #                                                               '%H:%M:%S')))
  #   end_min = start_min + 60*60*1 # 1 hour
  #   
  #   focus_pc <- pc +
  #     xlim(unclass(start_min)$minute, unclass(end_min)$minute) +
  #     ylim(focus_range[1], focus_range[2])
  #   
  #   ggsave(filename = paste0('Focus_', region_set_name, '_ninetieth_', day,
  #                            '_', season, '.png'),
  #          plot = focus_pc,
  #          width = 5, height = 4)
  #   
  #   
  # }
  
  # end: plot cumulative curves
}

# generate plots for NOTAM counts by day & demand for all 90th percentile days
source('NOTAM_demand_plots.R')

# plot regions & save models
regions <- c('Western', 'Central', 'Eastern')
for (i in 1:length(regions)) {
  region_set_short <- paste(substr(regions[1:i], 1, 1), collapse="")
  print(paste('###### ', paste(regions[1:i], collapse=" + "), ' ######'))
  staff_model <- compute_staff_model(ninetieth_days$date[i], regions[1:i])
  write.csv(staff_model, file.path(output_dir, paste0('staff_model_90th_',
                                                      region_set_short,
                                                      '.csv')))
  plot_staff_model(ninetieth_days$date[i], staff_model, regions[1:i])
  
  # plot seasons & save models
  ninetieth_day_rs <- ninetieth_day_seasons %>%
    filter(service_areas == concat_service_acronyms(regions, i))
  for (j in 1:length(ninetieth_day_rs$date)) {
    day <- ninetieth_day_rs$date[j]
    season <- ninetieth_day_rs$season[j]
    print(paste('###### ', concat_service_acronyms(regions, i), season, day,
                ' ######'))
    staff_model <- optimize_staff_allocation(day,
                                             sum(staff_model$staff_starting),
                                             regions[1:i])
    write.csv(staff_model, file.path(output_dir, paste0('staff_model_90th_',
                                                        region_set_short, '_',
                                                        season, '.csv')))
    plot_staff_model(day, staff_model, regions[1:i], season)
  }
}
