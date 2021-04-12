# TEST ALGORITHM 1 - 2021/03/25
# - Overstaff the NOTAM processing office.
  # - This can be achieved by finding the busiest hour in the day, calculating
  #   the man-hours required to process every NOTAM in that hour with an average
  #   processing delay of 2 minutes, then staffing the entire day with the
  #   number of staff required for that hour rounded up to the nearest multiple
  #   of 8.
# - While the average delay in processing is less than 2 minutes:
  # - Calculate the average delay in processing NOTAMs for the day.
    # - Assume a NOTAM is processed immediately unless everyone on staff is
    #   currently processing a NOTAM. Unfortunately this assumption does not
    #   respect people's need to step out for whatever reason, including
    #   bathroom breaks.
    # - Assume it takes exactly 3 minutes to process any given NOTAM.
  # - For each of 24 sliding 8-hour windows ([0:23], [0:23]+8 mod 24):
    # - For X people originally on staff, skip this window if X/8 people have
    #   already been removed from this window.
    # - Skip this window if only 1 person is on staff during any
    #   hour in the window.
    # - Create some kind of "minimum effect on delay" variable with associated
    #   "first hour of window with minimum effect on delay" variable.
    # - Reduce the amount of people on staff by 1 for each hour in the window.
    # - Calculate the average delay in processing NOTAMs for the day.
    # - "Effect on delay" is the sum of the differences for each hour within the
    #   window.
    # - If the effect on delay is smaller than the minimum, reset the minimum
    #   and note the first hour of this window.
  # - If the minimum effect on delay keeps the average delay under two minutes,
  #   remove the associated window. Otherwise, the staff model you currently
  #   have is the best one this algorithm can find.

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)

# all time variables are in minutes unless otherwise specified.
notam_processing_time <- 3
max_delay_target <- 2
shift_length <- 8
seconds_in_hour <- 3600
hours_in_day <- 24
minutes_in_day <- hours_in_day * 60

# find the busiest day ----
# TODO that actual process is skipped here, i need to go back and put it in
beginning_of_day = ninetieth_day_NAS$date
end_of_day = beginning_of_day + 1
dt_busy_day <- dt %>%
  filter(Action.Date >= beginning_of_day & Action.Date < end_of_day) %>%
  group_by(Action.Date) %>%
  summarize(incoming_notams=n()) %>%
  mutate(minutes_elapsed = (60 * hour(Action.Date)) + minute(Action.Date),
         cumulative_notams = cumsum(incoming_notams)) %>%
  select(minutes_elapsed, incoming_notams, cumulative_notams) %>%
  arrange(minutes_elapsed)
# # RANDOM DATA TEST ONLY
# dt_busy_day = tibble(minutes_elapsed = (1:minutes_in_day) - 1,
#                      incoming_notams = sample(0:20, minutes_in_day,
#                                               replace=TRUE),
#                      cumulative_notams = cumsum(incoming_notams))

# find the busiest hour ----
busiest_hour <- which.max(this_day$hourly_count)
beginning_of_hour <- this_day$yr_hour[busiest_hour]
end_of_hour <- beginning_of_hour + seconds_in_hour
dt_busy_hour <- dt %>%
  filter(Action.Date >= beginning_of_hour & Action.Date < end_of_hour) %>%
  group_by(Action.Date) %>%
  summarize(incoming_notams=n()) %>%
  mutate(minutes_elapsed = minute(Action.Date),
         cumulative_notams = cumsum(incoming_notams)) %>%
  select(minutes_elapsed, incoming_notams, cumulative_notams) %>%
  arrange(minutes_elapsed)
# # RANDOM DATA TEST ONLY #
# busiest_hour = which.max((dt_busy_day %>%
#   mutate(hours_elapsed = minutes_elapsed %/% 60) %>%
#   group_by(hours_elapsed) %>%
#   summarize(n=sum(incoming_notams)))$n)
# dt_busy_hour = dt_busy_day %>%
#   filter((minutes_elapsed %/% 60) + 1 == busiest_hour)
# dt_busy_hour$minutes_elapsed <- dt_busy_hour$minutes_elapsed -
#   dt_busy_hour$minutes_elapsed[1]

# calculate average delay function ----
# - on_staff is a vector where each entry represents the number of people on
#     staff at that hour.
# - notams_by_minute is a tibble with two columns: minuntes_elapsed and n, where
#     minutes_elapsed is the number of minutes from whatever starting time and
#     n is the number of NOTAMs coming in at that minute.
# - notam_processing_time is an integer representing how long it takes for a
#   staff member to process a NOTAM once they get around to it.
calculate_average_delay <- function(on_staff, notams_by_minute,
                                    notam_processing_time) {
  # initialize variables ----
  available_staff <- on_staff[1]
  total_notams <- sum(notams_by_minute$incoming_notams)
  notams_delayed <- 0
  total_delay <- 0
  # explanation of this vector: let's say a NOTAM takes 3 minute to process, if
  # this vector is (2 1 3), that means 2 NOTAMS have just been added, 1 has been
  # processing for one minute, and 3 have been processing for 2 minutes.
  notams_processing <- integer(notam_processing_time)
  m <- 0
  length_of_analysis_period = 60 * length(on_staff)
  
  # process NOTAMs ----
  # handle all incoming NOTAMs for the hour, then process all leftover NOTAMs to
  # calculate average delay for all NOTAMs which came in during that hour
  while (m < length_of_analysis_period || available_staff == 0) {
    max_available <- on_staff[(min(length_of_analysis_period-1, m)%/%60) + 1]
    available_staff <- min(max_available, available_staff +
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
    total_delay <- total_delay + notams_delayed
    m <- m+1
  }
  
  # return average delay ----
  average_delay <- total_delay / total_notams
}

# find minimum multiple of 8 ----
# for staff to handle busiest hour with a delay of less than 2 minutes
on_staff <- 8
hour_avg_delay <- Inf
while (hour_avg_delay > max_delay_target) {
  on_staff <- on_staff + shift_length
  hour_avg_delay <- calculate_average_delay(on_staff, dt_busy_hour,
                                           notam_processing_time)
}

# find optimal staffing arrangement ----
max_removals_per_hour <- on_staff / shift_length
removals <- integer(hours_in_day)

on_staff <- rep(on_staff, each=hours_in_day)
day_avg_delay <- calculate_average_delay(on_staff, dt_busy_day,
                                         notam_processing_time)
window_start <- 1:hours_in_day - 1
while (day_avg_delay < max_delay_target) {
  best_avg_delay <- Inf
  hour_of_min_dev <- hours_in_day
  on_staff_candidate <- on_staff
  
  for (w in window_start) {
    if (removals[w+1] == max_removals_per_hour) next
    if (1 %in% c(on_staff, on_staff)[(w+1):(w+shift_length)]) next
    
    on_staff_test <- on_staff
    for (i in 1:shift_length - 1) {
      h = (w + i) %% hours_in_day
      on_staff_test[h+1] <- on_staff_test[h+1] - 1
    }
    
    test_avg_delay <- calculate_average_delay(on_staff_test, dt_busy_day,
                                              notam_processing_time)
    if (test_avg_delay < best_avg_delay) {
      hour_of_min_dev <- w
      on_staff_candidate <- on_staff_test
      best_avg_delay <- test_avg_delay
    }
    # we're looking for the lowest increase in delay. in this case the increase
    # is zero
    if (best_avg_delay == day_avg_delay) break
  }
  
  print(on_staff_candidate)
  print(best_avg_delay)
  
  if (best_avg_delay >= max_delay_target || best_avg_delay == Inf) break
  else {
    on_staff <- on_staff_candidate
    removals[hour_of_min_dev+1] <- removals[hour_of_min_dev+1] + 1
    day_avg_delay <- best_avg_delay
  }
}

dt_busy_day <- dt_busy_day %>% mutate(on_staff = on_staff[1 + (minutes_elapsed %/% 60)])
