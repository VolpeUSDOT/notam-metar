# Overview ----
# This calculates 'busy periods' in the NOTAM data by calculating how many NOTAMs per hour are incoming. 
# In addition, this script also builts off of the output of the models in NOTAM_Freq_Analysis.Rmd to estimate the intraday, seasonal, and weekend/weekday effect. We will use this for generating staffing models.


# Setup ----
# Set working directory to location where NOTAM_Freq.RData exists


working_dir <- ifelse(grepl('lylet', path.expand('~/')),
                      'C:/Users/lylet/OneDrive/Documents/NOTAM/input_output',
                      'H:/Consult/NOTAM + METAR/NOTAM_Freq')

setwd(working_dir)

load("NOTAM_Freq.RData")
load('NOTAM_Freq_Model_Out.RData')

library(tidyverse) # for ggplot2 + dplyr

# begin: busy period search parameters ----

#decide how much of the available data will be used for model building (versus model testing)
# min_hours_on_call_worker <- 4L
ignore_overlapping_busy_periods = TRUE
busy_periods_calendar_day_flag = TRUE
block_size_hours <- 24L #expecting integer 0-24
stopifnot(block_size_hours>=0L & block_size_hours<=24L & typeof(block_size_hours) == "integer")
earliest_datetime <- min(UniqueInteractions$datetimes)
print(earliest_datetime)
latest_datetime <- max(UniqueInteractions$datetimes)
print(latest_datetime)
earliest_complete_day <- 
  60*60*24 + 
  as.POSIXct( 
    paste(
    as.POSIXlt.POSIXct(earliest_datetime)$year + 1900
    , as.POSIXlt.POSIXct(earliest_datetime)$mon + 1
    , as.POSIXlt.POSIXct(earliest_datetime)$mday
    , sep = "-")
  , tz = "UTC")
latest_complete_day <-
  -60*60*24 + 
  as.POSIXct( 
    paste(
    as.POSIXlt.POSIXct(latest_datetime)$year + 1900
    , as.POSIXlt.POSIXct(latest_datetime)$mon + 1
    , as.POSIXlt.POSIXct(latest_datetime)$mday
    , sep = "-")
  , tz = "UTC")

if(block_size_hours == 24){ #leaving out the partial days 
  earliest_datetime <- earliest_complete_day
  latest_datetime <- latest_complete_day
}
print(earliest_datetime)
print(latest_datetime)

study_start <- earliest_datetime

# study_length_hours <- as.integer(1 + difftime(latest_datetime, study_start, units = "hours"))
study_length_whole_days <- as.integer(1 + difftime(latest_datetime, study_start, units = "days"))
  
share_of_study_for_model_building <- 0.5
model_build_days <- as.integer(study_length_whole_days * share_of_study_for_model_building)

model_build_hours <- model_build_days*24
offset <- 0 #e.g., 0 for model build, 60*60*model_build_hours for model test

top_block_list_size <- model_build_hours # 25L

# For each hour of the study period, create two vectors. First is a vector of starting hours, second is a vector of ending hours. Difference between start and end is 60 minutes. 
hourly_bin_start <-  c( seq( 1, model_build_hours ) ) #c( seq( model_build_hours + 1, study_length_hours) )
hourly_bin_start <- as.POSIXct((hourly_bin_start-1)*(60*60) + study_start + offset, origin = "1970-01-01", tz = "UTC")
hourly_bin_end <- hourly_bin_start + 60*60

stopifnot(hourly_bin_end[1] - hourly_bin_start[1] == '1') 

#end: busy period search parameters

# Create a data frame where each row is an hourly bin.
df = 
  # head(
  data.frame(hourly_bin_start, hourly_bin_end, hourly_count = -1, block_count = -1, block_rank_descending = -1)
# , 1000)

# # Create hour_number variable in the UniqueInteractions data frame if not already there, starting from datetimes.
# # The value of hour_number is the number of hours passed since the study_start time point
# if (!("hour_number" %in% colnames(UniqueInteractions))){
#   hour_number <- as.integer(1 + difftime(UniqueInteractions$datetimes, study_start, units = "hours"))
#   UniqueInteractions <- cbind(UniqueInteractions, hour_number)
# }

# Loop through each row, and enumerate how many times that hour number occurs in the original data frame.
# This at least could also be tidily-accomplished with UniqueInteractions %>% group_by(hour_number) %>% summarize(hourly_count = n())
# However, the second part is to assess the block count, which is not so easy to calculate in a tidy way. Here, a loop over each row is a reasonable approach, although slow.
# block_count = the count of NOTAMs for the block_size_hours number of hours starting with this hour (default currently 4 hrs)

for(i in 1:nrow(df)){
  df$hourly_count[i] = sum(UniqueInteractions$datetimes >= hourly_bin_start[i] & UniqueInteractions$datetimes < hourly_bin_end[i])
  df$block_count[i] = sum(UniqueInteractions$datetimes >= hourly_bin_start[i] & UniqueInteractions$datetimes < hourly_bin_start[i] + block_size_hours*60*60)
  # df$block_count[i] = sum(i <= UniqueInteractions$hour_number & i + block_size_hours > UniqueInteractions$hour_number)
}

# set the values of block_rank_descending column to the rank of the block based on its block count (1 is highest count) 
df$block_rank_descending = rank(-df$block_count, ties.method = "first")
# orig_df = re-ordered df by blocks_rank_descending
orig_df <- df[order(df$block_rank_descending),]
df <- data.frame(orig_df)

# Select just the top_block_list_size number of rows, default 25 currently
top_blocks = head(orig_df[order(df$block_rank_descending),], top_block_list_size)

# Initialize the block start points, update to remove overlapping blocks from the top blocks list
confirmed_blocks = data.frame()

if(block_size_hours == 1 || ignore_overlapping_busy_periods){
  busy_periods = confirmed_blocks = top_blocks
} else
{
  confirmed_blocks = top_blocks$hourly_bin_start[1]
  last_confirmed_block = confirmed_blocks
  
  for(j in 2:(top_block_list_size)){
    # filter the entire data frame for entries that conflict, append the top entry from the data frame
    df <- df[abs(difftime(last_confirmed_block, df$hourly_bin_start, units = "hours")) > block_size_hours, ]
    last_confirmed_block = confirmed_blocks[j] = head(df$hourly_bin_start,1)
  }
  busy_periods <- data.frame(cumulative_count = -1L, orig_df[orig_df$hourly_bin_start %in% as.vector(confirmed_blocks), ])
}

# Create beginning-of-day and end-of-day values for this day
busy_periods$bod <- as.POSIXct( paste(
  as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$year + 1900
  , as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$mon + 1
  , as.POSIXlt.POSIXct(busy_periods$hourly_bin_start)$mday
  , sep = "-"), tz = "UTC")
busy_periods$eod <- busy_periods$bod + 60*60*24

if(busy_periods_calendar_day_flag){
  busy_periods = busy_periods[busy_periods$bod == busy_periods$hourly_bin_start,]
}
top_block_list_size = nrow(busy_periods)

# Augment busy periods frame----
for (i in 1:top_block_list_size){ 
  busy_periods$cumulative_count[i] = sum(busy_periods$block_count <= busy_periods$block_count[i])
}

busy_periods$percentile = busy_periods$cumulative_count / top_block_list_size

print(busy_periods)

p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per x-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest x-hour blocks as a Cumulative Distribution Function")
)
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)

# # Adding staffing models ----
# 
# # Add to orig_df the staffing models.
# # Staffing Models
# # - Uniform 5 - current, assuming 5 FTE staff always on
# # - Uniform 2 - assume 2 FTE staff always on
# # - Dynamic Seasonal Round-Up - Use the values from the exploratory data analysis (NOTAM_Freq_Analysis.Rmd..) and get the staff needed for each time period by:
# #   --Season: Winter/Spring/Summer/Fall
# #   -- Weekend/Weekday:
# #   -- Off/Valley/Peak intraday
# #   -- Round up the number of estimated NOTAMs, convert to integer number of staff required per hour, to completely cover the NOTAM per hour, even if excess capacity will accumulate
# # - Dynamic Seasonal Round-Down
# #    -- Same as above, but when calculating staff required to cover NOTAMs per hour, allow for a backlog to potentially accumulate by rounding down to the next smallest integer number of staff required.
# # - Dynamic Aseasonal
# # Same as above, but without seasonal variation, only weekend/weekday and intraday
# 
# uniform_5 = 5
# uniform_2 = 2
# 
# # For the dynamic staffing models, we need to add season, weekend/weekday, and intraday periods to orig_df data frame.
# 
# orig_df <- orig_df %>%
#   mutate(dow = format(hourly_bin_start, '%A'),
#          hour = format(hourly_bin_start, '%H'),
#          weekend = as.factor(ifelse(dow == 'Saturday' | dow == 'Sunday', 'Weekend', 'Weekday')),
#          month   = format(hourly_bin_start, '%m'),
#          season  = as.factor(ifelse(month == '12' | month == '01' | month == '02', 'Winter',
#                                     ifelse(month == '03' | month == '04' | month == '05', 'Spring',
#                                            ifelse(month == '06' | month == '07' | month == '08', 'Summer',
#                                                   ifelse(month == '09' | month == '10' | month == '11', 'Fall', NA))))),
#          peak2 = ifelse(hour >= 13 & hour <= 21, 'peak', 'off-peak'),
#          peak3 = ifelse(hour >= 13 & hour <= 21, 'peak',
#                         ifelse(hour >= 5 & hour <= 10, 'valley', 'off-peak'))
#   )


# # Assume 3 min per NOTAM. For dyanmic staffing model round down, we take the estimated number of NOTAMs per hour from pred, multiply by 3 minutes, and use modulo division (integer) by 60 min to get the number of staff required at a minimum for that hour.
# # Allow periods of 0 staff in this model.
# # For conservative 'round up' model, take ceiling() of the NOTMAMs per hour * 3 min first, then divide by 60 min, then take ceiling of the result.
# # if integer division results in 0, increment to 1.
# 
# dynam_model_1 = ( pred$Estimate * 3 ) %/% 60
# dynam_model_1[dynam_model_1 == 0] = 1
# 
# dynam_model_2 = ceiling( ( ceiling(pred$Estimate) * 3 ) / 60 )
# dynam_model_2[dynam_model_2 == 0] = 1
# 
# # Also make aseasonal dynamic model. Could do floor or ceiling, or both, here just doing round for now
# pred_aseason = pred %>%
#   group_by(`Intra-Day Period`, `Weekend/Day`) %>%
#   summarize(Estimate = mean(Estimate),
#             SD = mean(StdDev)) %>%
#   mutate(dynam_model_3 = round( (Estimate * 3 ) / 60 ))
# 
# pred_aseason$dynam_model_3[pred_aseason$dynam_model_3 == 0] = 1
# 
# pred = left_join(pred, pred_aseason %>% select(`Intra-Day Period`, `Weekend/Day`, dynam_model_3))
# 
# pred = data.frame(pred, dynam_model_1, dynam_model_2)
# 
# orig_df <- left_join(orig_df, pred %>% select(Season,
#                                               Weekend.Day,
#                                               Intra.Day.Period,
#                                               dynam_model_1,
#                                               dynam_model_2,
#                                               dynam_model_3),
#                      by = c("season" = "Season",
#                             "weekend" = "Weekend.Day",
#                             "peak3" = "Intra.Day.Period"))
# 
# orig_df <- data.frame(orig_df, uniform_5, uniform_2)
# 

# # Plot staffing models ----
# 
# ggplot(orig_df %>% filter(hourly_bin_start > '2017-12-31' & hourly_bin_start <= '2018-12-31'), aes(x = hourly_bin_start)) +
#   geom_line(aes(y = dynam_model_1)) +
#   #facet_wrap(~season + weekend)
#   facet_wrap(~weekend)
# 
# # Example weeks
# start1 = as.POSIXct('2018-01-01')
# start2 = as.POSIXct('2018-04-02')
# start3 = as.POSIXct('2018-07-02')
# start4 = as.POSIXct('2018-10-01')
# 
# 
# ex_weeks = c(seq(start1, start1 + 6 * 24 * 60 * 60, by = 24 * 60 * 60),
#              seq(start2, start2 + 6 * 24 * 60 * 60, by = 24 * 60 * 60),
#              seq(start3, start3 + 6 * 24 * 60 * 60, by = 24 * 60 * 60),
#              seq(start4, start4 + 6 * 24 * 60 * 60, by = 24 * 60 * 60)
# )
# 
# gp1 <- ggplot(orig_df %>% filter(format(hourly_bin_start, '%Y-%m-%d') %in% as.character(ex_weeks)), aes(x = hourly_bin_start)) +
#   geom_line(aes(y = dynam_model_1), size = 1.2, col = 'darkred', alpha = 0.4) +
#   geom_line(aes(y = dynam_model_2), size = 1.2, col = 'darkblue', alpha = 0.4) +
#   geom_line(aes(y = dynam_model_3), size = 1.2, col = 'darkgreen', alpha = 0.4) +
#   # geom_line(aes(y = uniform_2), size = 1.2, col = 'grey20') +
#   # geom_line(aes(y = uniform_5), size = 1.2, col = 'grey80') +
#   theme_bw() +
#   ylab('Staff') + xlab('Date') +
#   ggtitle('Visualization of two staffing models for example weeks in each season \n Red = Round-down, Blue = Round-up, Green = Aseasonal') +
#   facet_wrap(~season, scales = 'free_x')
# gp1
# 
# ggsave(filename = 'NOTAM_Staffing_Models_all3.png', width = 8, height = 6)
# 
# 
# gp2 <- ggplot(orig_df %>% filter(format(hourly_bin_start, '%Y-%m-%d') %in% as.character(ex_weeks)), aes(x = hourly_bin_start)) +
#   geom_line(aes(y = dynam_model_1), size = 1.2, col = 'darkred', alpha = 0.4) +
#   geom_line(aes(y = dynam_model_2), size = 1.2, col = 'darkblue', alpha = 0.4) +
#   #  geom_line(aes(y = dynam_model_3), size = 1.2, col = 'darkgreen', alpha = 0.4) +
#   # geom_line(aes(y = uniform_2), size = 1.2, col = 'grey20') +
#   # geom_line(aes(y = uniform_5), size = 1.2, col = 'grey80') +
#   theme_bw() +
#   ylab('Staff') + xlab('Date') +
#   ggtitle('Visualization of two staffing models for example weeks in each season \n Red = Round-down, Blue = Round-up') +
#   facet_wrap(~season, scales = 'free_x')
# gp2
# 
# ggsave(filename = 'NOTAM_Staffing_Models_2.png', width = 8, height = 6)




# Save output to working directory ----

save(list = c('busy_periods',
              'orig_df',
              'allInteractions',
              'UniqueInteractions'),
     file = 'NOTAM_Freq_w_busy_days.RData')
