# Overview ----
# This calculates 'busy periods' in the NOTAM data by calculating how many NOTAMs per hour are incoming. 
# In addition, this script also builts off of the output of the models in NOTAM_Freq_Analysis.Rmd to estimate the intraday, seasonal, and weekend/weekday effect. We will use this for generating staffing models.


# Setup ----
# Set working directory to location where NOTAM_Freq.RData exists


working_dir <- ifelse(grepl('Dan', path.expand('~/')),
                      'H:/Consult/NOTAM + METAR/NOTAM_Freq',
                      '<you_path_here>')

setwd(working_dir)

load("NOTAM_Freq.RData")
load('NOTAM_Freq_Model_Out.RData')

library(tidyverse) # for ggplot2 + dplyr

# begin: busy period search parameters ----

min_hours_on_call_worker <- 4L
block_size <- min_hours_on_call_worker
top_block_list_size <- 25L

study_start <- min(UniqueInteractions$datetimes)
study_length <- as.integer(1 + difftime(max(UniqueInteractions$datetimes), study_start, units = "hours"))

# For each hour of the study period, create two vectors. First is a vector of starting hours, second is a vector of ending hours. Difference between start and end is 60 minutes. 
hourly_bin_start <- c(seq(1, study_length))
hourly_bin_start <- as.POSIXct((hourly_bin_start-1)*(60*60) + study_start, origin = "1970-01-01", tz = "UTC")
hourly_bin_end <- hourly_bin_start + 60*60

stopifnot(hourly_bin_end[1] - hourly_bin_start[1] == '1') 

#end: busy period search parameters

# Create a data frame where each row is an hourly bin.
df = 
  # head(
  data.frame(hourly_bin_start, hourly_bin_end, hourly_count = -1, block_count = -1, block_rank_descending = -1)
  # , 1000)

# Create hour_number variable in the UniqueInteractions data frame if not already there, starting from datetimes.
# The value of hour_number is the number of hours passed since the study_start time point
if (!("hour_number" %in% colnames(UniqueInteractions))){
  hour_number <- as.integer(1 + difftime(UniqueInteractions$datetimes, study_start, units = "hours"))
  UniqueInteractions <- cbind(UniqueInteractions, hour_number)
}

# Loop through each row, and enumerate how many times that hour number occurs in the original data frame.
# This at least could also be tidily-accomplished with UniqueInteractions %>% group_by(hour_number) %>% summarize(hourly_count = n())
# However, the second part is to assess the block count, which is not so easy to calculate in a tidy way. Here, a loop over each row is a reasonable approach, although slow.
# block_count = the count of NOTAMs for the block_size number of hours starting with this hour (default currently 4 hrs)

for(i in 1:nrow(df)){
  df$hourly_count[i] = sum(i == UniqueInteractions$hour_number)
  df$block_count[i] = sum(i <= UniqueInteractions$hour_number & i + block_size > UniqueInteractions$hour_number)
}

# set the values of block_rank_descending column to the rank of the block based on its block count (1 is highest count) 
df$block_rank_descending = rank(-df$block_count, ties.method = "first")
# orig_df = re-ordered df by blocks_rank_descending
orig_df <- df[order(df$block_rank_descending),]
df <- data.frame(orig_df)

# Select just the top_block_list_size number of rows, default 25 currently
top_blocks = head(orig_df[order(df$block_rank_descending),], top_block_list_size)

# Initialize the block start points, update to remove overlapping blocks from the top blocks list

confirmed_blocks = 
  # as.POSIXct(
    top_blocks$hourly_bin_start[1]
    # )

last_confirmed_block = confirmed_blocks

for(j in 2:(top_block_list_size)){
  # filter the entire data frame for entries that conflict, append the top entry from the data frame
  df <- df[abs(difftime(last_confirmed_block, df$hourly_bin_start, units = "hours")) > block_size, ]
  
  last_confirmed_block = confirmed_blocks[j] = 
    # as.POSIXct(
      head(df$hourly_bin_start,1)
      # )
}

busy_periods <- data.frame(cumulative_count = -1L, orig_df[orig_df$hourly_bin_start %in% as.vector(confirmed_blocks), ])
print(busy_periods)


# Adding staffing models ----

# Add to orig_df the staffing models.
# Staffing Models
# - Uniform 5 - current, assuming 5 FTE staff always on 
# - Uniform 2 - assume 2 FTE staff always on
# - Dynamic Seasonal Round-Up - Use the values from the exploratory data analysis (NOTAM_Freq_Analysis.Rmd..) and get the staff needed for each time period by:
#   ○ Season: Winter/Spring/Summer/Fall
#   ○ Weekend/Weekday:
#   ○ Off/Valley/Peak intraday
#   ○ Round up the number of estimated NOTAMs, convert to integer number of staff required per hour, to completely cover the NOTAM per hour, even if excess capacity will accumulate
# - Dynamic Seasonal Round-Down
#    ○ Same as above, but when calculating staff required to cover NOTAMs per hour, allow for a backlog to potentially accumulate by rounding down to the next smallest integer number of staff required.
# - Dynamic Aseasonal
# Same as above, but without seasonal variation, only weekend/weekday and intraday

uniform_5 = 5
uniform_2 = 2

# For the dynamic staffing models, we need to add season, weekend/weekday, and intraday periods to orig_df data frame.

orig_df <- orig_df %>% 
  mutate(dow = format(hourly_bin_start, '%A'),
         hour = format(hourly_bin_start, '%H'),
         weekend = as.factor(ifelse(dow == 'Saturday' | dow == 'Sunday', 'Weekend', 'Weekday')),
         month   = format(hourly_bin_start, '%m'),
         season  = as.factor(ifelse(month == '12' | month == '01' | month == '02', 'Winter',
                                    ifelse(month == '03' | month == '04' | month == '05', 'Spring',
                                           ifelse(month == '06' | month == '07' | month == '08', 'Summer',
                                                  ifelse(month == '09' | month == '10' | month == '11', 'Fall', NA))))),  
         peak2 = ifelse(hour >= 13 & hour <= 21, 'peak', 'off-peak'),
         peak3 = ifelse(hour >= 13 & hour <= 21, 'peak', 
                        ifelse(hour >= 5 & hour <= 10, 'valley', 'off-peak'))
         )
          

# Assume 3 min per NOTAM. For dyanmic staffing model round down, we take the estimated number of NOTAMs per hour from pred, multiply by 3 minutes, and use modulo division (integer) by 60 min to get the number of staff required at a minimum for that hour.
# if integer division results in 0, increment to 1.
# For conservative 'round up' model, take ceiling() of the NOTMAMs per hour * 3 min first, then divide by 60 min, then take ceiling of the result.

dynam_model_1 = ( pred$Estimate * 3 ) %/% 60
dynam_model_1[dynam_model_1 == 0] = 1

dynam_model_2 = ceiling( ( ceiling(pred$Estimate) * 3 ) / 60 )
dynam_model_2[dynam_model_2 == 0] = 1

pred = data.frame(pred, dynam_model_1, dynam_model_2)

orig_df <- left_join(orig_df, pred %>% select(Season,
                                          Weekend.Day,
                                          Intra.Day.Period,
                                          dynam_model_1,
                                          dynam_model_2),
                 by = c("season" = "Season",
                        "weekend" = "Weekend.Day",
                        "peak3" = "Intra.Day.Period"))

orig_df <- data.frame(orig_df, uniform_5, uniform_2)

# Save output to working directory ----

save(list = c('busy_periods',
              'orig_df',
              'allInteractions',
              'UniqueInteractions'),
     file = 'NOTAM_Freq_w_busy_days.RData')

# Plot staffing models ----

ggplot(orig_df %>% filter(hourly_bin_start > '2017-12-31' & hourly_bin_start <= '2018-12-31'), aes(x = hourly_bin_start)) + 
  geom_line(aes(y = dynam_model_1)) +
  #facet_wrap(~season + weekend)
  facet_wrap(~weekend)

# Example weeks
start1 = as.POSIXct('2018-01-01')
start2 = as.POSIXct('2018-04-01')
start3 = as.POSIXct('2018-07-01')
start4 = as.POSIXct('2018-10-01')


ex_weeks = c(seq(start1, start1 + 7 * 24 * 60 * 60, by = 24 * 60 * 60),
            seq(start2, start2 + 7 * 24 * 60 * 60, by = 24 * 60 * 60),
            seq(start3, start3 + 7 * 24 * 60 * 60, by = 24 * 60 * 60), 
            seq(start4, start4 + 7 * 24 * 60 * 60, by = 24 * 60 * 60)
            )

gp1 <- ggplot(orig_df %>% filter(format(hourly_bin_start, '%Y-%m-%d') %in% as.character(ex_weeks)), aes(x = hourly_bin_start)) + 
  geom_line(aes(y = dynam_model_1), size = 2, col = 'darkred', alpha = 0.6) + 
  geom_line(aes(y = dynam_model_2), size = 2, col = 'darkblue', alpha = 0.6) + 
  # geom_line(aes(y = uniform_2), size = 1.2, col = 'grey20') + 
  # geom_line(aes(y = uniform_5), size = 1.2, col = 'grey80') + 
  theme_bw() +
  ylab('Staff') + xlab('Date') +
  ggtitle('Visualization of two staffing models for example weeks in each season \n Red = Round-down, Blue = Round-up') +
  facet_wrap(~season, scales = 'free_x') 

ggsave(filename = 'NOTAM_Staffing_Models.png', width = 8, height = 6)
