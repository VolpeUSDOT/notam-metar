# Setup ----
# Set working directory to location where NOTAM_Freq.RData exists

working_dir <- ifelse(grepl('Dan', path.expand('~/')),
                      'H:/Consult/NOTAM + METAR/NOTAM_Freq',
                      '<you_path_here>')

setwd(working_dir)

load("NOTAM_Freq.RData")
library(ggplot2)

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

# Save output to working directory ----

save(list = c('busy_periods',
              'orig_df',
              'allInteractions',
              'UniqueInteractions'),
     file = 'NOTAM_Freq_w_busy_days.RData')

