load("C:/Users/Tema.Nwana/Downloads/NOTAM_Freq.RData")
library(ggplot2)

#?
min_time_on_call_worker <- 4L
# set block size to min_time_on_call_worker
block_size <- min_time_on_call_worker
# no. rows of the top_blocks table 
top_block_list_size <- 50

# earliest date of the unique interaction we are looking at
study_start <- min(UniqueInteractions$datetimes)
# total hours of data we are looking at (+1 because vector is (inclusive, non-inclusive))
study_length <- as.integer(1 + difftime(max(UniqueInteractions$datetimes), study_start, units = "hours"))

# vector of numbers from 1 to study length (see above)
hourly_bin_start <- c(seq(1, study_length))
# vector of times in UTC of each start time every hour in the study 
hourly_bin_start <- as.POSIXct((hourly_bin_start-1)*(60*60) + study_start, origin = "1970-01-01", tz = "UTC")
# vector of times in UTC of each end time every hour in the study 
hourly_bin_end <- hourly_bin_start + 60*60

# example row: [2017-10-03 00:02:00, 2017-10-03 01:02:00, -1, -1, -1]
df = 
  # head(
  data.frame(hourly_bin_start, hourly_bin_end, hourly_count = -1, block_count = -1, block_rank_descending = -1)
  # , 1000)

# if "hour_number" is not a column name in the UniqueInteractions table
if (!("hour_number" %in% colnames(UniqueInteractions))){
  # list of differences in hours of each entry from the study_start (sea line 12)
  # (there are less hours than there are entries multiple entries in some hour periods)
  hour_number <- as.integer(1 + difftime(UniqueInteractions$datetimes, study_start, units = "hours"))
  # add the hour_number list as a new column in the UniqueInteractions table
  UniqueInteractions <- cbind(UniqueInteractions, hour_number)
}

# loop through each row in df
for(i in 1:nrow(df)){
  # set the values in hourly_count column to the number of instances that occured i hour(s) of study_start
  df$hourly_count[i] = sum(i == UniqueInteractions$hour_number)
  # set the values in block_count column to the number of instances that occured each 4 hour block period
  # (a new period starts every hour. 1-5, 2-6, 3-7)
  df$block_count[i] = sum(i <= UniqueInteractions$hour_number & i + block_size > UniqueInteractions$hour_number)
}

# for(i in 1:study_length){
#   df$block_count[i] = sum(df$hourly_count[])
# }

# set the values of block_rank_descending column to the rank of the block based on its block count (1 is highest count) 
df$block_rank_descending = rank(-df$block_count, ties.method = "first")
# orig_df = re-ordered df by blocks_rank_descending
orig_df <- df[order(df$block_rank_descending),]
# reset df to orig_df
df <- data.frame(orig_df)

# top 50 blocks based on block count
top_blocks = head(orig_df[order(df$block_rank_descending),], top_block_list_size)

# initialize confirmed_blocks to "2017-12-14 14:02:00 UTC"
confirmed_blocks = 
  # as.POSIXct(
    top_blocks$hourly_bin_start[1]
    # )
# initialize last_confirmed_blocks
last_confirmed_block = confirmed_blocks
# loop
for(j in 2:(top_block_list_size)){
  # checking to see if the number of hours between the last_confirmed block and the values of hour_bin_start column is at least 4
  # if the values pass, keep them in the data frame
  df <- df[abs(difftime(last_confirmed_block, df$hourly_bin_start, units = "hours")) > block_size, ]#filter the entire data frame for entries that conflict, append the top entry from the data frame
  # set last_confirmed_block to the jth value of confirmed_blocks (the first index in the hourly_bin_start column)
  last_confirmed_block = confirmed_blocks[j] = 
    # as.POSIXct(
      head(df$hourly_bin_start,1)
      # )
}

# confirmed busiest periods (those without conflicts as described above)
busy_periods <- orig_df[orig_df$hourly_bin_start %in% as.vector(confirmed_blocks), ]
print(busy_periods)

# initialize cumalitive_count as an empty vector
cumulative_count <- vector()
# loop
for(k in 1:top_block_list_size){
  # set the kth value of cumalitive_count to the number of periods in busy_periods that have a block count <= the block count in the kth entry
  # cumulative_count[k] = top_block_list_size - k + 1
  cumulative_count[k] = sum(busy_periods$block_count <= busy_periods$block_count[k])
  # get the year, month, and date for kth entry
  y <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$year + 1900
  m <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mon + 1
  d <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mday
  # beginning of day
  bod <- as.POSIXct(
      # print(
        paste(y, m, d, sep = "-")
      # )
    , tz = "UTC")
  # add 1 day in units of seconds to bod (end of day)
  eod <- bod + 60*60*24
  # print graph showing the block counts starting every hour from bod to eod
  print(ggplot2::qplot(data = orig_df[orig_df$hourly_bin_start > bod & orig_df$hourly_bin_start < eod, ], x = hourly_bin_start, y = block_count))
  
}

# add cumalitive count vector as a column to busy_periods df
busy_periods <- cbind(busy_periods, cumulative_count)

# CDF of busiest 4 hour blocks
p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per 4-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest 4-hour blocks as a Cumulative Distribution Function")
      )
# save cdf
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)
