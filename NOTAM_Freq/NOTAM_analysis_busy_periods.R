load("C:/Users/Tema.Nwana/Downloads/NOTAM_Freq.RData")
library(ggplot2)

#?
min_time_on_call_worker <- 4L
#?
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
  # (not equal to number of entries; there are less hours than there are entries multiple entries in some hour periods)
  hour_number <- as.integer(1 + difftime(UniqueInteractions$datetimes, study_start, units = "hours"))
  # add the hour_number list as a new column in the UniqueInteractions table
  UniqueInteractions <- cbind(UniqueInteractions, hour_number)
}

for(i in 1:nrow(df)){
  df$hourly_count[i] = sum(i == UniqueInteractions$hour_number)
  df$block_count[i] = sum(i <= UniqueInteractions$hour_number & i + block_size > UniqueInteractions$hour_number)
}

# for(i in 1:study_length){
#   df$block_count[i] = sum(df$hourly_count[])
# }

df$block_rank_descending = rank(-df$block_count, ties.method = "first")
orig_df <- df[order(df$block_rank_descending),]
df <- data.frame(orig_df)

top_blocks = head(orig_df[order(df$block_rank_descending),], top_block_list_size)

confirmed_blocks = 
  # as.POSIXct(
    top_blocks$hourly_bin_start[1]
    # )
last_confirmed_block = confirmed_blocks
for(j in 2:(top_block_list_size)){
  df <- df[abs(difftime(last_confirmed_block, df$hourly_bin_start, units = "hours")) > block_size, ]#filter the entire data frame for entries that conflict, append the top entry from the data frame
  last_confirmed_block = confirmed_blocks[j] = 
    # as.POSIXct(
      head(df$hourly_bin_start,1)
      # )
}

busy_periods <- orig_df[orig_df$hourly_bin_start %in% as.vector(confirmed_blocks), ]
print(busy_periods)

cumulative_count <- vector()
for(k in 1:top_block_list_size){
  cumulative_count[k] = sum(busy_periods$block_count <= busy_periods$block_count[k])
  y <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$year + 1900
  m <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mon + 1
  d <- as.POSIXlt.POSIXct(busy_periods$hourly_bin_start[k])$mday
  bod <- as.POSIXct(
      # print(
        paste(y, m, d, sep = "-")
      # )
    , tz = "UTC")
  eod <- bod + 60*60*24
  print(ggplot2::qplot(data = orig_df[orig_df$hourly_bin_start > bod & orig_df$hourly_bin_start < eod, ], x = hourly_bin_start, y = block_count))
  
}

busy_periods <- cbind(busy_periods, cumulative_count)

p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per 4-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest 4-hour blocks as a Cumulative Distribution Function")
      )
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)
