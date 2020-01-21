load("NOTAM_Freq.RData")
library(ggplot2)

#begin: busy period search parameters
min_hours_on_call_worker <- 4L
block_size <- min_hours_on_call_worker
top_block_list_size <- 25L

study_start <- min(UniqueInteractions$datetimes)
study_length <- as.integer(1 + difftime(max(UniqueInteractions$datetimes), study_start, units = "hours"))

hourly_bin_start <- c(seq(1, study_length))
hourly_bin_start <- as.POSIXct((hourly_bin_start-1)*(60*60) + study_start, origin = "1970-01-01", tz = "UTC")
hourly_bin_end <- hourly_bin_start + 60*60
#end: busy period search parameters

df = 
  # head(
  data.frame(hourly_bin_start, hourly_bin_end, hourly_count = -1, block_count = -1, block_rank_descending = -1)
  # , 1000)

if (!("hour_number" %in% colnames(UniqueInteractions))){
  hour_number <- as.integer(1 + difftime(UniqueInteractions$datetimes, study_start, units = "hours"))
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

busy_periods <- data.frame(cumulative_count = -1L, orig_df[orig_df$hourly_bin_start %in% as.vector(confirmed_blocks), ])
print(busy_periods)


