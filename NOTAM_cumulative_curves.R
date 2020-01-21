load("NOTAM_Freq_w_busy_days.RData")
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

compute_staff_reqd <- function(day) {
  day_as_datetime <- as.POSIXct.Date(day, origin = "1970-01-01", tz = "UTC")
  minutes_per_day = 24L*60L
  minute <- c(seq(1:minutes_per_day))
  arrivals <- msgs_processed <- staff_required <- vector(length = minutes_per_day)
  arrivals_on_day <- UniqueInteractions[UniqueInteractions$date == day,]
  # ordered_arrivals <- arrivals_on_day[order(arrivals_on_day$datetimes),]
  
  minute <- day_as_datetime + (minute-1)*60
  staffing_df <- data.frame(minute, arrivals, msgs_processed, staff_required)
  
  for(n in 1:minutes_per_day){
    staffing_df$arrivals[n] = sum(arrivals_on_day$datetimes == staffing_df$minute[n])
    staffing_df$cml_arrivals[n] = sum(arrivals_on_day$datetimes <= staffing_df$minute[n])
  }
  
  staffing_df$staff_required = ceiling(staffing_df$arrivals / average_NOTAM_processing_time_in_minutes)
  
  #begin: plot cumulative curve with estimated instantaneous staffing level
  p <- ggplot(data = staffing_df) + xlab("minute") 
  
  pc <- p + 
    geom_line(aes(x = minute, y = cml_arrivals)) + 
    ylab("Cumulative NOTAMs")
  
  ps <- p + 
    geom_point(aes(x = minute, y = staff_required)) + 
    ylab("instantaneous staffing requirement")
  
  g1 <- ggplotGrob(pc)
  g2 <- ggplotGrob(ps)
  g <- rbind(g1, g2, size = "first")
  g$widths <- unit.pmax(g1$widths, g2$widths)
  grid.newpage()
  grid.draw(g)
  #end: plot cumulative curve with estimated instantaneous staffing level
}

#begin: staffing paramaters
average_NOTAM_processing_time_in_minutes <- 3 #per Bruce W, each processor can process 480 NOTAMs per day
# day = as.Date("2019-08-07", tz = "UTC") #testing
#end: staffing parameters
for(k in 1:top_block_list_size){
  busy_periods$cumulative_count[k] = sum(busy_periods$block_count <= busy_periods$block_count[k])
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
  day = as.Date(bod)
  compute_staff_reqd(day)  
}

p <- ggplot(data = busy_periods)
print(p 
      + geom_point(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + geom_line(aes(x = block_count, y = cumulative_count/top_block_list_size)) 
      + xlab("NOTAMs per 4-hour block") 
      + ylab("Share of busiest blocks having equal or fewer NOTAMs")
      + labs(title = "Busiest 4-hour blocks as a Cumulative Distribution Function")
)
ggsave("NOTAM_busy_periods_CDF.png", width = 10, height = 5)

