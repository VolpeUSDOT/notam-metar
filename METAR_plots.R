# Making METAR plots
# Brief script for plotting METAR values for example airport stations

library(tidyverse)
library(readxl)
library(gridExtra)

project_dir = '~/Notam_Local' 

setwd(project_dir)

d = read.csv('METAR_extract.csv', na.strings = "M")

n = read_xlsx('notam_extract.xlsx', sheet = 'data')

# Format data ----

d = d %>%
  mutate(vsby = as.numeric(vsby),
         skyl1 = as.numeric(skyl1),
         skyl2 = as.numeric(skyl2),
         skyl3 = as.numeric(skyl3),
         skyl4 = as.numeric(skyl4),
         valid = as.POSIXct(valid, format = "%Y-%m-%d %H:%M", tz = "UTC"),
         date = format(valid, "%Y-%m-%d"),
         hr = format(valid, "%Y-%m-%d"))

d %>%
  group_by(station) %>%
  summarize(vis_min = min(vsby, na.rm=T),
            vis_max = max(vsby, na.rm=T),
            vis_mean = mean(vsby, na.rm=T))

# Subset to just stations in d, format time variables. Reformat d$station to include leading K

n = n %>%
  filter(notam_location %in% ifelse(nchar(as.character(d$station)) == 3, paste0("K", d$station), d$station)) %>%
  mutate(station = as.factor(notam_location),
         start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),
         end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),
         start_hr = format(start_time, format = "%Y-%m-%d %H"),
         end_hr = format(end_time, format = "%Y-%m-%d %H"))


# Count active by station, date, and hour
# Simple way: loop, place in date range by hour
hr_range = with(n, range(start_time, end_time))
start_day = format(min(hr_range), '%Y-%m-%d')
end_day = format(max(hr_range), '%Y-%m-%d')

day_hour <- seq(from = as.POSIXct(paste0(start_day, "00:00"), tz = 'UTC'), 
                  to = as.POSIXct(paste0(end_day, " 23:00"), tz = 'UTC'),
                  by = "hour")

day_hr_ct <- vector()

for(i in day_hour){
  # i = day_hour[100]
  n_filt = n %>%
    filter(start_time <= i & end_time >= i) %>%
    group_by(notam_location) %>%
    summarize(count_active = n())
  
  day_hr_ct = rbind(day_hr_ct, n_filt)

  }

day_hr_ctx = data.frame(day_hr_ct, day_hour)

# Plotting ----
# Plot a dual-axis figure for visibility and sky level 1

d_filter = d %>%
  filter(station %in% c("MSY"
                        #, "PHL", "LAX"
                        )) %>%
  filter(date >= '2019-04-01' & date < '2019-05-01')
  
# ggplot(d_filter, aes(group = station)) +
#   geom_line(aes(x = valid, y = vsby, color = 'Visibility')) +
#   geom_line(aes(x = valid, y = skyl1, color = 'Ceiling')) +
#   scale_y_continuous(sec.axis = sec_axis(~. * 1000, name = "Sky Level 1 Altitude (ft)")) +
#   facet_wrap(~station) +
#   theme_bw()
# 
# 
# ggplot(d_filter, aes(group = station)) +
# #  geom_line(aes(x = valid, y = vsby, color = 'Visibility')) +
#   geom_line(aes(x = valid, y = skyl1, color = 'Ceiling')) +
# #  scale_y_continuous(sec.axis = sec_axis(~., name = "Sky Level 1 Altitude (ft)")) +
#   facet_wrap(~station) +
#   theme_bw()


sky_plot = ggplot(d_filter, aes(group = station)) +
  geom_line(aes(x = valid, y = skyl1), color = 'blue') +
  facet_wrap(~station) +
  xlab("Date") +
  ylab("Sky Level 1 Altitude (ft)") +
  theme_bw()

vis_plot = ggplot(d_filter, aes(group = station)) +
  geom_line(aes(x = valid, y = vsby), color = 'red') +
  facet_wrap(~station) +
  xlab("Date") +
  ylab("Visibility (mi)") +
  theme_bw()

grid.arrange(sky_plot, vis_plot)
