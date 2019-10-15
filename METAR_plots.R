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
    summarize(count_active = n()) %>%
    mutate(day_hr = i)
  
  day_hr_ct = rbind(day_hr_ct, n_filt)

  }

day_hr_ct = day_hr_ct %>%
  mutate(day_hr = as.POSIXct(day_hr, origin = '1970-01-01', tz = 'UTC'))

# Plotting ----
# Plot a dual-axis figure for visibility and sky level 1

d_filter = d %>%
  filter(station %in% c("MSY", "PHL", "LAX"
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


# Zoom to one week, one location. Put vis in feet, 5280' per mile
d_filter2 = d_filter %>% 
  mutate(vis_ft = vsby*5280) %>% 
  filter(date >= "2019-04-02" & date <= "2019-04-09" & station == 'MSY')


sky_plot = ggplot(d_filter2) +
  geom_line(aes(x = valid, y = skyl1), color = 'blue') +
  xlab("Date") +
  ylab("Sky Level 1 Altitude (ft) \n Limit 5000'") +
  ylim(0, 5000) +
  theme_bw() +
  ggtitle('MSY METAR sample, 2019-04-02 to 2019-04-09')

vis_plot = ggplot(d_filter2) +
  geom_line(aes(x = valid, y = vis_ft), color = 'red') +
  xlab("Date") +
  ylab("Visibility (ft) \n Limit 10,000'") +
  ylim(0, 10000) +
  theme_bw()

grid.arrange(sky_plot, vis_plot)
# Save to file with arrangeGrob
g <- arrangeGrob(sky_plot, vis_plot)
g
ggsave('METAR_example_plot_MSY.jpeg', g)


# Plot active NOTAMs ----
# First, simple plot of count by station and active date range
ggplot(day_hr_ct,
       aes(x = day_hr,
           y = count_active,
           group = notam_location)) +
  geom_line() +
  facet_wrap(~notam_location)


# Just plot MSY
notam_count_plot <- ggplot(day_hr_ct %>% 
         filter(notam_location == "KMSY" &
                  day_hr >= '2019-04-02' & day_hr <= '2019-04-09'),
       aes(x = day_hr,
           y = count_active,
           group = notam_location)) +
  geom_line(color = 'darkgreen', size = 1) +
  theme_bw() +
  ggtitle('Count of active NOTAMs in MSY') 


grid.arrange(notam_count_plot, sky_plot, vis_plot)
# Save to file with arrangeGrob
g <- arrangeGrob(notam_count_plot, sky_plot, vis_plot)

ggsave('METAR_example_plot_MSY_plus_NOTAMS.jpeg', g)

# What are the notams that vary regularly at MSY every day?

n_MSY = n %>% 
  filter(notam_location == "KMSY" &
           start_time >= '2019-04-02' & end_time <= '2019-04-09')


unique(n_MSY$text)

