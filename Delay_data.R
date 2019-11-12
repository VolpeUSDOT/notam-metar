# Flight delay connection

# A note about the flight data. In the data set, every “row” is one flight. The flight has several attributes such as origin airport, destination airport, etc etc. To compute the “delay” we subtract “wheels_off_actual” from “wheels_off_scheduled” and divide by 60 to get the delay in minutes . This is called “Wheels Off Delay” and is the metric we can focus on when trying to ascertain correlation with other factors like METAR , NOTAM and TMI data. 

project_dir = '~/Notam_Local' 

setwd(project_dir)

library(tidyverse)
library(jsonlite)

# d <- read_json("flightmeasures5c.json", simplifyVector = T)
# d <- stream_in(file("flightmeasures5c.json"), verbose=FALSE, pagesize=10000)

# 57 columns, 275,655 rows.
d <- ndjson::stream_in("flightmeasures5c.json.gz")

names(d) <- gsub("_source.", "", names(d))

d <- d %>%
  mutate(delay = ( wheelsoff_time_actual - wheelsoff_time_scheduled ) / 60) %>%
  filter(!is.na(delay))

# 58 columns, 83, 218 rows

save(list = c('d'), file = 'flightmeasures5c.RData')
