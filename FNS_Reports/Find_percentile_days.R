
# Find percentile days ----

day_sums = orig_dt_hr %>%
  ungroup() %>%
  group_by(Region, date) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days for each region

pctile_days = day_sums %>%
  ungroup() %>%
  group_by(Region) %>%
  summarize(quantile = seq(0, 1, 0.1),
            daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))

ninetieth = pctile_days %>% filter(quantile == 0.9)

# Which days are the 90th percentile by service area?

ninetieth_days = vector()

for(r in unique(day_sums$Region)){
  dr = day_sums %>% 
    filter(Region == r) 
  
  pctile_days_r = pctile_days %>% 
    filter(Region == r) 
  
  ninetieth_r = pctile_days_r %>% filter(quantile == 0.9)
  hundredth_r = pctile_days_r %>% filter(quantile == 1)  
  
  dr = dr %>%
    filter(daily_count >=  ninetieth_r$daily_count & 
             daily_count <  hundredth_r$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_r$daily_count) %>%
    filter(proximity_to_ninetieth == min(proximity_to_ninetieth))
  
  class(dr) = 'data.frame'
  
  ninetieth_days = rbind(dr, ninetieth_days)
  
}

ninetieth_days

# Across the whole NAS, by weekend and season ----

day_sum_NAS = orig_dt_hr %>%
  mutate(season_we = paste(season, weekend)) %>%
  ungroup() %>%
  group_by(date, season, weekend, season_we) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days 

pctile_day_NAS = day_sum_NAS %>%
  ungroup() %>%
  mutate(season_we = paste(season, weekend)) %>%
  group_by(season, weekend, season_we) %>%
  summarize(quantile = seq(0, 1, 0.1),
            daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))

ninetieth = pctile_day_NAS %>% filter(quantile == 0.9)

# Which days are the 90th percentile demand day?

ninetieth_r = pctile_day_NAS %>% filter(quantile == 0.9)
hundredth_r = pctile_day_NAS %>% filter(quantile == 1)  

ninetieth_day_NAS  = vector()

for(sw in unique(pctile_day_NAS$season_we)){
  # sw = 'Fall Weekday'
  ninetieth_sw = ninetieth_r %>% filter(season_we == sw)
  hundredth_sw = hundredth_r %>% filter(season_we == sw)
  
  ninetieth_day_NASsw = day_sum_NAS %>%
    filter(season_we == sw) %>%
    filter(daily_count >=  ninetieth_sw$daily_count & 
             daily_count <  hundredth_sw$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_sw$daily_count)
  
  ninetieth_day_NASsw = ninetieth_day_NASsw %>% 
    filter(proximity_to_ninetieth == min(ninetieth_day_NASsw$proximity_to_ninetieth))
  
  class(ninetieth_day_NASsw) = 'data.frame'
  
  ninetieth_day_NAS = rbind(ninetieth_day_NAS, ninetieth_day_NASsw)
}

ninetieth_day_NAS

write.csv(ninetieth_day_NAS, file = 'Ninetieth_day_NAS.csv', row.names = F)
