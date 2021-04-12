
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

# Across the whole NAS, by season ----

day_sum_NAS = orig_dt_hr %>%
  ungroup() %>%
  group_by(date, season) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days 

pctile_day_NAS = day_sum_NAS %>%
  ungroup() %>%
  group_by(season) %>%
  summarize(quantile = seq(0, 1, 0.1),
            daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))

ninetieth = pctile_day_NAS %>% filter(quantile == 0.9)

# Which days are the 90th percentile demand day?

ninetieth_r = pctile_day_NAS %>% filter(quantile == 0.9)
hundredth_r = pctile_day_NAS %>% filter(quantile == 1)  

ninetieth_day_NAS  = vector()

for(s in unique(pctile_day_NAS$season)){
  ninetieth_s = ninetieth_r %>% filter(season == s)
  hundredth_s = hundredth_r %>% filter(season == s)
  
  ninetieth_day_NASs = day_sum_NAS %>%
    filter(season == s) %>%
    filter(daily_count >=  ninetieth_s$daily_count & 
             daily_count <  hundredth_s$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_s$daily_count)
  
  ninetieth_day_NASs = ninetieth_day_NASs %>% 
    filter(proximity_to_ninetieth == min(ninetieth_day_NASs$proximity_to_ninetieth))
  
  class(ninetieth_day_NASs) = 'data.frame'
  
  ninetieth_day_NAS = rbind(ninetieth_day_NAS, ninetieth_day_NASs)
}

ninetieth_day_NAS

write.csv(ninetieth_day_NAS, file = 'Ninetieth_day_NAS.csv', row.names = F)
