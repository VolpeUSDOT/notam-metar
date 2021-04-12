
# Find percentile days for each region ----

day_sums = orig_dt_hr %>%
  ungroup() %>%
  group_by(Region, date) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days for each set of regions

regions = c("Western", "Central", "Eastern")
service_acronyms = paste0(substr(regions, 1, 1), "SA")
# this is an awkward line of code because of a design flaw in paste() which
# throws an error on a string vector of length 1
concat_service_acronyms <- function(r) {
  if (r == 1) service_acronyms[1] else paste(service_acronyms[1:r],
                                             collapse = ' + ')
}
pctile_days = tibble()
for (r in 1:length(regions)) {
  sa_field <- concat_service_acronyms(r)
  pctile_days_section = day_sums %>%
    filter(Region %in% regions[1:r]) %>%
    # mutate(service_areas = sa_field) %>%
    ungroup() %>%
    # select('service_areas', 'date', 'daily_count') %>%
    group_by(date) %>%
    summarize(daily_count = sum(daily_count)) %>%
    summarize(service_areas = sa_field,
              quantile = seq(0, 1, 0.1),
              daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))
  pctile_days <- rbind(pctile_days, pctile_days_section)
}

ninetieth = pctile_days %>% filter(quantile == 0.9)

# Which days are the 90th percentile by service area?

ninetieth_days = tibble()

for(r in 1:length(regions)){
  sa_field <- concat_service_acronyms(r)
  dr = day_sums %>% 
    filter(Region %in% regions[1:r]) %>%
    group_by(date) %>%
    summarize(daily_count = sum(daily_count))
  
  pctile_days_r = pctile_days %>% 
    filter(service_areas == sa_field) 
  
  ninetieth_r = pctile_days_r %>% filter(quantile == 0.9)
  hundredth_r = pctile_days_r %>% filter(quantile == 1)  
  
  dr = dr %>%
    filter(daily_count >=  ninetieth_r$daily_count & 
             daily_count <  hundredth_r$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_r$daily_count) %>%
    mutate(service_areas = sa_field, .before = 1) %>%
    filter(proximity_to_ninetieth == min(proximity_to_ninetieth)) %>%
    filter(date == min(date)) # arbitrary tiebreaker
  
  # class(dr) = 'data.frame'
  
  ninetieth_days = rbind(ninetieth_days, dr)
  
}

ninetieth_days

# Across the whole NAS, by season ----

day_sum_NAS = orig_dt_hr %>%
  ungroup() %>%
  group_by(season, date) %>%
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

ninetieth_day_NAS  = tibble()

for(s in unique(pctile_day_NAS$season)){
  ninetieth_s = ninetieth_r %>% filter(season == s)
  hundredth_s = hundredth_r %>% filter(season == s)
  
  ninetieth_day_season = day_sum_NAS %>%
    filter(season == s) %>%
    filter(daily_count >=  ninetieth_s$daily_count & 
             daily_count <  hundredth_s$daily_count) %>%
    mutate(proximity_to_ninetieth = daily_count - ninetieth_s$daily_count)
  
  ninetieth_day_season = ninetieth_day_season %>% 
    filter(proximity_to_ninetieth == min(ninetieth_day_season$proximity_to_ninetieth)) %>%
    filter(date == min(date)) # arbitrary tiebreaker
  
  # class(ninetieth_day_NAS) = 'data.frame'
  
  ninetieth_day_NAS = rbind(ninetieth_day_NAS, ninetieth_day_season)
}

ninetieth_day_NAS

write.csv(ninetieth_day_NAS, file = 'Ninetieth_day_by_season.csv', row.names = F)
write.csv(ninetieth_days, file = 'Ninetieth_day_by_service_area.csv', row.names = F)
