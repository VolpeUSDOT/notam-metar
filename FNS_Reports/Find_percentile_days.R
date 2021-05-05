
# Find percentile days for each region ----

day_sums = orig_dt_hr %>%
  ungroup() %>%
  group_by(Region, date) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days for each set of regions

regions = c("Western", "Central", "Eastern")
# this is an awkward line of code because of a design flaw in paste() which
# throws an error on a string vector of length 1
concat_service_acronyms <- function(regions, r) {
  service_acronyms = paste0(substr(regions, 1, 1), "SA")
  if (r == 1) service_acronyms[1] else paste(service_acronyms[1:r],
                                             collapse = ' + ')
}

pctile_days = tibble()
for (r in 1:length(regions)) {
  sa_field <- concat_service_acronyms(regions, r)
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
  sa_field <- concat_service_acronyms(regions, r)
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

# results grouped by season ----

day_sum_NAS = orig_dt_hr %>%
  ungroup() %>%
  group_by(Region, date, season) %>%
  summarize(daily_count = sum(hourly_count))


# Now find the quantile days

ninetieth_day_seasons <- tibble()

for (r in 1:length(regions)) {
  sa_field <- concat_service_acronyms(regions, r)
  day_info <- day_sum_NAS %>%
    ungroup() %>%
    filter(Region %in% regions[1:r]) %>%
    group_by(date, season) %>%
    summarize(daily_count = sum(daily_count))
  
  pctile_day_NAS <- day_info %>%
    group_by(season) %>%
    summarize(service_areas = sa_field,
              quantile = seq(0, 1, 0.1),
              daily_count = quantile(daily_count, probs = seq(0, 1, 0.1)))
  
  ninetieth <- pctile_day_NAS %>% filter(quantile == 0.9)
  
  # Which days are the 90th percentile demand day?
  
  ninetieth_r <- pctile_day_NAS %>% filter(quantile == 0.9)
  hundredth_r <- pctile_day_NAS %>% filter(quantile == 1)
  
  for(s in unique(pctile_day_NAS$season)){
    ninetieth_s <- ninetieth_r %>% filter(season == s)
    hundredth_s <- hundredth_r %>% filter(season == s)
    
    ninetieth_day_s <- day_info %>%
      filter(season == s &
               daily_count >=  ninetieth_s$daily_count & 
               daily_count <  hundredth_s$daily_count) %>%
      mutate(proximity_to_ninetieth = daily_count - ninetieth_s$daily_count,
             service_areas = sa_field)
    
    ninetieth_day_s <- ninetieth_day_s %>% 
      filter(proximity_to_ninetieth ==
               min(ninetieth_day_s$proximity_to_ninetieth)) %>%
      ungroup() %>%
      select(service_areas, season, date, daily_count, 
             proximity_to_ninetieth) %>%
      filter(date == min(date)) # arbitrary tiebreaker
    
    ninetieth_day_seasons <- rbind(ninetieth_day_seasons, ninetieth_day_s)
  }
}

ninetieth_day_seasons

write.csv(ninetieth_day_seasons, file = file.path(output_dir,
                                                  'Ninetieth_day_by_season.csv'),
          row.names = F)


write.csv(ninetieth_days, file = file.path(output_dir,
                                           'Ninetieth_day_by_service_area.csv'),
          row.names = F)
