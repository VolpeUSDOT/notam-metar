# Compare staffing models

# Setup ----

# Get dependencies if not already installed
source('Utility/get_packages.R')


# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists
if(grepl('notam-metar$', getwd())){
  setwd('./FNS_Reports')
}

library(tidyverse)


# Compile results ----

# Could scan the directory as well, here manually entering the 8 completed analyses as of March 2021.

results_dirs <- c('Results_Max',
                  'Results_Mean',
                  'Results_Staggered_Max',
                  'Results_Staggered_Mean',
                  'Results_Max_addRegion',
                  'Results_Mean_addRegion',
                  'Results_Staggered_addRegion_Max',
                  'Results_Staggered_addRegion_Mean')

shift_length = ifelse(grepl('Staggered', results_dirs), 'Staggered', '8hr')
target_delay_type = ifelse(grepl('Max', results_dirs), 'Max', 'Mean')
geography = ifelse(grepl('addRegion', results_dirs), 'addRegion', 'Each')

res <- data.frame(results_dirs, shift_length, target_delay_type, geography)

# For each one, get the total staffing for just WSA, and for across all three. Do this by season and weekend/day type

# Then repeat for 5 min target instead of 2 min target

target_delay = 2

# Check to see if all these directories are present. 
stopifnot(all(results_dirs %in% dir())) # If fails, check to see you have run these analyses (or change the names in the results_dirs vector)

# Now loop through these results directories, read in the staffing models, and compile
compiled <- vector()

for(r in results_dirs){
  # r = results_dirs[8]
  d <- read_csv(file.path(r, 'All_Staff_models.csv'))
  
  # Total hourly staff for each season / weekend/day?
  rx <- d %>%
    filter(target == 2 & !is.na(target) & !is.na(season)) %>%
    group_by(Region, season, weekend) %>%
    summarize(sum_staff = sum(hourly_staff_model),
              max_staff = max(hourly_staff_model),
              NOTAM_count = sum(hourly_count)) %>%
    mutate(results_dirs = r) %>%
    left_join(res)
  
  if(length(compiled) == 0) {
    compiled = rx
  } else {
    compiled = rbind(compiled, rx )
  }
  
}

# Save for future use
write.csv(compiled, 'Cross_Analysis_Compiled_Results_2min.csv', row.names = F)

# Summary table across each region analysis -----
cx <- compiled %>% 
  filter(geography == 'Each') %>%
  group_by(shift_length, target_delay_type) %>%
  summarize(sum_staff = sum(sum_staff),
            max_staff = sum(max_staff))

rx <- res %>% filter(geography == 'Each') %>%
  left_join(cx)

# Compare sum using each to sum using addRegions
cx2 <- compiled %>% 
  filter(geography == 'addRegion', 
         Region == 'Western + Central + Eastern') %>%
  group_by(shift_length, target_delay_type) %>%
  summarize(sum_staff_addRegion = sum(sum_staff))

rx2 = rx %>% left_join(cx2) %>% select(-geography)

# Summary table for report. Copy to Excel then to Word.
knitr::kable(rx2, format = 'pipe')

# What percent increase in staff hours for each region separately versus all together?
rx2 %>%
  mutate(pct_ch = 100 * ( (sum_staff - sum_staff_addRegion) / sum_staff_addRegion))

# Regions comparisons ----

# For just the addRegions analysis, look at how the total staffing hours increase
region_res = res %>% filter(geography == 'addRegion')
region_compiled <- compiled %>% filter(geography == 'addRegion')

ggplot(region_compiled, aes(x = NOTAM_count, y = sum_staff, color = Region)) +
  geom_point() + 
  theme_bw() +
  ggtitle('Sum of staff hours needed \n to meet 2 minute delay targets for 90th percentile days by region') +
  xlab('Count of NOTAMs in a 24 hr period') +
  ylab('Sum of staff hours in a 24 hr period') +
  facet_wrap(~ target_delay_type + shift_length)


ggsave(filename = 'AddRegion_DelayTargetType_ShiftType_each_day_point.jpeg',
       width = 8, height = 7)


ggplot(region_compiled, aes(x = Region, y = sum_staff, fill = target_delay_type)) +
  geom_boxplot() + 
  theme_bw() +
  ggtitle('Sum of staff hours needed \n to meet 2 minute delay targets for 90th percentile days by region') +
  ylab('Sum of staff hours in a 24 hr period') +
  facet_wrap(~ shift_length) +
  theme(axis.text.x=element_text(angle=90)) +
  guides(fill = guide_legend(title = "Target Delay Type"))

ggsave(filename = 'AddRegion_DelayTargetType_ShiftType_boxplot.jpeg',
       width = 8, height = 7)

# What percent greater is the max versus mean delay type?
compiled %>% group_by(target_delay_type, shift_length) %>% summarize(sum(sum_staff))



