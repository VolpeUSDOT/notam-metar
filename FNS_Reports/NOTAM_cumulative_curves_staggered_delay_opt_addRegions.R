# Adapted for FNS reports of NOTAMs, with service area added
# Adding a step to optimize the staffing needed
# This version adds regions in sequence. First just WSA, then WSA + CSA, then WSA + CSA + ESA (all NAS).

# Setup ----

# Set working directory to location where NOTAM_Freq_w_busy_days.RData exists
if(grepl('notam-metar$', getwd())){
  setwd('./FNS_Reports')
}

# Get dependencies if not already installed
source('Utility/get_packages.R')

# Get the functions
source('Busy_periods_functions.R')

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
library(tidyr)


# <<>><<>><<>>
# User inputs - Three required inputs 

# 1. Specify a single integer, or a vector of integers for the delay targets in minutes
max_delay_target = c(2, 5, 15) 

# 2. Should this analysis use the peak (absolute maximum) delay at any time in the 24 hours period, or the average delay?
delay_target_type = 'Max' # Options: 'Max' or 'Mean'

# 3. Specify where to save the results. Any character string can be used here; it will become a directory where the output is save.
output_dir = 'Results_Staggered_addRegion'

output_dir = paste(output_dir, delay_target_type, sep = '_')

if(dir.exists(output_dir)){ 
  if(!askYesNo(msg = 'The selected output directory exists. Do you want to overwrite it?')){
    stop('Please change the output_dir name')
  }
} else {
  dir.create(output_dir)
}

# <<>><<>><<>>

if(!file.exists("FNS_NOTAM_Freq_w_busy_days.RData")){
  cat('Attempting to source the script to generate busy_periods data frame')
  rmarkdown::render('NOTAM_FNS_Analysis.Rmd')
}

load("FNS_NOTAM_Freq_w_busy_days.RData")

# Find the percentile days. This results in data frames of the 90th percentile busiest days, 
# either by service area or across the whole NAS, by season, weekend/weekday type
source('Find_percentile_days.R')

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Run busy day analysis ----

# Data frame with the 90th percentile days

ninetieth_days

ninetieth_day_NAS

# Region sequence
region_sequence = list('Western',
                       c('Western', 'Central'),
                       c('Western', 'Central', 'Eastern'))

# set up blank data frame to save all staff model targets
all_staff_models = vector()

# Loop over season x weekend combinations

for(sw in 1:nrow(ninetieth_day_NAS)){
  # sw = 1
  season = ninetieth_day_NAS[sw, 'season']
  weekend = ninetieth_day_NAS[sw, 'weekend']
  
  # Loop over targets 
  
  for(delay_target_x in max_delay_target){
    # delay_target_x = 15    
    staff_models = vector()
    
    # save to PDF
    fn = paste0('NOTAM_Staffing_', season, '_', weekend, '_', formatC(delay_target_x, width = 2, flag = 0),'min_target.pdf')
    
    pdf(file.path(output_dir, fn), width = 10, height = 8)
    
    for(Region_x in region_sequence){
      
      # Region_x = c('Western', 'Central')

      demand_day_of_interest = ninetieth_day_NAS[ninetieth_day_NAS$season == season &
                                                    ninetieth_day_NAS$weekend == weekend, 'date']
      
      # Count in the region (or group of regions) on the demand day
  
      count_on_demand_day = sum(ninetieth_days[ninetieth_days$Region %in% Region_x, 'daily_count'])
  
      # Run busy day analysis ----
      day = demand_day_of_interest
      print(day)
      
      bod <- day
      eod <- day + 1  
      
      this_day = orig_dt_hr %>% 
        filter(date == bod & Region %in% Region_x) %>%
        group_by(yr_hour, date, dow, hour, weekend, month, season) %>%
        summarize(hourly_count = sum(hourly_count))
      
      region_set_name = ifelse(length(Region_x) > 1,
                               paste(Region_x, collapse = ' + '), Region_x)
      
      gp <- ggplot(data = this_day,
                   aes(x = yr_hour,
                       y = hourly_count)) +
        geom_point() +
        geom_text(aes(label = hourly_count), nudge_y = 4) +
        geom_line() +
        xlab('Hour (UTC)') + ylab('Hourly NOTAM count') +
        ggtitle(paste(region_set_name, bod, ', Busy Period Percentile:', 100* 0.9, '\n', season, weekend)) +
        theme_bw()
      
      print(gp)
      
      # staff model 90 ----    
      # populate the base model  
    
      # Make an guess about high and low staffing based on total NOTAM count, then optimize
      # 1. high med low are set up using a rule of thumb, with floors set
      # 2. The identity of which third of the day is set to high, med, or low is set here
      # 3. Then we use a while loop to get to the target max_delay value
      
      shift_length = 8L
      
      if(delay_target_type == 'Max'){
        high = floor(count_on_demand_day / 150); high = ifelse(high < 2, 2, high)
        med = floor(count_on_demand_day / 250); med = ifelse(med < 1, 1, med)
        low = floor(count_on_demand_day / 300); low = ifelse(low < 1, 1, low)
      }
      
      if(delay_target_type == 'Mean'){
        high = floor(count_on_demand_day / 300); high = ifelse(high < 2, 2, high)
        med = floor(count_on_demand_day / 350); med = ifelse(med < 1, 1, med)
        low = floor(count_on_demand_day / 500); low = ifelse(low < 1, 1, low)
      }
      
      shift_hour_starts <- this_day %>%
        ungroup() %>%
        mutate(day_thirds = cut(as.numeric(this_day$hour), 3)) %>%
        group_by(day_thirds) %>%
        summarize(sum_thirds = sum(hourly_count)) %>%
        ungroup() %>%
        summarize(
                  high = c('00', '08', '16')[which(sum_thirds == max(sum_thirds))],
                  med = c('00', '08', '16')[which(sum_thirds != max(sum_thirds) & sum_thirds != min(sum_thirds))],
                  low = c('00', '08', '16')[which(sum_thirds == min(sum_thirds))])
      
      # Order of shifts
      first_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) == min(as.numeric(shift_hour_starts)))]
      second_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) != min(as.numeric(shift_hour_starts)) & 
                                               as.numeric(shift_hour_starts) != max(as.numeric(shift_hour_starts)))]
      third_shift = shift_hour_starts[which(as.numeric(shift_hour_starts) == max(as.numeric(shift_hour_starts)))]
      
      # Establish the base model. This will get optimized to the max delay
      hourly_staff_model = c(rep(get(names(first_shift)), shift_length),
                             rep(get(names(second_shift)), shift_length),
                             rep(get(names(third_shift)), shift_length))
                             
      
    ### Calculate, plot, and report ----
    
      staff_reqd <- compute_staff_reqd(day = day,
                                       processing_time_in_minutes = 3,
                                       hourly_staff_model = hourly_staff_model,
                                       Region = Region_x)
      
      # Select delay target type from the user input 
      max_delay = ifelse(delay_target_type == 'Max', 
                         round(staff_reqd$max_minutes_delay),
                         ifelse(delay_target_type == 'Mean',
                                round(staff_reqd$average_minutes_delay, 2),
                                stop('Specify delay_target_type of Max or Mean')))
      
                         
      
      while(max_delay > delay_target_x){
        # In which **Hour** does max delay occurs in? Add staff starting the previous hour.
        
        add_to_this_hour = as.numeric(staff_reqd$hour_of_max_delay)
        
        # If not midnight, subtract one hour to start the shift the hour before the max delay occurs
        # Change the term `recenter_the_shift` to any integer between 1 and 7 move the starting time of the staffer back that many hours. Leave as 0 for the staffer to start at exactly the hour needed.
        recenter_the_shift = 0
        
        add_to_this_hour = ifelse(add_to_this_hour > recenter_the_shift,
                                  add_to_this_hour - recenter_the_shift,
                                  add_to_this_hour)
        
        # If the shift would run past midnight, shorten the effective shift length to fit in this day
        shift_length_use = ifelse(add_to_this_hour >= 16,
                                  24-add_to_this_hour,
                                  shift_length)
        # Add staff. Pad with 0 until the hour of max delay, and with 0 until end of day
        
        
        addl_staff = c(rep(0, add_to_this_hour),
                       rep(1, shift_length_use),
                       rep(0, 24-(add_to_this_hour + shift_length_use)))
        
        
        hourly_staff_model = rowSums(data.frame(hourly_staff_model, addl_staff))
        
        staff_reqd <- compute_staff_reqd(day = day,
                                         processing_time_in_minutes = 3,
                                         hourly_staff_model = hourly_staff_model,
                                         Region = Region_x)
        
        # Select delay target type from the user input 
        max_delay = ifelse(delay_target_type == 'Max', 
                           round(staff_reqd$max_minutes_delay),
                           ifelse(delay_target_type == 'Mean',
                                  round(staff_reqd$average_minutes_delay, 2),
                                  stop('Specify delay_target_type of Max or Mean')))
        
      } # end while
      
      plot_staffing_model_focus(day, staff_reqd, Region_x,
                                focus_inset = F)
      
      # Save staffing model for this target
      hourly_staff_df_x = data.frame(hourly_staff_model = hourly_staff_model,
                                      hour = formatC(0:23, width = 2, flag = 0))
      
      if('peak3' %in% names(this_day)){
        staff_model_df_x = this_day %>% select(-hourly_count_rank, -peak2, -peak3)
      } else {
        staff_model_df_x = this_day
      }
      
      staff_model_df_x = left_join(hourly_staff_df_x, staff_model_df_x, by = 'hour')
      
      # Add in region identifier if doing a multiple region set
      if(!'Region' %in% names(staff_model_df_x)){
        staff_model_df_x$Region = region_set_name
      }
      
      staff_models = rbind(staff_models, as.data.frame(staff_model_df_x))
      
      
    } # end loop over regions
    
    write.csv(staff_models,
              file = file.path(output_dir, 
                               paste0('Staff_models_90th_', season, '_', weekend, '_', delay_target_x, 'min_target.csv')),
              row.names = F)
    
    dev.off()#; system(paste('open', file.path(output_dir, fn)))
    
    # Save with targets
    all_staff_models = rbind(all_staff_models, data.frame(staff_models, 
                                                          target = delay_target_x))
  } # end loop over targets
  
} # end loop over season x weekend combinations

write.csv(all_staff_models,
          file = file.path(output_dir, 
                           paste0('All_Staff_models.csv')),
          row.names = F)



# Summarize all staff models ----

# Maximum staff needed for the 2 min delay target

all_staff_models %>%
  filter(target == 2 & !is.na(target) & !is.na(season)) %>%
  group_by(Region, season, weekend) %>%
  summarize(max_staff = max(hourly_staff_model)) %>%
  pivot_wider(id_cols = c(season, weekend),
              names_from = Region,
              values_from = c(max_staff)) %>%
  group_by(season, weekend) %>%
  rowwise() %>%
  mutate(Total = sum(across()))


# Total staff hours

# all_staff_models = read.csv(file.path('Results_Staggered_0', 'All_Staff_models.csv'))

all_staff_models %>%
  filter(target == 2 & !is.na(target) & !is.na(season)) %>%
  group_by(Region, season, weekend) %>%
  summarize(sum_staff = sum(hourly_staff_model),
            max_staff = max(hourly_staff_model)) %>%
  pivot_wider(id_cols = c(season, weekend),
              names_from = Region,
              values_from = c(sum_staff)) %>%
  group_by(season, weekend) %>%
  rowwise() %>%
  mutate(Total = sum(across()))
