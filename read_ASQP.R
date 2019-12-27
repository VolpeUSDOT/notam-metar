
nameconv <- function(oldnames){
  # gsub: Find single uppercase letters followed by lowercase letters, replace with the same thing preceded by an underscore 
  nn <- gsub('(.)([A-Z]{1}[a-z])', '\\1_\\2', oldnames)
  # convert to uppercase
  nn <- toupper(nn)
  # eliminate duplicate underscores
  nn <- sub('__', '_', nn)
  # Fix Day of week and day of month
  nn <- gsub('DAYOF', 'DAY_OF', nn)
  nn <- gsub('OFWEEK', 'OF_WEEK', nn)
  # Change Reporting airline to carrier, and DOT ID to Airline ID 
  nn[nn == 'REPORTING_AIRLINE'] = 'CARRIER'
  nn[nn == 'DOT_ID_REPORTING__AIRLINE'] = 'AIRLINE_ID'
  nn[nn == 'FLIGHT_NUMBER__REPORTING__AIRLINE'] = 'FLIGHT_NUM'
  
  nn
}

# Fix comma issue by skipping header row, then add state to follow in ORIGIN_CITY_NAME and DEST_CITY_NAME

read_ASQP <- function(datafile){
  
  data_file <- readr::read_csv(datafile,
                               col_names = F, skip = 1)
  
  head_file <- readr::read_csv(datafile,
                               col_names = F, n_max = 1)
  
  head_file <- as.character(head_file[1,])
  
  head_file <- nameconv(head_file)
  
  if(length(head_file) < ncol(data_file)){
    # paste(head_file, collapse = "', '")
    
    # Manually add ORIGIN_STATE_NAME_1 and DEST_STATE_NAME_1
    names(data_file) = c('ID', 'SRC_FILE', 'YEAR', 'QUARTER', 'MONTH', 'DAY_OF_MONTH', 'DAY_OF_WEEK', 'FLIGHT_DATE', 'UNIQUE_CARRIER', 'AIRLINE_ID', 'CARRIER', 'TAIL_NUM', 'FLIGHT_NUM', 'ORIGIN', 'ORIGIN_CITY_NAME', 
                         'ORIGIN_STATE_NAME_1',
                         'ORIGIN_STATE', 'ORIGIN_STATE_FIPS', 'ORIGIN_STATE_NAME', 'ORIGIN_WAC', 'DEST', 'DEST_CITY_NAME',
                         'DEST_STATE_NAME_1',
                         'DEST_STATE', 'DEST_STATE_FIPS', 'DEST_STATE_NAME', 'DEST_WAC', 'CRS_DEP_TIME_HR', 'CRS_DEP_TIME_MIN', 'DEP_TIME_HR', 'DEP_TIME_MIN', 'DEP_DELAY', 'DEP_DELAY_MINS', 'DEP_DELAY_15', 'DEP_DELAY_GRPS', 'DEP_TIME_BLK', 'TAXI_OUT', 'WHEELS_OFF', 'WHEELS_ON', 'TAXI_IN', 'CRS_ARR_TIME_HR', 'CRS_ARR_TIME_MIN', 'ARR_TIME_HR', 'ARR_TIME_MIN', 'ARR_DELAY', 'ARR_DELAY_MINS', 'ARR_DELAY_15', 'ARR_DELAY_GRPS', 'ARR_TIME_BLK', 'CANCELLED', 'CANCELLATION_CODE', 'DIVERTED', 'CRS_ELAPSED_TIME', 'ACTUAL_ELAPSED_TIME', 'AIR_TIME', 'FLIGHTS', 'DISTANCE', 'DISTANCE_GRP', 'CARRIER_DELAY', 'WEATHER_DELAY', 'NAS_DELAY', 'SECURITY_DELAY', 'LATE_AIRCRAFT_DELAY')
  } else {
    names(data_file) = head_file
  }
  # Need time zones to correctly format departure and arrival times. 
  # Will then need to correct arrival date for overnight flights. Will have to check for arrival time < departure time and increment the date by one day.  
  
  # Columns to drop
  to_drop = c('ORIGIN_STATE_NAME_1', 'DEST_STATE_NAME_1',
              'ORIGIN_WAC', 'ORIGIN_STATE', 'ORIGIN_STATE_FIPS',
              'TAIL_NUM', 'UNIQUE_CARRIER', 'SRC_FILE', 'ID', 'FLIGHTS',
              'QUARTER', 'DAY_OF_MONTH',
              'ORIGIN_STATE_NAME', 'DEST_STATE_NAME',
              'DEST_WAC', 'DEST_STATE', 'DEST_STATE_FIPS')
  
  # Drop columns and format
  data_file = data_file[!names(data_file) %in% to_drop] %>%
    mutate(DAY_OF_WEEK = as.factor(DAY_OF_WEEK),
           FLIGHT_DATE = as.Date(FLIGHT_DATE),
           AIRLINE_ID = as.factor(AIRLINE_ID),
           CARRIER = as.factor(CARRIER),
           FLIGHT_NUM = as.factor(FLIGHT_NUM),
           ACTUAL_ELAPSED_TIME = as.numeric(ACTUAL_ELAPSED_TIME),
           AIR_TIME = as.numeric(AIR_TIME),
           ORIGIN = as.factor(ORIGIN),
           ORIGIN_CITY_NAME = as.factor(ORIGIN_CITY_NAME),
           DEST = as.factor(DEST),
           DEST_CITY_NAME = as.factor(DEST_CITY_NAME),
           CANCELLATION_CODE = as.factor(CANCELLATION_CODE)
           # Dep_time = as.POSIXct(paste(FLIGHT_DATE, CRS_DEP_TIME_HR, CRS_DEP_TIME_MIN), 
           #                     format = "%Y-%m-%d %H %M", tz = ...),
           # Arr_time = as.POSIXct(paste(FLIGHT_DATE, CRS_ARR_TIME_HR, CRS_ARR_TIME_MIN), 
           #                     format = "%Y-%m-%d %H %M", tz = ....)
    )
} # end read_ASQP function
