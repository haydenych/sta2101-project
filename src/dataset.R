library(dplyr)

get_all <- function(seed=2101) {
  set.seed(seed)
  
  raw <- read.csv("../data/tx_intersection_crash.csv")
  
  crash <- raw[c("tot_crash_count", "sidewalk_lenght_150ft_ft", "lanes_major", "lanes_minor", "median_major", "median_minor", 
                 "on_system", "median_width_ft_major", "median_width_ft_minor", "lane_width_ft_major", "lane_width_ft_minor", 
                 "shoulder_width_ft_major", "shoulder_width_ft_minor", "aadt_lane_major", "aadt_lane_minor", "truck_perc_major", 
                 "truck_perc_minor", "tot_WMT_sqmi", "tot_WMT_pop", "tot_WMT", "speed_lim_mph_major", "speed_lim_mph_minor",
                 "f_local_major", "f_local_minor", "f_collector_major", "f_collector_minor", "f_arterial_major", "f_arterial_minor", 
                 "a_rural", "a_small_urban", "a_urbanized", "signalized_ind", "approaches", "dist_near_school_mi", "dist_near_hops_mi", 
                 "transit_ind", "transit_stops_025mi_count", "pop_den", "employ_den", "cen_tr_income", "cen_tr_age", "county_rain")]
  crash <- na.omit(crash)

  save(crash, file="../data/all.RData")
}


get_all_std <- function(seed=2101) {
  set.seed(seed)
  
  raw <- read.csv("../data/tx_intersection_crash.csv")
  
  crash <- raw[c("tot_crash_count", "sidewalk_lenght_150ft_ft", "lanes_major", "lanes_minor", "median_major", "median_minor", 
                 "on_system", "median_width_ft_major", "median_width_ft_minor", "lane_width_ft_major", "lane_width_ft_minor", 
                 "shoulder_width_ft_major", "shoulder_width_ft_minor", "aadt_lane_major", "aadt_lane_minor", "truck_perc_major", 
                 "truck_perc_minor", "tot_WMT_sqmi", "tot_WMT_pop", "tot_WMT", "speed_lim_mph_major", "speed_lim_mph_minor",
                 "f_local_major", "f_local_minor", "f_collector_major", "f_collector_minor", "f_arterial_major", "f_arterial_minor", 
                 "a_rural", "a_small_urban", "a_urbanized", "signalized_ind", "approaches", "dist_near_school_mi", "dist_near_hops_mi", 
                 "transit_ind", "transit_stops_025mi_count", "pop_den", "employ_den", "cen_tr_income", "cen_tr_age", "county_rain")]
  crash <- na.omit(crash)
  
  crash_std <- scale(subset(crash, select=c(-tot_crash_count)))
  crash_std <- dplyr::bind_cols(subset(crash, select=c(tot_crash_count)), crash_std)
  
  save(crash_std, file="../data/all_std.RData")
}

get_splits_std <- function(seed=2101) {
  set.seed(seed)
  
  raw <- read.csv("../data/tx_intersection_crash.csv")
  
  crash <- raw[c("tot_crash_count", "sidewalk_lenght_150ft_ft", "lanes_major", "lanes_minor", "median_major", "median_minor", 
                 "on_system", "median_width_ft_major", "median_width_ft_minor", "lane_width_ft_major", "lane_width_ft_minor", 
                 "shoulder_width_ft_major", "shoulder_width_ft_minor", "aadt_lane_major", "aadt_lane_minor", "truck_perc_major", 
                 "truck_perc_minor", "tot_WMT_sqmi", "tot_WMT_pop", "tot_WMT", "speed_lim_mph_major", "speed_lim_mph_minor",
                 "f_local_major", "f_local_minor", "f_collector_major", "f_collector_minor", "f_arterial_major", "f_arterial_minor", 
                 "a_rural", "a_small_urban", "a_urbanized", "signalized_ind", "approaches", "dist_near_school_mi", "dist_near_hops_mi", 
                 "transit_ind", "transit_stops_025mi_count", "pop_den", "employ_den", "cen_tr_income", "cen_tr_age", "county_rain")]
  crash <- na.omit(crash)
  
  # Add ID for splitting
  crash$id <- 1:nrow(crash)
  
  # Separate into 60/20/20 splits for train/val/test
  train_val <- crash %>% dplyr::sample_frac(0.80)
  test  <- dplyr::anti_join(crash, train_val, by='id')
  
  train <- train_val %>% dplyr::sample_frac(0.75)
  val  <- dplyr::anti_join(train_val, train, by='id')
  
  # Remove the ID
  train = subset(train, select=c(-id))
  val = subset(val, select=c(-id))
  test = subset(test, select=c(-id))
  
  # Standardize the predictors
  train_std <- scale(subset(train, select=c(-tot_crash_count)))
  train_std <- dplyr::bind_cols(subset(train, select=c(tot_crash_count)), train_std)
  
  val_std <- scale(subset(val, select=c(-tot_crash_count)))
  val_std <- dplyr::bind_cols(subset(val, select=c(tot_crash_count)), val_std)
  
  test_std <- scale(subset(test, select=c(-tot_crash_count)))
  test_std <- dplyr::bind_cols(subset(test, select=c(tot_crash_count)), test_std)
  
  save(train_std, val_std, test_std, file="../data/crash_std.RData")
}

get_cv_std <- function(seed=2101) {
  set.seed(seed)
  
  library(caret)

  raw <- read.csv("../data/tx_intersection_crash.csv")
  
  crash <- raw[c("tot_crash_count", "sidewalk_lenght_150ft_ft", "lanes_major", "lanes_minor", "median_major", "median_minor", 
                 "on_system", "median_width_ft_major", "median_width_ft_minor", "lane_width_ft_major", "lane_width_ft_minor", 
                 "shoulder_width_ft_major", "shoulder_width_ft_minor", "aadt_lane_major", "aadt_lane_minor", "truck_perc_major", 
                 "truck_perc_minor", "tot_WMT_sqmi", "tot_WMT_pop", "tot_WMT", "speed_lim_mph_major", "speed_lim_mph_minor",
                 "f_local_major", "f_local_minor", "f_collector_major", "f_collector_minor", "f_arterial_major", "f_arterial_minor", 
                 "a_rural", "a_small_urban", "a_urbanized", "signalized_ind", "approaches", "dist_near_school_mi", "dist_near_hops_mi", 
                 "transit_ind", "transit_stops_025mi_count", "pop_den", "employ_den", "cen_tr_income", "cen_tr_age", "county_rain")]
  crash <- na.omit(crash)
  crash$id <- 1:nrow(crash)

  train_val <- crash %>% dplyr::sample_frac(0.80)
  test  <- dplyr::anti_join(crash, train_val, by='id')
  
  # Reassign the id for cross validation
  train_val = subset(train_val, select=c(-id))
  train_val$id <- 1:nrow(train_val)
  
  train_val_std <- scale(subset(train_val, select=c(-id, -tot_crash_count)))
  train_val_std <- dplyr::bind_cols(subset(train_val, select=c(id, tot_crash_count)), train_val_std)
  
  test_std <- scale(subset(test, select=c(-id, -tot_crash_count)))
  test_std <- dplyr::bind_cols(subset(test, select=c(tot_crash_count)), test_std)
  
  folds <- createFolds(train_val_std$id, k=5)
  
  save(train_val_std, test_std, folds, file="../data/folds_std.RData")
}

slice_arterial <- function(data) {
  data <- data[data$f_arterial_major > 0 | data$f_arterial_minor > 0, ]
  
  data <- subset(data, select=c(tot_crash_count, sidewalk_lenght_150ft_ft, lanes_major, lanes_minor, median_major, median_minor, 
                                median_width_ft_major, median_width_ft_minor, lane_width_ft_major, lane_width_ft_minor, 
                                shoulder_width_ft_major, shoulder_width_ft_minor, approaches))
  
  return (data)
}

slice_collector <- function(data) {
  data <- data[data$f_collector_major > 0 | data$f_collector_minor > 0, ]
  
  data <- subset(data, select=c(tot_crash_count, sidewalk_lenght_150ft_ft, lanes_major, lanes_minor, median_major, median_minor, 
                                median_width_ft_major, median_width_ft_minor, lane_width_ft_major, lane_width_ft_minor, 
                                shoulder_width_ft_major, shoulder_width_ft_minor, approaches))
  
  return (data)
}

slice_local <- function(data) {
  data <- data[data$f_local_major > 0 | data$f_local_minor > 0, ]
  
  data <- subset(data, select=c(tot_crash_count, sidewalk_lenght_150ft_ft, lanes_major, lanes_minor, median_major, median_minor, 
                                median_width_ft_major, median_width_ft_minor, lane_width_ft_major, lane_width_ft_minor, 
                                shoulder_width_ft_major, shoulder_width_ft_minor, approaches))
  
  return (data)
}