# This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

# Replicate flowline 4x, once for each scenario in `scenario.nums` ----

asrp_reach_data_scenarios <- lapply(scenario.nums, function(j) {
  flowline %>% 
    select(noaaid, GSU, forest, curr_temp, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, Reach, species, can_ang, Subbasin_num) %>%
    mutate(Scenario_num = j) 
}) %>%
  do.call('rbind',.) 

# replicate the replicated flowline 3 more times, once for each year in `scenario.years` ----

asrp_reach_data <- lapply(scenario.years, function(k) {
  asrp_reach_data_scenarios %>% 
    mutate(year = k)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c('dev_and_climate', 'scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only',
                                              'scenario_1_no_climate_chg', 'scenario_2_no_climate_chg', 'scenario_3_no_climate_chg')),
         !(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", "scenario_2_fp_only", 
                               "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", "scenario_3_beaver_only",
                               'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only') & 
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_scenarios %>%
              select(GSU, Scenario_num, managed_forest)) %>%
  mutate(managed_forest = ifelse(is.na(managed_forest), 
                                 'n',
                                 as.character(managed_forest))) %>%
  
  # Assign temperature with and without tree growth, and intensity scalars for temperature, wood, floodplains and beaver based on year ----
  mutate(tm_2019 = curr_temp,
         tm_2019_cc_only = curr_temp,
         asrp_temp_w_growth = case_when(
           year == 2019 ~ tm_2019,
           year == 2040 ~ tm_2040,
           year == 2080 ~ tm_2080),
         asrp_temp_cc_only = case_when(
           year == 2019 ~ tm_2019_cc_only,
           year == 2040 ~ tm_2040_cc_only,
           year == 2080 ~ tm_2080_cc_only),
         temp_intensity_scalar = case_when(
           year == 2019 & Scenario_num == 'Current_asrp' ~ 0,
           year == 2019 & !Scenario_num == 'Current_asrp' ~ 1,
           year == 2040 ~ .75,
           year == 2080 ~ .75),
         wood_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c('scenario_1', 'scenario_2', 'scenario_3', "scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     "scenario_3_beaver_only", 'scenario_1_barrier_only', 'scenario_2_barrier_only', 
                                                     'scenario_3_barrier_only'),
                                 1,
                                 0),
           year == 2040 ~ 1,
           year == 2080 ~ 1),
         fp_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c('scenario_1', 'scenario_2', 'scenario_3', "scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only',
                                                     "scenario_3_beaver_only"),
                                 ifelse(managed_forest == 'y',
                                        1,
                                        1),
                                 0),
           year %in% c(2040, 2080) & managed_forest == 'y'~ 1,
           year %in% c(2040, 2080) & managed_forest == 'n'~ 1),
         beaver_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c('scenario_1', 'scenario_2', 'scenario_3', "scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     "scenario_3_beaver_only",
                                                     'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only'),
                                 ifelse(managed_forest == 'y',
                                        1,
                                        1),
                                 0),
           year %in% c(2040, 2080) ~ ifelse(managed_forest == 'y',
                                            .1,
                                            1))) %>%
  left_join(., asrp_scenarios) %>%
  
  
  mutate(
    
    # Create single restoration percentage field based on whether or not forest == 'y' for each reach ----  
    
    rest_perc = ifelse(is.na(rest_perc),
                         0,
                         rest_perc),
    
    # Fix fields joined from the asrp scenarios data frame with `NA` values ----
    
    #It is assumed that `NA` values for these particular fields come about only in rows where the GSU does not match any of the GSUs that receive 
    #restoration effort under the asrp scenarios 
    
    primary_cr_only = ifelse(is.na(primary_cr_only),
                             "n",
                             as.character(primary_cr_only)),
    LW = case_when(
      is.na(LW) ~ 'n',
      LW == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y'),
      LW == 'n' ~ 'n'),
    Barriers = case_when(
      is.na(Barriers) ~ 'n',
      Barriers == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y'),
      Barriers == 'n' ~ 'n'),
    Riparian = case_when(
      is.na(Riparian) | Riparian == 'n' ~ 'n',
      Riparian == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y')),
    Floodplain = case_when(
      Floodplain == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               ifelse(managed_forest == 'y',
                      ifelse(LW == 'y',
                             'y',
                             'n'),
                      'y')),
      is.na(Floodplain) | Floodplain == 'n' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               ifelse(managed_forest == 'y',
                           ifelse(LW == 'y',
                                  'y',
                                  'n'),
                           ifelse(Riparian == 'y',
                                  'y',
                                  'n')))),
    Beaver = case_when(
      (primary_cr_only == 'y' & !Reach %in% primary_cr) ~ 'n',
      !(primary_cr_only == 'y' & !Reach %in% primary_cr) ~
        ifelse(LW == 'y',
               'y',
               'n'))) %>%
  
  # Calculate wood and temperature multipliers based on the particular asrp scenario and year of each row ----

left_join(., wood_data) %>%
  mutate(woodmult_s_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_s - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         woodmult_w_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_w - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         asrp_temp = ifelse(Riparian == 'y',
                            ifelse(can_ang > 170,
                                   asrp_temp_cc_only - (asrp_temp_cc_only - asrp_temp_w_growth) * temp_intensity_scalar,
                                   asrp_temp_w_growth),
                            ifelse(can_ang > 170,
                                   asrp_temp_cc_only,
                                   asrp_temp_w_growth)),
         asrp_temp = ifelse(Scenario_num %in% c('scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only', 
                                                'scenario_1_no_climate_chg', 'scenario_2_no_climate_chg', 'scenario_3_no_climate_chg'),
                            ifelse(year == 2040,
                                   asrp_temp - 1.8,
                                   asrp_temp - 3),
                            asrp_temp),

# Where floodplain reconnection occurs, we expect a 1° reduction in temperature.  This gets scaled by the restoration percentage.
         asrp_temp = ifelse(Floodplain == 'y', 
                            asrp_temp - (1 * rest_perc),
                            asrp_temp),
         tempmult.asrp = temp_func(asrp_temp)) %>%
  
  # add in future impervious area by GSU, scenario and year ----

left_join(., fut_imperv, by = c('GSU', 'year')) %>%
  mutate(future_imperv = ifelse(is.na(future_imperv),
                                0,
                                future_imperv)) %>%
  mutate(LW = ifelse(Scenario_num %in% c("scenario_1_fp_only", "scenario_2_fp_only", "scenario_3_fp_only", 
                                         'scenario_1_beaver_only', 'scenario_2_beaver_only','scenario_3_beaver_only',
                                         'scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only',
                                         'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only'),
                     'n',
                     as.character(LW)),
         Floodplain = ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", 
                                                 'scenario_1_beaver_only', 'scenario_2_beaver_only','scenario_3_beaver_only',
                                                 'scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only',
                                                 'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only'),
                             'n',
                             as.character(Floodplain)),
         Riparian = ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", 
                                                 'scenario_1_beaver_only', 'scenario_2_beaver_only','scenario_3_beaver_only',
                                                 'scenario_1_fp_only', 'scenario_2_fp_only', 'scenario_3_fp_only',
                                               'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only'),
                             'n',
                             as.character(Floodplain)),
         Barriers = ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", 
                                               'scenario_1_beaver_only', 'scenario_2_beaver_only','scenario_3_beaver_only',
                                               'scenario_1_fp_only', 'scenario_2_fp_only', 'scenario_3_fp_only',
                                               'scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only'),
                           'n',
                           as.character(Barriers)),
         Beaver = ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", 
                                               'scenario_1_barrier_only', 'scenario_2_barrier_only','scenario_3_barrier_only',
                                               'scenario_1_fp_only', 'scenario_2_fp_only', 'scenario_3_fp_only',
                                               'scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only'),
                           'n',
                           as.character(Beaver)))

rm(asrp_reach_data_scenarios)
