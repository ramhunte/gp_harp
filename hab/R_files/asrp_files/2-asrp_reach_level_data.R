# This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

# Replicate flowline 4x, once for each scenario in `scenario.nums` ----

asrp_reach_data_scenarios <- lapply(scenario.nums, function(j) {
  flowline %>% 
    select(noaaid, GSU, forest, curr_temp, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, Reach, species, can_ang, Subbasin_num, prespawn_temp, 
           temp_diff_2040, temp_diff_2040_cc_only, temp_diff_2080, temp_diff_2080_cc_only) %>%
    mutate(Scenario_num = j) 
}) %>%
  do.call('rbind',.) 

# replicate the replicated flowline 3 more times, once for each year in `scenario.years` ----

asrp_reach_data <- lapply(scenario.years, function(k) {
  asrp_reach_data_scenarios %>% 
    mutate(year = k)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios)),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_test_scenarios) & 
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_scenarios %>%
              select(GSU, Scenario_num, managed_forest)) %>%
  mutate(managed_forest = ifelse(is.na(managed_forest), 
                                 'n',
                                 as.character(managed_forest))) %>%
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
         year == 2019 ~ 0,
         year %in% c(2040, 2080) ~ 
           ifelse(managed_forest == 'y',
                  0,
                  .75)),
       
       # assign wood intensity scalar.  This is set to 1 for all restoration scenarios
       wood_intensity_scalar = case_when(
         Scenario_num %in% diag_test_scenarios ~ 1,
         !Scenario_num %in% diag_test_scenarios ~
           case_when(
             year == 2019 ~ ifelse(Scenario_num %in% single_action_scenarios,
                                   1,
                                   0),
             year %in% c(2040, 2080) ~ 1)),
       
       # assign floodplain intensity scalar.  This is set to 1 for all restoration scenarios
       fp_intensity_scalar = case_when(
         Scenario_num %in% diag_test_scenarios ~ 1,
         !Scenario_num %in% diag_test_scenarios ~
           case_when(
             year == 2019 ~ ifelse(Scenario_num %in% single_action_scenarios,
                                   1,
                                   0),
             year %in% c(2040, 2080) ~ 1)),
       beaver_intensity_scalar = case_when(
         Scenario_num %in% diag_test_scenarios ~ 1,
         !Scenario_num %in% diag_test_scenarios ~
           case_when(
             year == 2019 ~ ifelse(Scenario_num %in% single_action_scenarios,
                                   ifelse(managed_forest == 'y',
                                          .1,
                                          1),
                                   0),
             year %in% c(2040, 2080) ~ ifelse(managed_forest == 'y',
                                              .1,
                                              1)))) %>%
  left_join(., asrp_scenarios %>%
              select(-managed_forest)) %>%
  mutate(
    rest_perc = case_when(
      Scenario_num %in% diag_test_scenarios ~ 1,
      !Scenario_num %in% diag_test_scenarios ~
        ifelse(is.na(rest_perc),
               0,
               rest_perc)),
    primary_cr_only = ifelse(is.na(primary_cr_only),
                             "n",
                             as.character(primary_cr_only)),
    LW = case_when(
      Scenario_num %in% c('wood_test', 'lw_flp_test') ~ 'y',
      !Scenario_num %in% c('wood_test', 'lw_flp_test') ~
        case_when(
          is.na(LW) | LW == 'n' ~ 'n',
          LW == 'y' ~
            ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
                   'n',
                   'y'))),
    Barriers = case_when(
      Scenario_num == 'barrier_test' ~ 'y',
      !Scenario_num == 'barrier_test' ~ 
        case_when(
          is.na(Barriers) | Barriers == 'n' ~ 'n',
          Barriers == 'y' ~ 'y')),
    Riparian = case_when(
      is.na(Riparian) | Riparian == 'n' ~ 'n',
      Riparian == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y')),
    Floodplain = case_when(
      Scenario_num %in% c('fp_test', 'lw_flp_test') ~ 'y',
      !Scenario_num %in% c('fp_test', 'lw_flp_test') ~
        case_when(
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
                                 'n'))))),
    Beaver = case_when(
      Scenario_num == 'beaver_test' ~ 'y',
      !Scenario_num == 'beaver_test' ~
        case_when(
          (primary_cr_only == 'y' & !Reach %in% primary_cr) ~ 'n',
          !(primary_cr_only == 'y' & !Reach %in% primary_cr) ~
            ifelse(LW == 'y',
                   'y',
                   'n')))) %>%
  mutate(LW = ifelse(str_detect(Scenario_num, 'beaver|riparian|barrier|fp'),
                     'n',
                     as.character(LW)),
         Floodplain = ifelse(str_detect(Scenario_num, 'wood|beaver|riparian|barrier'),
                             'n',
                             as.character(Floodplain)),
         Riparian = ifelse(str_detect(Scenario_num, 'wood|beaver|fp|barrier|flp'),
                           'n',
                           as.character(Riparian)),
         Barriers = ifelse(str_detect(Scenario_num, 'wood|beaver|fp|riparian|flp'),
                           'n',
                           as.character(Barriers)),
         Beaver = ifelse(str_detect(Scenario_num, 'wood|fp|barrier|riparian|flp'),
                         'n',
                         as.character(Beaver))) %>%
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
         asrp_temp = case_when(
           Scenario_num %in% diag_test_scenarios ~ asrp_temp,
           !Scenario_num %in% diag_test_scenarios ~
             ifelse(Floodplain == 'y', # Where floodplain reconnection occurs, we expect a 1° reduction in temperature.  This gets scaled by the restoration percentage.
                    asrp_temp - (1 * rest_perc),
                    asrp_temp)),
         asrp_temp = ifelse(Scenario_num %in% growth_scenarios,
                            ifelse(year == 2040,
                                   asrp_temp - 1.8,
                                   asrp_temp - 3),
                            asrp_temp),
         tempmult.asrp = temp_func(asrp_temp),
         prespawn_temp_asrp = case_when(
           year == 2019 ~ prespawn_temp,
           year == 2040 ~ ifelse(!Riparian == 'y' & can_ang > 170,
                                 prespawn_temp + temp_diff_2040_cc_only * prespawn_temp_slope - prespawn_temp_intercept,
                                 prespawn_temp + temp_diff_2040 * prespawn_temp_slope - prespawn_temp_intercept), 
           year == 2080 ~ ifelse(!Riparian == 'y' & can_ang > 170,
                                 prespawn_temp + temp_diff_2080_cc_only * prespawn_temp_slope - prespawn_temp_intercept,
                                 prespawn_temp + temp_diff_2080 * prespawn_temp_slope - prespawn_temp_intercept)),
         prespawn_temp_asrp = case_when(
           Scenario_num %in% diag_test_scenarios ~ prespawn_temp_asrp,
           !Scenario_num %in% diag_test_scenarios ~ ifelse(Floodplain == 'y',
                                                           prespawn_temp_asrp - (1 * rest_perc * prespawn_temp_slope - prespawn_temp_intercept),
                                                           prespawn_temp_asrp)),
         prespawn_temp_asrp = ifelse(Scenario_num %in% growth_scenarios,
                                     ifelse(year == 2040,
                                            prespawn_temp_asrp - (1.8 * prespawn_temp_slope - prespawn_temp_intercept),
                                            prespawn_temp_asrp - (3 * prespawn_temp_slope - prespawn_temp_intercept)),
                                     prespawn_temp_asrp)) %>%
  
  # add in future impervious area by GSU, scenario and year ----

left_join(., fut_imperv, by = c('GSU', 'year')) %>%
  mutate(future_imperv = ifelse(is.na(future_imperv),
                                0,
                                future_imperv))
rm(asrp_reach_data_scenarios)
