# This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

# Replicate flowline 4x, once for each scenario in `scenario.nums` ----

asrp_reach_data_scenarios <- lapply(scenario.nums, function(j) {
  flowline %>% 
    select(noaaid, GSU, forest, curr_temp, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, Reach, species, can_ang, Subbasin_num, prespawn_temp,
           temp_diff_prespawn,  temp_diff_2040_prespawn, temp_diff_2080_prespawn, hist_temp, temp_diff_rear, Habitat, BF_width, left_ht, right_ht, 
           prespawn_temp_mid, prespawn_temp_late) %>%
    mutate(Scenario_num = j) 
}) %>%
  do.call('rbind',.) 

# replicate the replicated flowline 3 more times, once for each year in `scenario.years` ----

asrp_reach_data <- lapply(scenario.years, function(k) {
  asrp_reach_data_scenarios %>% 
    mutate(year = k)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios, 'dev_and_climate', 'cc_only', 
                                              'rip_and_climate', 'fp_temp', 'rip_and_flp')),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_scenarios) & 
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_scenarios %>%
              select(GSU, Scenario_num, managed_forest)) %>%
  mutate(managed_forest = ifelse(is.na(managed_forest), 
                                 'n',
                                 as.character(managed_forest))) %>%
mutate(asrp_temp_w_growth = case_when(
         year == 2019 ~ ifelse(Scenario_num %in% c('Shade', 'Historical'),
                               hist_temp,
                               curr_temp),
         year == 2040 ~ tm_2040,
         year == 2080 ~ tm_2080),
       asrp_temp_cc_only = case_when(
         year == 2019 ~ ifelse(Scenario_num %in% c('Shade', 'Historical'),
                               hist_temp,
                               curr_temp),
         year == 2040 ~ tm_2040_cc_only,
         year == 2080 ~ tm_2080_cc_only),
       temp_intensity_scalar = case_when(
         Scenario_num %in% c('Shade', 'Historical', 'rip_and_climate', 'rip_and_flp') ~ 1,
         !Scenario_num %in% c('Shade', 'Historical', 'rip_and_climate', 'rip_and_flp') ~
           case_when(
             year == 2019 ~ 0,
             year %in% c(2040, 2080) ~ 
               ifelse(managed_forest == 'y',
                      0,
                      .75))),
       
       # assign wood intensity scalar.  This is set to 1 for all restoration scenarios
       wood_intensity_scalar = case_when(
         Scenario_num %in% diag_scenarios ~ 1,
         !Scenario_num %in% diag_scenarios ~
           case_when(
             year == 2019 ~ ifelse(Scenario_num %in% single_action_scenarios,
                                   1,
                                   0),
             year %in% c(2040, 2080) ~ 1)),
       
       # assign floodplain intensity scalar.  This is set to 1 for all restoration scenarios
       fp_intensity_scalar = case_when(
         Scenario_num %in% diag_scenarios ~ 1,
         !Scenario_num %in% diag_scenarios ~
           case_when(
             year == 2019 ~ ifelse(Scenario_num %in% single_action_scenarios,
                                   1,
                                   0),
             year %in% c(2040, 2080) ~ 1)),
       beaver_intensity_scalar = case_when(
         Scenario_num %in% diag_scenarios ~ 1,
         !Scenario_num %in% diag_scenarios ~
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
      Scenario_num %in% c(diag_scenarios, 'fp_temp', 'rip_and_climate', 'rip_and_flp') ~ 1,
      !Scenario_num %in% c(diag_scenarios, 'fp_temp', 'rip_and_climate', 'rip_and_flp') ~
        ifelse(is.na(rest_perc),
               0,
               rest_perc)),
    primary_cr_only = ifelse(is.na(primary_cr_only),
                             "n",
                             as.character(primary_cr_only)),
    LW = case_when(
      Scenario_num %in% c('Wood', 'FP_wood_comb', 'Historical') ~ 'y',
      !Scenario_num %in% c('Wood', 'FP_wood_comb', 'Historical') ~
        case_when(
          is.na(LW) | LW == 'n' ~ 'n',
          LW == 'y' ~
            ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
                   'n',
                   'y'))),
    Barriers = case_when(
      Scenario_num %in% c('Barriers', 'Historical') ~ 'y',
      !Scenario_num %in% c('Barriers', 'Historical') ~ 
        case_when(
          is.na(Barriers) | Barriers == 'n' ~ 'n',
          Barriers == 'y' ~ 'y')),
    Riparian = case_when(
      Scenario_num %in% c('Shade', 'Historical', 'rip_and_climate', 'rip_and_flp') ~ 'y',
      !Scenario_num %in% c('Shade', 'Historical', 'rip_and_climate', 'rip_and_flp') ~
        case_when(
          is.na(Riparian) | Riparian == 'n' ~ 'n',
          Riparian == 'y' ~
            ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
                   'n',
                   'y'))),
    Floodplain = case_when(
      Scenario_num %in% c('Floodplain', 'FP_wood_comb', 'Historical') ~ 'y',
      !Scenario_num %in% c('Floodplain', 'FP_wood_comb', 'Historical') ~
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
      Scenario_num %in% c('Beaver', 'Historical') ~ 'y',
      !Scenario_num %in% c('Beaver', 'Historical') ~
        case_when(
          (primary_cr_only == 'y' & !Reach %in% primary_cr) ~ 'n',
          !(primary_cr_only == 'y' & !Reach %in% primary_cr) ~
            ifelse(LW == 'y',
                   'y',
                   'n')))) %>%
  mutate(LW = ifelse(str_detect(Scenario_num, regex('beaver|riparian|barrier|fp_only|lr|fine|shade|cc|rip|fp_temp', ignore_case = TRUE)),
                     'n',
                     as.character(LW)),
         Floodplain = ifelse(str_detect(Scenario_num, regex('wood_only|beaver|riparian|barrier|lr|fine|shade|cc|rip|fp_temp', ignore_case = TRUE)),
                             'n',
                             as.character(Floodplain)),
         Riparian = ifelse(str_detect(Scenario_num, regex('wood|beaver|fp|barrier|lr|fine|cc|fp_temp', ignore_case = TRUE)),
                           'n',
                           as.character(Riparian)),
         Barriers = ifelse(str_detect(Scenario_num, regex('wood|beaver|fp|riparian|lr|fine|shade|cc|rip|fp_temp', ignore_case = TRUE)),
                           'n',
                           as.character(Barriers)),
         Beaver = ifelse(str_detect(Scenario_num, regex('wood|fp|barrier|riparian|lr|fine|shade|cc|rip|fp_temp', ignore_case = TRUE)),
                         'n',
                         as.character(Beaver))) %>%
  left_join(., wood_data) %>%
  mutate(woodmult_s_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_s - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         woodmult_w_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_w - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         fp_temp_reduction = case_when(
           Habitat == 'LgRiver' & BF_width >= 30 ~ 1.43,
           Habitat == 'LgRiver' & BF_width < 30 ~ 1,
           Habitat == 'SmStream' & BF_width >= 10 & noaaid %in% ss_fp_reconnect ~ .72,
           Habitat == 'SmStream' & BF_width < 10 & noaaid %in% ss_fp_reconnect ~.29,
           TRUE ~ 0),
         asrp_temp = case_when(Scenario_num %in% c('fp_temp', 'cc_only') & (left_ht <= 15 | right_ht <= 15) ~ asrp_temp_cc_only,
                               Scenario_num %in% c('fp_temp', 'cc_only') & (left_ht > 15 & right_ht > 15) ~ asrp_temp_w_growth,
                               !Scenario_num %in% c('fp_temp', 'cc_only') ~
                                 ifelse(Riparian == 'y',
                                        ifelse(can_ang > 170,
                                               asrp_temp_cc_only - (asrp_temp_cc_only - asrp_temp_w_growth) * temp_intensity_scalar,
                                               asrp_temp_w_growth),
                                        ifelse(can_ang > 170,
                                               asrp_temp_cc_only,
                                               asrp_temp_w_growth))),
         asrp_temp = case_when(
           Floodplain == 'y' & species %in% c('spring_chinook', 'fall_chinook') ~ asrp_temp - mwmt_to_mdm_func(fp_temp_reduction) * rest_perc,
           Floodplain == 'y' & !species %in% c('spring_chinook', 'fall_chinook') ~ asrp_temp - (fp_temp_reduction * rest_perc),
           Floodplain != 'y' & Scenario_num %in% c('fp_temp', 'rip_and_flp') & species %in% c('spring_chinook', 'fall_chinook') ~
             asrp_temp - mwmt_to_mdm_func(fp_temp_reduction) * rest_perc,
           Floodplain != 'y' & Scenario_num %in% c('fp_temp', 'rip_and_flp') & !species %in% c('spring_chinook', 'fall_chinook') ~
             asrp_temp - (fp_temp_reduction * rest_perc),
           TRUE ~ asrp_temp),
         # asrp_temp = ifelse(Scenario_num %in% growth_scenarios,
         #                    ifelse(year == 2040,
         #                           asrp_temp - cc_mid_rear,
         #                           asrp_temp - cc_late_rear),
         #                    asrp_temp),
         tempmult.asrp = temp_func(asrp_temp),
         prespawn_temp_asrp = case_when(
           Scenario_num %in% c('Shade', 'Historical') ~ prespawn_temp - temp_diff_prespawn, # convert 7DADM to MDM
           !Scenario_num %in% c('Shade', 'Historical') ~
             case_when(
               year == 2019 ~ prespawn_temp,
               year == 2040 & Scenario_num %in% c('cc_only', 'fp_temp') & (left_ht <= 15 | right_ht <= 15) ~ prespawn_temp_mid,
               year == 2040 & Scenario_num %in% c('cc_only', 'fp_temp') & (left_ht > 15 & right_ht > 15) ~ prespawn_temp_mid + temp_diff_2040_prespawn,
               year == 2040 & !Scenario_num %in% c('cc_only', 'fp_temp') ~ ifelse(!Riparian == 'y' & can_ang > 170,
                                                                 prespawn_temp_mid,
                                                                 prespawn_temp_mid + temp_diff_2040_prespawn), 
               year == 2080 & Scenario_num %in% c('cc_only', 'fp_temp') & (left_ht <= 15 | right_ht <= 15) ~ prespawn_temp_late,
               year == 2080 & Scenario_num %in% c('cc_only', 'fp_temp') & (left_ht > 15 & right_ht > 15) ~ prespawn_temp_late + temp_diff_2080_prespawn,
               year == 2080 & !Scenario_num %in% c('cc_only', 'fp_temp') ~ ifelse(!Riparian == 'y' & can_ang > 170,
                                                                 prespawn_temp_late,
                                                                 prespawn_temp_late + temp_diff_2080_prespawn))),
         prespawn_temp_asrp = ifelse(Floodplain == 'y',
                                     prespawn_temp_asrp - (fp_temp_reduction * rest_perc),
                                     ifelse(Scenario_num %in% c('fp_temp', 'rip_and_flp'),
                                            prespawn_temp_asrp - (fp_temp_reduction * rest_perc),
                                            prespawn_temp_asrp)),
         prespawn_temp_asrp = ifelse(Scenario_num %in% growth_scenarios,
                                     ifelse(year == 2040,
                                            prespawn_temp_asrp - cc_mid_prespawn,
                                            prespawn_temp_asrp - cc_late_prespawn),
                                     prespawn_temp_asrp)) %>%
  select(-Habitat, -asrp_temp_w_growth, -asrp_temp_cc_only, -tm_2040, -tm_2080, -tm_2040_cc_only, -tm_2080_cc_only) %>%
  
  # add in future impervious area by GSU, scenario and year ----

left_join(., fut_imperv, by = c('GSU', 'year')) %>%
  mutate(future_imperv = ifelse(is.na(future_imperv),
                                0,
                                future_imperv))

rm(asrp_reach_data_scenarios, asrp_scenarios)
write.csv(asrp_reach_data %>% 
            filter(Scenario_num %in% c('Current', 'Historical', 'cc_only', 'rip_and_climate', 'rip_and_flp', 'fp_temp')), 
          'temp_paper/asrp_reach_data.csv')
