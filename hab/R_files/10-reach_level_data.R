# This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

# Replicate flowline 4x, once for each scenario in `scenario.nums` ----

asrp_reach_data_scenarios <- lapply(scenario.nums, function(j) {
  flowline %>% 
    select(noaaid, GSU, forest, Reach, species, can_ang, Subbasin_num, Habitat, BF_width) %>%
    mutate(Scenario_num = j) 
}) %>%
  do.call('rbind',.) 

# replicate the replicated flowline 3 more times, once for each year in `scenario.years` ----

asrp_reach_data <- lapply(scenario.years, function(k) {
  asrp_reach_data_scenarios %>% 
    mutate(year = k)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios, 'dev_and_climate')),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_scenarios) & 
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_scenarios %>%
              select(GSU, Scenario_num, managed_forest)) %>%
  mutate(managed_forest = ifelse(is.na(managed_forest), 
                                 'n',
                                 as.character(managed_forest))) %>%
mutate(
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
      Scenario_num %in% diag_scenarios ~ 1,
      !Scenario_num %in% diag_scenarios ~
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
      Scenario_num %in% c('Shade', 'Historical') ~ 'y',
      !Scenario_num %in% c('Shade', 'Historical') ~
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
  mutate(LW = ifelse(str_detect(Scenario_num, regex('beaver|riparian|barrier|fp_only|lr|fine|shade', ignore_case = TRUE)),
                     'n',
                     as.character(LW)),
         Floodplain = ifelse(str_detect(Scenario_num, regex('wood_only|beaver|riparian|barrier|lr|fine|shade', ignore_case = TRUE)),
                             'n',
                             as.character(Floodplain)),
         Riparian = ifelse(str_detect(Scenario_num, regex('wood|beaver|fp|barrier|lr|fine', ignore_case = TRUE)),
                           'n',
                           as.character(Riparian)),
         Barriers = ifelse(str_detect(Scenario_num, regex('wood|beaver|fp|riparian|lr|fine|shade', ignore_case = TRUE)),
                           'n',
                           as.character(Barriers)),
         Beaver = ifelse(str_detect(Scenario_num, regex('wood|fp|barrier|riparian|lr|fine|shade', ignore_case = TRUE)),
                         'n',
                         as.character(Beaver))) %>%
  left_join(., wood_data) %>%
  mutate(woodmult_s_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_s - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         woodmult_w_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_w - 1) * rest_perc * wood_intensity_scalar),
                                  1)) %>%
  left_join(., read.csv('misc/temperature.csv')) %>%
  mutate(tempmult.asrp = temp_func(temperature),
         prespawn_temp_asrp = prespawn_temperature) %>%
  select(-Habitat) %>%
  # add in future impervious area by GSU, scenario and year ----

left_join(., fut_imperv, by = c('GSU', 'year')) %>%
  mutate(future_imperv = ifelse(is.na(future_imperv),
                                0,
                                future_imperv))
rm(asrp_reach_data_scenarios, asrp_scenarios)
