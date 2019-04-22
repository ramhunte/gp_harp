# This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

# Replicate flowline 4x, once for each scenario in `scenario.nums` ----

asrp_reach_data_scenarios <- lapply(scenario.nums, function(j) {
  flowline %>% 
    select(noaaid, GSU, forest, curr_temp, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, Reach, species, can_ang, Subbasin_num) %>%
    mutate(forest = ifelse(forest == "Yes",
                           "y", 
                           "n"),
           Scenario_num = j) 
}) %>%
  do.call('rbind',.) 

# replicate the replicated flowline 3 more times, once for each year in `scenario.years` ----

asrp_reach_data <- lapply(scenario.years, function(k) {
  asrp_reach_data_scenarios %>% 
    mutate(year = k)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3")),
         !(Scenario_num == "Current_asrp" & year %in% c(2040, 2080)),
         !(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", "scenario_2_fp_only", 
                               "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", "scenario_3_beaver_only") & 
             year %in% c(2040, 2080))) %>%
  
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
           year == 2019 ~ 0,
           year == 2040 ~ 1,
           year == 2080 ~ 1),
         wood_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     "scenario_3_beaver_only"),
                                 .6,
                                 0),
           year == 2040 ~ .6,
           year == 2080 ~ .6),
         fp_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     "scenario_3_beaver_only"),
                                 ifelse(forest == 'y',
                                        .3,
                                        .5),
                                 0),
           year %in% c(2040, 2080) & forest == 'y' ~ .3,
           year %in% c(2040, 2080) & !forest == 'y' ~ .5),
         beaver_intensity_scalar = case_when(
           year == 2019 ~ ifelse(Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", 
                                                     "scenario_2_fp_only", "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", 
                                                     "scenario_3_beaver_only"),
                                 .3,
                                 0),
           year == 2040 ~ .3,
           year == 2080 ~ .3)) %>%
  left_join(., asrp_scenarios) %>%
  
  
  mutate(

# Create single restoration percentage field based on whether or not forest == 'y' for each reach ----  
    
    rest_perc_f = ifelse(is.na(rest_perc_f),
                              0,
                              rest_perc_f),
         rest_perc_nf = ifelse(is.na(rest_perc_nf),
                               0,
                               rest_perc_nf),
         rest_perc = ifelse(forest == 'y',
                            rest_perc_f,
                            rest_perc_nf),

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
    Floodplain = case_when(
      is.na(Floodplain) ~ 'n',
      Floodplain == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y'),
      Floodplain == 'n' ~ 'n'),
    Beaver = case_when(
      is.na(Beaver) ~ 'n',
      Beaver == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y'),
      Beaver == 'n' ~ 'n'),
    Riparian = case_when(
      is.na(Riparian) ~ 'n',
      Riparian == 'y' ~
        ifelse(primary_cr_only == 'y' & !Reach %in% primary_cr,
               'n',
               'y'),
      Riparian == 'n' ~ 'n')) %>%
  select(-rest_perc_f, -rest_perc_nf) %>%

# Calculate wood and temperature multipliers based on the particular asrp scenario and year of each row ----
  
  left_join(., wood_data) %>%
  mutate(woodmult_s_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_s - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         woodmult_w_asrp = ifelse(LW == 'y',
                                  1 + ((woodmult_w - 1) * rest_perc * wood_intensity_scalar),
                                  1),
         asrp_temp = ifelse(Riparian == 'y',
                            asrp_temp_w_growth,
                            ifelse(can_ang > 170,
                                   asrp_temp_cc_only,
                                   asrp_temp_w_growth)),
         tempmult.asrp = ifelse(species %in% c("coho", "steelhead"),
                                temp_func(asrp_temp),
                                1))

rm(asrp_reach_data_scenarios)
