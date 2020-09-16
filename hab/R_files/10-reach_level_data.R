# # This script creates a table from which noaaid level characteristics are drawn in the ASRP scenario scripts 

asrp_reach_data <- read.csv('misc/scenarios_df.csv') %>%
  select(-X) %>%
  mutate(wood_intensity_scalar = 1,
         fp_intensity_scalar = 1,
         beaver_intensity_scalar = case_when(!Scenario_num %in% diag_scenarios  & managed_forest == 'y' ~ .1,
                                             TRUE ~ 1)) %>%
  left_join(., flowline %>%
              select(noaaid, GSU, Reach, species, can_ang, Subbasin_num, Habitat, BF_width))  %>%
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
  left_join(., fut_imperv, by = c('GSU', 'year')) %>%
  mutate(future_imperv = ifelse(is.na(future_imperv),
                                0,
                                future_imperv))
