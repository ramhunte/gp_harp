# Prep work to begin asrp scenarios: ----
# 1. Prepare asrp scenarios data frame
# 2. Replicate habitat data to assign unique combinations of year (2019, 2040, 2080) and scenario (Current, 1-3).
# 3. Generate list of primary creeks

# Prep asrp_scenarios data frame.  Convert columns to character form for compatibility later in process ----

if (run_single_action == 'yes') {
  single_action_list <- c('wood_only', 'beaver_only', 'fp_only', 'barrier_only', 'riparian_only', 'no_climate_chg')
} else {
  single_action_list <- c('wood_only', 'beaver_only', 'fp_only')
}

scenario.years <- c(2040, 2080, 2019) # Note:  We use 2019 as the current year for the Current_asrp scenario

asrp_scenarios <- c('scenario_1', 'scenario_2', 'scenario_3')
single_actions <- c('wood_only', 'beaver_only', 'fp_only')
single_action_scenarios <- expand.grid(asrp_scenarios, single_actions) %>%
  unite(scenario, Var1, Var2, sep = '_') %>%
  pull(scenario)
scenario.nums <- c(asrp_scenarios, single_action_scenarios, 'dev_and_climate', diag_scenarios)


growth_scenarios <- c('scenario_1_riparian_only', 'scenario_2_riparian_only','scenario_3_riparian_only', 'scenario_1_no_climate_chg',
                      'scenario_2_no_climate_chg', 'scenario_3_no_climate_chg')

single_action_mvmt_scenarios <- c('scenario_1_wood_only', 'scenario_2_wood_only', 'scenario_3_wood_only', 'scenario_1_beaver_only', 
                                  'scenario_2_beaver_only', 'scenario_3_beaver_only', 'scenario_1_fp_only', 'scenario_2_fp_only', 'scenario_3_fp_only')

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
