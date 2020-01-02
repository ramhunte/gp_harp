# Generate data frame with passage percentage at the noaaid level for each asrp scenario and year ----
asrp_culvs <- asrp_reach_data %>%
  select(noaaid, GSU, Scenario_num, year, Barriers) %>%
  left_join(., flowline %>%
              select(noaaid, GSU, pass_tot_natural, pass_tot)) %>%
  mutate(
    pass_tot_asrp = case_when(
      Barriers == "y" ~ pass_tot_natural,
      Barriers == 'n' ~ pass_tot
    )) %>%
  filter(!(year == 2019 & Scenario_num %in% c('scenario_1', 'scenario_2', 'scenario_3', growth_scenarios, 'cc_only')),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_scenarios) & year %in% c(2040, 2080))) %>%
  select(noaaid, pass_tot_asrp, Scenario_num, year)
