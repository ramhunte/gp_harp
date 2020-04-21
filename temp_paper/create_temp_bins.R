temp_bins <- asrp_reach_data %>%
  mutate(temp_bin = ifelse(asrp_temp >= 24,
                                  'high',
                                  'low')) %>%
  filter(Scenario_num %in% c('cc_only', 'rip_and_climate', 'fp_temp', 'rip_and_flp', 'Current', 'Historical')) %>%
  group_by(Scenario_num, temp_bin, year) %>%
  tally() %>%
  unite(col = 'scenario', Scenario_num, year, sep = '_') %>%
  spread(temp_bin, n) %>%
  mutate(perc_above = high / (high + low) * 100)


         