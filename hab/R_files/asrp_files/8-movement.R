mvmt_data <- bind_rows(asrp_ss_mvmt, asrp_lr_mvmt, asrp_fp_mvmt) 

if (fishtype == "steelhead") {
  mvmt_data %<>%
    bind_rows(., mvmt_data %>%
                filter(life.stage %in% c("summer", "winter")) %>%
                mutate(life.stage = ifelse(life.stage == "summer",
                                           "summer.2",
                                           "winter.2")))
}

mvmt_data %<>% 
  left_join(., density) %>%
  mutate(capacity = Area * Density) %>% 
  filter(life.stage %in% c("winter", "winter.2")) %>%
  group_by(Subbasin_num, Scenario_num) %>%
  summarize(cap_mvmt = sum(capacity, na.rm = T))

asrp_mvmt <- mvmt_data %>%
  spread(Scenario_num, cap_mvmt) %>%
  left_join(., data %>%
              filter(hab.scenario == "Current",
                     life.stage == "winter") %>%
              select(Subbasin_num, capacity) %>%
              group_by(Subbasin_num) %>%
              summarize(cap_curr = sum(capacity, na.rm = TRUE)) %>%
              ungroup(),
            by = "Subbasin_num") %>%
  left_join(., asrp_cap %>%
              filter(life.stage == "winter", Scenario_num == "Current_asrp", year == 2019) %>%
              select(Subbasin_num, capacity) %>%
              group_by(Subbasin_num) %>%
              summarize(cap_scen = sum(capacity, na.rm = TRUE)) %>%
              ungroup(), 
            by = "Subbasin_num") %>%
  mutate(
    wood_diff_1 = scenario_1_wood_only - cap_scen,
    fp_diff_1 = scenario_1_fp_only - cap_scen,
    beaver_diff_1 = scenario_1_beaver_only - cap_scen,
    tot_diff_1 = wood_diff_1 + fp_diff_1 + beaver_diff_1,
    wood_perc_1 = wood_diff_1 / (cap_scen + tot_diff_1),
    fp_perc_1 = fp_diff_1 / (cap_scen + tot_diff_1),
    beaver_perc_1 = beaver_diff_1 / (cap_scen + tot_diff_1),
    curr_perc_1 = (cap_scen / (cap_scen + tot_diff_1)),
    scenario_1 = (wood_perc_1 * 7) + (fp_perc_1 * 3) + (beaver_perc_1 * 3) + (curr_perc_1 * 11),
    wood_diff_2 = scenario_2_wood_only - cap_scen,
    fp_diff_2 = scenario_2_fp_only - cap_scen,
    beaver_diff_2 = scenario_2_beaver_only - cap_scen,
    tot_diff_2 = wood_diff_2 + fp_diff_2 + beaver_diff_2,
    wood_perc_2 = wood_diff_2 / (cap_scen + tot_diff_2),
    fp_perc_2 = fp_diff_2 / (cap_scen + tot_diff_2),
    beaver_perc_2 = beaver_diff_2 / (cap_scen + tot_diff_2),
    curr_perc_2 = (cap_scen / (cap_scen + tot_diff_2)),
    scenario_2 = (wood_perc_2 * 7) + (fp_perc_2 * 3) + (beaver_perc_2 * 3) + (curr_perc_2 * 11),
    wood_diff_3 = scenario_3_wood_only - cap_scen,
    fp_diff_3 = scenario_3_fp_only - cap_scen,
    beaver_diff_3 = scenario_3_beaver_only - cap_scen,
    tot_diff_3 = wood_diff_3 + fp_diff_3 + beaver_diff_3,
    wood_perc_3 = wood_diff_3 / (cap_scen + tot_diff_3),
    fp_perc_3 = fp_diff_3 / (cap_scen + tot_diff_3),
    beaver_perc_3 = beaver_diff_3 / (cap_scen + tot_diff_3),
    curr_perc_3 = (cap_scen / (cap_scen + tot_diff_3)),
    scenario_3 = (wood_perc_3 * 7) + (fp_perc_3 * 3) + (beaver_perc_3 * 3) + (curr_perc_3 * 11)) %>%
  select(Subbasin_num, scenario_1, scenario_2, scenario_3) %>%
  gather(Scenario_num, movement, scenario_1:scenario_3)
