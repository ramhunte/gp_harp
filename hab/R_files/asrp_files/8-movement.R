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

mvmt_data_curr <- data %>%
  filter(hab.scenario == 'Current',
         life.stage %in% c('winter', 'winter.2')) %>%
  select(Subbasin_num, capacity) %>%
  group_by(Subbasin_num) %>%
  summarize(current_cap = sum(capacity, na.rm = TRUE)) %>%
  ungroup()

asrp_mvmt_1 <- mvmt_data %>%
  left_join(., mvmt_data_curr) %>%
  spread(Scenario_num, cap_mvmt) %>%
  rename(scenario_1_cap_curr = current_cap) %>%
  mutate(scenario_2_cap_curr = scenario_1_cap_curr,
         scenario_3_cap_curr = scenario_1_cap_curr) %>%
  gather(Scenario_num, cap, -Subbasin_num) %>%
  group_by(Subbasin_num) %>%
  mutate(
    scenario = case_when(
      str_detect(Scenario_num, 'scenario_1') ~ 'scenario_1',
      str_detect(Scenario_num, 'scenario_2') ~ 'scenario_2',
      str_detect(Scenario_num, 'scenario_3') ~ 'scenario_3'),
    Scenario_num = ifelse(str_detect(Scenario_num, 'curr'),
                          'current',
                          as.character(Scenario_num)),
    diff = cap - cap[Scenario_num == 'current']) %>%
  group_by(Subbasin_num, scenario) %>%
  mutate(tot_diff = sum(diff, na.rm = T),
         perc = case_when(
           Scenario_num == 'current' ~ cap / (cap[Scenario_num == 'current'] + tot_diff),
           !Scenario_num == 'current' ~ diff / (cap[Scenario_num == 'current'] + tot_diff)),
         curr_cap = cap) %>%
  group_by(Subbasin_num, Scenario_num) %>%
  mutate(
    perc_single_action = case_when(
      !Scenario_num == 'current' ~ diff / (curr_cap + diff),
      Scenario_num == 'current' ~ 0),
    mvmt_base = case_when(
      str_detect(Scenario_num, 'wood') ~ 7,
      str_detect(Scenario_num, 'fp') ~ 3,
      str_detect(Scenario_num, 'beaver') ~ 3,
      !str_detect(Scenario_num, 'wood|fp|beaver') ~ 11))

asrp_mvmt <- asrp_mvmt_1 %>%
  group_by(Subbasin_num, scenario) %>%
  summarize(movement = sum(perc * mvmt_base, na.rm = T)) %>%
  rename(Scenario_num = scenario) %>%
  bind_rows(., asrp_mvmt_1 %>%
              filter(!Scenario_num == 'current') %>%
              group_by(Subbasin_num, Scenario_num) %>%
              summarize(movement = sum((perc_single_action * mvmt_base) + ((1 - perc_single_action) * 11), na.rm = T))) %>%
  mutate(movement = ifelse(movement == 0,
                           NA,
                           movement))
