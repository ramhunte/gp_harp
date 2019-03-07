ASRP_scenario_1_2040 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_1",
         year == 2040) %>%
  select(-asrp_scenario, -year)

ASRP_scenario_1_2080 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_1",
         year == 2080) %>%
  select(-asrp_scenario, -year)

ASRP_scenario_2_2040 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_2",
         year == 2040) %>%
  select(-asrp_scenario, -year)

ASRP_scenario_2_2080 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_2",
         year == 2080) %>%
  select(-asrp_scenario, -year)

ASRP_scenario_3_2040 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_3",
         year == 2040) %>%
  select(-asrp_scenario, -year)

ASRP_scenario_3_2080 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "scenario_3",
         year == 2080) %>%
  select(-asrp_scenario, -year)

ASRP_Current_asrp_2019 <- asrp.scenarios.loop1 %>%
  filter(asrp_scenario == "Current_asrp",
         year == 2019) %>%
  select(-asrp_scenario, -year)