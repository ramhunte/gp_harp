total_coho <- flowline %>%
  filter(species == 'coho',
         spawn_dist == 'Yes')%>% 
  group_by(Habitat) %>%
  summarize(length = sum(Shape_Length, na.rm = T))

beaver_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         Beaver == 'y')

beaver_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         Beaver == 'y')

beaver_gsu_3 <- asrp_scenarios %>% 
  filter(Scenario_num == 'scenario_3',
         Beaver == 'y')

beaver_length_1 <- flowline %>%
  left_join(beaver_gsu_1, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))

beaver_length_2 <- flowline %>%
  left_join(beaver_gsu_2, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))

beaver_length_3 <- flowline %>%
  left_join(beaver_gsu_3, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))

Floodplain_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         Floodplain == 'y')

Floodplain_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         Floodplain == 'y')

Floodplain_gsu_3 <- asrp_scenarios %>% 
  filter(Scenario_num == 'scenario_3',
         Floodplain == 'y')

Floodplain_length_1 <- flowline %>%
  left_join(Floodplain_gsu_1, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))

Floodplain_length_2 <- flowline %>%
  left_join(Floodplain_gsu_2, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))

Floodplain_length_3 <- flowline %>%
  left_join(Floodplain_gsu_3, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         Habitat == 'SmStream') %>%
  summarize(length = sum(Shape_Length, na.rm = T))
  
