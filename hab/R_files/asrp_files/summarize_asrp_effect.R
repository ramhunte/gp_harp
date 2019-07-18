total_coho <- flowline %>%
  filter(species == 'coho',
         spawn_dist == 'Yes')%>% 
  group_by(Habitat) %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))



coho_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1')
gsu_1 <- unique(coho_gsu_1$GSU)

total_coho_1 <- flowline %>%
  filter(species == 'coho',
         spawn_dist == 'Yes',
         GSU %in% gsu_1) %>%
  summarize(length = sum(Shape_Length / 1000,na.rm = T))

total_coho_fp <- fp2 %>%
  filter(hab.scenario %in% c('Current', 'Historical')) %>%
  group_by(hab.scenario, life.stage) %>%
  summarize(Area = sum(Area, na.rm = T))

beaver_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         Beaver == 'y')

beaver_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         Beaver == 'y')

beaver_gsu_3 <- asrp_scenarios %>% 
  filter(Scenario_num == 'scenario_3',
         Beaver == 'y')

# beaver_gsu_3 <- asrp_reach_data %>%
#   filter(Scenario_num == 'scenario_3',
#          Beaver == 'y',
#          year == 2019) %>%
#   group_by(GSU) %>%
#   summarize(LW = unique(LW, na.rm = T),
#             Barriers = unique(Barriers, na.rm = T),
#             Floodplain = unique(Floodplain, na.rm = T),
#             Beaver = unique(Beaver, na.rm = T),
#             Riparian = unique(Riparian, na.rm = T),
#             rest_perc = unique(rest_perc, na.rm = T),
#             primary_cr_only = unique(primary_cr_only, na.rm = T),
#             Scenario_num = unique(Scenario_num, na.rm = T),
#             beaver_intensity_scalar = unique(beaver_intensity_scalar, na.rm = T))

beaver_length_1 <- flowline %>%
  left_join(beaver_gsu_1, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream',
         species == 'coho',
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

beaver_length_2 <- flowline %>%
  left_join(beaver_gsu_2, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream',
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

beaver_length_3 <- flowline %>%
  left_join(beaver_gsu_3, by = 'GSU') %>% 
  filter(Beaver == 'y',
         Habitat == 'SmStream',
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

beaver_forest_length_3 <-  asrp_reach_data %>%
              filter(Scenario_num == 'scenario_3',
                     year == 2019) %>%
  summarize(beav_perc = mean(beaver_intensity_scalar, na.rm = T))
  

Floodplain_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         Floodplain == 'y')

Floodplain_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         Floodplain == 'y')

Floodplain_gsu_3 <- asrp_scenarios %>% 
  filter(Scenario_num == 'scenario_3',
         Floodplain == 'y')

Floodplain_length_1 <- fp2 %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'scenario_1',
                     year == 2019) %>%
              select(noaaid, GSU, Scenario_num, Floodplain)) %>%
  # left_join(Floodplain_gsu_1, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         hab.scenario %in% c('Current', 'Historical')) %>%
  group_by(hab.scenario, life.stage) %>%
  summarize(Area = sum(Area, na.rm = T))

Floodplain_length_2 <- fp2 %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'scenario_2',
                     year == 2019) %>%
              select(noaaid, GSU, Scenario_num, Floodplain)) %>%
  # left_join(Floodplain_gsu_2, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         hab.scenario %in% c('Current', 'Historical')) %>%
  group_by(hab.scenario, life.stage) %>%
  summarize(Area = sum(Area, na.rm = T))


Floodplain_length_3 <- fp2 %>%
  left_join(., asrp_reach_data %>%
              filter(Scenario_num == 'scenario_3',
                     year == 2019) %>%
              select(noaaid, GSU, Scenario_num, Floodplain)) %>%
  # left_join(Floodplain_gsu_3, by = 'GSU') %>% 
  filter(Floodplain == 'y',
         hab.scenario %in% c('Current', 'Historical')) %>%
  group_by(hab.scenario, life.stage) %>%
  summarize(Area = sum(Area, na.rm = T))

wood_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         LW == 'y')

wood_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         LW == 'y')

wood_gsu_3 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_3',
         LW == 'y')

wood_length_1 <- flowline %>%
  left_join(wood_gsu_1, by = 'GSU') %>%
  filter(LW == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

wood_length_2 <- flowline %>%
  left_join(wood_gsu_2, by = 'GSU') %>%
  filter(LW == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

wood_length_3 <- flowline %>%
  left_join(wood_gsu_3, by = 'GSU') %>%
  filter(LW == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

riparian_gsu_1 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_1',
         Riparian == 'y')

riparian_gsu_2 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_2',
         Riparian == 'y') 

riparian_gsu_3 <- asrp_scenarios %>%
  filter(Scenario_num == 'scenario_3',
         Riparian == 'y')

riparian_length_1 <- flowline %>%
  left_join(., riparian_gsu_1, by = 'GSU') %>%
  filter(Riparian == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length/1000, na.rm = T))

riparian_length_2 <- flowline %>%
  left_join(., riparian_gsu_2, by = 'GSU') %>%
  filter(Riparian == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length / 1000, na.rm = T))

riparian_length_3 <- flowline %>%
  left_join(., riparian_gsu_2, by = 'GSU') %>%
  filter(Riparian == 'y',
         Habitat %in% c('SmStream', 'LgRiver'),
         species == 'coho', 
         spawn_dist == 'Yes') %>%
  summarize(length = sum(Shape_Length / 1000, na.rm  = T))
  
