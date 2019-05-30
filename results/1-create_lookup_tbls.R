#Create lookup table of habmodel and lcm outputs from current branch

hab_outputs <- lapply(c('coho'),function(y) {#, 'spring_chinook', 'fall_chinook', 'steelhead'), function(y) {
  # read.csv(file.path('outputs', y, 'hab.scenarios/outputs_long/habmodel_outputs.csv')) %>%
  read.csv(file.path('results', paste0(y, '_94'), 'hab.scenarios/outputs_long/habmodel_outputs.csv')) %>%
    mutate(species = y)
}) %>%
  do.call('rbind',.) %>%
  select(-X)

hab_outputs <- lapply(hab_outputs, function(x) {
  gsub('_', '.', x)
}) %>%
  data.frame() %>%
  mutate(scenario = ifelse(grepl('^scenario.', hab.scenario) | grepl('^Current.', hab.scenario), 
                           paste0('ASRP.', hab.scenario),
                           as.character(hab.scenario))) %>%
  gather(param, data, capacity:survival) %>%
  unite(col = param, life.stage, param, sep = '.') %>%
  spread(param, data) %>%
  
  left_join(., subbasin_names %>%
              mutate(Subbasin_num = as.factor(Subbasin_num))) %>%
  select( -hab.scenario, -adults.survival, -egg.to.fry.capacity, -eggs.survival, -prespawn.capacity)

lcm_outputs <- bind_rows(
  # read.csv("outputs/coho/lcm/coho_abundance_by_subbasin.csv") %>%
  read.csv(file.path('results', 'coho_94', 'lcm', 'coho_abundance_by_subbasin.csv')) %>%
    mutate(species = 'coho')#,
  # read.csv("outputs/spring_chinook/lcm/spring.chinook_abundance_by_subbasin.csv") %>%
    # mutate(species = 'spring.chinook'),
  # read.csv("outputs/fall_chinook/lcm/fall.chinook_abundance_by_subbasin.csv") %>%
    # mutate(species = 'fall.chinook'),
  # read.csv("outputs/steelhead/lcm/steelhead_abundance_by_subbasin.csv") %>%
    # mutate(species = 'steelhead')
) %>%
  rename(Subbasin = natal.basin) %>%
  select(-X)

lookup_tbl <- full_join(hab_outputs, lcm_outputs)


# Create lookup table of habmodel and lcm outputs from dev branch

hab_outputs_93 <- lapply(c('coho'),function(y) {#, 'spring_chinook', 'fall_chinook', 'steelhead'), function(y) {
  # read.csv(file.path('outputs', y, 'hab.scenarios/outputs_long/habmodel_outputs.csv')) %>%
  read.csv(file.path('results', paste0(y, '_93'), 'hab.scenarios/outputs_long/habmodel_outputs.csv')) %>%
    mutate(species = y)
}) %>%
  do.call('rbind',.) %>%
  select(-X)

hab_outputs_93 <- lapply(hab_outputs_93, function(x) {
  gsub('_', '.', x)
}) %>%
  data.frame() %>%
  mutate(scenario = ifelse(grepl('^scenario.', hab.scenario) | grepl('^Current.', hab.scenario), 
                           paste0('ASRP.', hab.scenario),
                           as.character(hab.scenario))) %>%
  gather(param, data, capacity:survival) %>%
  unite(col = param, life.stage, param, sep = '.') %>%
  spread(param, data) %>%
  
  left_join(., subbasin_names %>%
              mutate(Subbasin_num = as.factor(Subbasin_num))) %>%
  select( -hab.scenario, -adults.survival, -egg.to.fry.capacity, -eggs.survival, -prespawn.capacity)

lcm_outputs_93 <- bind_rows(
  # read.csv("outputs/coho/lcm/coho_abundance_by_subbasin.csv") %>%
  read.csv(file.path('results', 'coho_93', 'lcm', 'coho_abundance_by_subbasin.csv')) %>%
    mutate(species = 'coho')#,
  # read.csv("outputs/spring_chinook/lcm/spring.chinook_abundance_by_subbasin.csv") %>%
  # mutate(species = 'spring.chinook'),
  # read.csv("outputs/fall_chinook/lcm/fall.chinook_abundance_by_subbasin.csv") %>%
  # mutate(species = 'fall.chinook'),
  # read.csv("outputs/steelhead/lcm/steelhead_abundance_by_subbasin.csv") %>%
  # mutate(species = 'steelhead')
) %>%
  rename(Subbasin = natal.basin) %>%
  select(-X)

lookup_tbl_93 <- full_join(hab_outputs_93, lcm_outputs_93)

# hab_outputs_dev <- lapply(c('coho', 'spring_chinook', 'fall_chinook', 'steelhead'), function(z) {
#   hab_cmd <- paste0('git show dev:outputs/', z, '/hab.scenarios/outputs_long/habmodel_outputs.csv > habmodel_outputs_', z, '_dev.csv')
#   shell(cmd = hab_cmd) 
#   read.csv(paste0('habmodel_outputs_', z, '_dev.csv')) %>%
#     mutate(species = z)
# }) %>%
#   do.call('rbind',.) %>%
#   select(-X)
# 
# hab_outputs_dev <- data.frame(lapply(hab_outputs_dev, function(a) {
#   gsub('_', '.', a)
# })) %>%
#   mutate(scenario = ifelse(grepl('^scenario.', hab.scenario) | grepl('^Current.', hab.scenario), 
#                            paste0('ASRP.', hab.scenario),
#                            as.character(hab.scenario))) %>%
#   gather(param, data, capacity:survival) %>%
#   unite(col = param, life.stage, param, sep = '.') %>%
#   spread(param, data) %>%
#   
#   left_join(., subbasin_names %>%
#               mutate(Subbasin_num = as.factor(Subbasin_num))) %>%
#   select( -hab.scenario, -adults.survival, -egg.to.fry.capacity, -eggs.survival, -prespawn.capacity)
# 
# 
# lcm_outputs_dev <- lapply(c('coho', 'spring_chinook', 'fall_chinook', 'steelhead'), function(b) {
#   
#   path_to_file <- file.path('outputs', b, 'lcm') %>%
#     list.files(., pattern = 'abundance_by_subbasin.csv', full.names = T)
#   
#   lcm_cmd <- paste0('git show dev:', path_to_file, ' > lcm_outputs_', b, '_dev.csv')
#   shell(cmd = lcm_cmd)
#   read.csv(paste0('lcm_outputs_', b, '_dev.csv')) %>%
#     mutate(species = b) %>%
#     select(-X) %>%
#     gather(param, data, c(-natal.basin, -scenario, -species))
# }) %>%
#   do.call('rbind',.) %>%
#   rename(Subbasin = natal.basin) %>%
#   group_by(Subbasin, scenario, param, species) %>%
#   summarize(data = sum(data, na.rm = T)) %>%
#   ungroup() %>%
#   spread(param, data)
# 
# dev_lookup_tbl <- full_join(hab_outputs_dev, lcm_outputs_dev)

# Create lookup table to compare dev with feature branch

lookup_tbl_compare <- lookup_tbl %>%
  gather(param, value, c(adults.capacity:winter.survival, spawners:non.natal.smolts)) %>%
  mutate(version = 'feature') %>%
  full_join(., lookup_tbl_93 %>%
              gather(param, value, c(adults.capacity:winter.survival, spawners:non.natal.smolts)) %>%
              mutate(version = 'dev')) %>%
  mutate(version = factor(version, levels = c('dev', 'feature')),
         value = as.numeric(value)) %>%
  select(-Subbasin_num) %>%
  gather(basin_type, basin, Subbasin:EcoRegion) %>%
  group_by(species, scenario, param, version, basin_type, basin) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(., plot.params)

