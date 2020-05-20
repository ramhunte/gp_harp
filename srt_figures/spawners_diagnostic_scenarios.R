diagnostic_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_diagnostics <- lcm_results <- lapply(diagnostic_species, function(l) {
  
  read.csv(paste0('outputs/', l, '/lcm/', str_replace(l, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = capitalize(str_replace(l, '_', ' '))) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species) %>%
    filter(scenario %in% diag_scenarios) %>%
    group_by(Subbasin, species) %>%
    mutate(Current_spawners = spawners[scenario == 'Current'],
           restoration_potential = spawners - Current_spawners) 
}) %>%
  do.call('rbind',.) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(spawners[scenario == 'Current'])) %>%
  filter(!scenario %in% c('Current', 'Historical')) %>%
  ungroup() %>%
  left_join(., flowline_subbasin) %>%
  mutate(length_km = round(length_km, digits = 2),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2),
         spawners_per_km = round(Current_spawners / length_km, digits = 1)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv')) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, length_km, scenario, Current_spawners, spawners, restoration_potential, rank_rest_potential, 
         spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent) 

colnames(spawners_diagnostics) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Total Km', 'Scenario', 'Current spawners', 
                                    'Diagnostic scenario spawners',
                                    'Restoration potential (spawners)', 'Rank (spawners)', 'Spawners per km', 
                                    'Rank (spawners/km)', 'Percent change', 'Rank(percent change)')

write.csv(spawners_diagnostics, 'srt_figures/results/spawners_diagnostics.csv')
