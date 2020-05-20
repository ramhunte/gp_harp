#### Current and historical only ----
edr_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

spawners_edr <- lapply(edr_species, function(s) {
  read.csv(paste0('outputs/', s, '/lcm/', str_replace(s, '_', '.'), '_abundance_by_EDR.csv')) %>%
    mutate(species = capitalize(str_replace(s, '_', ' '))) %>%
    select(species,scenario, EcoRegion, basinwide_spawners) 
}) %>%
  do.call('rbind',.) %>%
  filter(scenario %in% c('Historical', 'Current')) %>%
  spread(scenario, basinwide_spawners) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(Current, na.rm = T)) %>%
  ungroup() %>%
  left_join(flowline_edr) %>%
  mutate(length_km = round(length_km, digits = 2),
         restoration_potential = Historical - Current,
         spawners_per_km = round(restoration_potential / length_km, digits = 1),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  select(species, EcoRegion, length_km, Current, Historical, restoration_potential, rank_rest_potential, spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent) 

colnames(spawners_edr) <- c('Species', 'EcoRegion', 'Total Km', 'Current spawners', 'Historical spawners', 'Restoration potential (spawners)', 
                            'Rank (spawners)', 'Spawners per km', 'Rank (spawners/km)', 'Percent change', 'Rank (percent change)')

write.csv(spawners_edr, 'srt_figures/results/spawners_edr.csv')

#### Diagnostic scenarios included ----
spawners_edr_diagnostics <- lapply(edr_species, function(t) {
  read.csv(paste0('outputs/', t, '/lcm/', str_replace(t, '_', '.'), '_abundance_by_EDR.csv')) %>%
    mutate(species = capitalize(str_replace(t, '_', ' '))) %>%
    select(species,scenario, EcoRegion, basinwide_spawners) %>%
    filter(scenario %in% diag_scenarios) %>%
    group_by(EcoRegion, species) %>%
    mutate(Current_spawners = basinwide_spawners[scenario == 'Current'],
           restoration_potential = basinwide_spawners - Current_spawners)
}) %>%
  do.call('rbind',.) %>%
  rename(spawners = basinwide_spawners) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(spawners[scenario == 'Current'])) %>%
  filter(!scenario %in% c('Current', 'Historical')) %>%
  ungroup() %>%
  left_join(., flowline_edr) %>%
  mutate(length_km = round(length_km, digits = 2),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2),
         spawners_per_km = round(Current_spawners / length_km, digits = 1)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  select(species, EcoRegion, length_km, scenario, Current_spawners, spawners, restoration_potential, rank_rest_potential, spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent)

colnames(spawners_edr_diagnostics) <- c('Species', 'EcoRegion', 'Total Km', 'Diagnostic scenario', 'Current spawners', 'Diagnostic scenario spawners', 'Restoration potential (spawners)', 
                                        'Rank (spawners)', 'Spawners per km', 'Rank (spawners/km)', 'Percent change', 'Rank (percent change)')

write.csv(spawners_edr_diagnostics, 'srt_figures/results/spawners_edr_diagnostics.csv')
