diagnostic_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_diagnostics <- lcm_results <- lapply(diagnostic_species, function(l) {
  
  read.csv(paste0('outputs/', l, '/lcm/', str_replace(l, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = l) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species) %>%
    filter(scenario %in% diag_scenarios[]) %>%
    group_by(Subbasin, species) %>%
    mutate(Current_spawners = spawners[scenario == 'Current'],
           restoration_potential = spawners - Current_spawners) 
}) %>%
  do.call('rbind',.) %>%
  filter(!scenario %in% c('Current', 'Historical')) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(spawners[scenario == 'Current'])) %>%
  ungroup() %>%
  left_join(., flowline_subbasin) %>%
  mutate(perc_of_basin = Current_spawners / spawners_basinwide,
         spawners_per_km = Current_spawners / length_km,
         rank_diff = rank(-restoration_potential, ties.method = 'min'),
         # rank_curr = rank(-Current_spawners, ties.method = 'min'),
         rank_weighted = rank(-perc_of_basin, ties.method = 'min'),
         rank_weighted_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  rowwise() %>%
  mutate(
    # rank_mean = mean(c(rank_diff, rank_curr)),
         rank_wt_mean = mean(c(rank_diff, rank_weighted)),
         rank_wt_per_km_mean = mean(c(rank_diff, rank_weighted_per_km))) %>% 
  ungroup() %>%
  mutate(
    # rank = rank(rank_mean, ties.method = 'min'),
         rank_wt = rank(rank_wt_mean, ties.method = 'min'),
         rank_wt_per_km = rank(rank_wt_per_km_mean, ties.method = 'min')) %>%
  group_by(species) %>%
  mutate(rank_species = rank(rank_diff, ties.method = 'min'),
         rank_wt_km_species = rank(rank_wt_per_km, ties.method = 'min')) %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv')) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, scenario, Current_spawners, spawners, restoration_potential, rank_diff, rank_species, spawners_per_km, rank_wt_per_km,
         rank_wt_km_species) 
  

# spawners_diagnostics <- spawners_diagnostics[c('species', 'Subbasin', 'Subbasin_num', 'EcoRegion', 'scenario','Current_spawners', 'spawners', 'restoration_potential', 'rank', 'spawners_per_km', 'rank_wt_per_km')]

colnames(spawners_diagnostics) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Scenario', 'Current spawners', 'Diagnostic scenario spawners',
                                    'Restoration potential (spawners)', 'Rank (spawners)', 'Rank (spawners by species', 'Spawners per km', 
                                    'Rank (spawners/km)', 'Rank (spawners/km by species)')

write.csv(spawners_diagnostics, 'srt_figures/results/spawners_diagnostics.csv')
