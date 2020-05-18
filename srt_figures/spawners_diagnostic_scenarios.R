diagnostic_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_diagnostics <- lcm_results <- lapply(diagnostic_species, function(l) {
  
  read.csv(paste0('outputs/', l, '/lcm/', str_replace(l, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = l) %>%
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
  ungroup() %>%
  # group_by(scenario) %>%
  mutate(perc_of_basin = Current_spawners / spawners_basinwide,
         rank_diff = rank(-restoration_potential, ties.method = 'min'),
         rank_curr = rank(-Current_spawners, ties.method = 'min'),
         rank_weighted = rank(-perc_of_basin, ties.method = 'min')) %>%
  
  rowwise() %>%
  mutate(rank = mean(c(rank_diff, rank_curr)),
         rank_wt = mean(c(rank_diff, rank_weighted))) %>% 
  filter(!scenario %in% c('Current', 'Historical')) %>%
  select(species, Subbasin, scenario, Current_spawners, spawners, restoration_potential, rank, rank_wt) %>%
  # gather(group, data, c(restoration_potential, spawners, rank, rank_wt)) %>%
  # unite(scenario, group, col = 'scenario_data') %>%
  # spread(scenario_data, data) %>%
  left_join(., subbasin_names) 

spawners_diagnostics <- spawners_diagnostics[c('species', 'Subbasin', 'Subbasin_num', 'EcoRegion', 'scenario','Current_spawners', 'spawners', 'restoration_potential', 'rank', 'rank_wt')]
# spawners_diagnostics_2 <- spawners_diagnostics[c('species', 'Subbasin', 'Subbasin_num', 'EcoRegion', 'Current_spawners',
                                               # 'Barriers_spawners', 'Barriers_restoration_potential', 'Barriers_rank', 'Barriers_rank_wt',
                                               # 'Beaver_spawners', 'Beaver_restoration_potential', 'Beaver_rank', 'Beaver_rank_wt',
                                               # 'Floodplain_spawners', 'Floodplain_restoration_potential', 'Floodplain_rank', 'Floodplain_rank_wt',
                                               # 'Shade_spawners', 'Shade_restoration_potential', 'Shade_rank', 'Shade_rank_wt',
                                               # 'Wood_spawners', 'Wood_restoration_potential', 'Wood_rank', 'Wood_rank_wt')]

write.csv(spawners_diagnostics, 'srt_figures/results/spawners_diagnostics.csv')
