
edr_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

spawners_edr <- lapply(edr_species, function(s) {
  read.csv(paste0('outputs/', s, '/lcm/', str_replace(s, '_', '.'), '_abundance_by_EDR.csv')) %>%
    mutate(species = s) %>%
    select(species,scenario, EcoRegion, basinwide_spawners)
    }) %>%
  do.call('rbind',.) %>%
  filter(scenario %in% c('Historical', 'Current')) %>%
  spread(scenario, basinwide_spawners) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(Current, na.rm = T)) %>%
  ungroup() %>%
  mutate(restoration_potential = Historical - Current,
         perc_of_basin = Current / spawners_basinwide,
         rank_diff = rank(-restoration_potential, ties.method = 'min'),
         rank_curr = rank(-Current, ties.method = 'min'),
         rank_weighted = rank(-perc_of_basin, ties.method = 'min')) %>%
  rowwise() %>%
  mutate(rank = mean(c(rank_diff, rank_curr)),
         rank_wt = mean(c(rank_diff, rank_weighted))) %>%
  select(species, EcoRegion, Current, Historical, restoration_potential, rank, rank_wt)


write.csv(spawners_edr, 'srt_figures/results/spawners_edr.csv')

