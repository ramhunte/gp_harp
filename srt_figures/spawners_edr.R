
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
  left_join(flowline_edr) %>%
  mutate(restoration_potential = Historical - Current,
         spawners_per_km = Current / length_km,
         perc_of_basin = Current / spawners_basinwide,
         rank_diff = rank(-restoration_potential, ties.method = 'min'),
         # rank_curr = rank(-Current, ties.method = 'min'),
         rank_weighted = rank(-perc_of_basin, ties.method = 'min'),
         rank_weighted_spawner_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  rowwise() %>%
  mutate(
    # rank_mean = mean(c(rank_diff, rank_curr)),
         rank_wt_mean = mean(c(rank_diff, rank_weighted)),
         rank_wt_per_km_mean = mean(c(rank_diff, rank_weighted_spawner_per_km)),
         ) %>%
  ungroup() %>%
  mutate(
    # rank = rank(rank_mean, ties.method = ''),
         rank_wt = rank(rank_wt_mean, ties.method = 'min'),
         rank_wt_per_km = rank(rank_wt_per_km_mean, ties.method = 'min')) %>%
  group_by(species) %>%
  mutate(rank_species = rank(rank_diff, ties.method = 'min'),
         rank_wt_km_species = rank(rank_wt_per_km, ties.method = 'min')) %>%
select(species, EcoRegion, lenght_km, Current, Historical, restoration_potential,rank_diff, rank_species, spawners_per_km,rank_wt_per_km, rank_wt_km_species)
  

colnames(spawners_edr) <- c('Species', 'EcoRegion', 'Total Km', 'Current spawners', 'Historical spawners', 'Restoration potential (spawners)', 'Rank (spawners)',
                            'Rank (spawners by species)', 'Spawners per km', 'Rank (spawners/km)', 'Rank (spawners/km by species)')


write.csv(spawners_edr, 'srt_figures/results/spawners_edr.csv')

