subbasin_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_subbasin <- lapply(subbasin_species, function(sb) {
  read.csv(paste0('outputs/', sb, '/lcm/', str_replace(sb, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = sb) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species)
  }) %>%
  do.call('rbind',.) %>%
  filter(scenario %in% c('Historical', 'Current')) %>%
  spread(scenario, spawners) %>% 
  group_by(species) %>%
  mutate(spawners_basinwide = sum(Current, na.rm = T)) %>%
  ungroup() %>%
  left_join(., flowline_subbasin) %>%
  mutate(restoration_potential = Historical - Current,
         spawners_per_km = Current / length_km,
         perc_of_basin = Current / spawners_basinwide,
         rank_diff = rank(-restoration_potential, ties.method = 'first'),
         # rank_curr = rank(-Current, ties.method = 'first'),
         rank_weighted = rank(-perc_of_basin, ties.method = 'first'),
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
  select(species, Subbasin, Subbasin_num, EcoRegion, length_km, Current, Historical, restoration_potential, rank_diff, rank_species, spawners_per_km,
         rank_wt_per_km, rank_wt_km_species)

colnames(spawners_subbasin) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Total Km', 'Current spawners', 'Historical spawners',
                                 'Restoration potential (spawners)', 'Rank (spawners)', 'Rank (spawners by species)', 'Spawners per km', 
                                 'Rank (spawners/km)', 'Rank (spawners/km by species)')

  
write.csv(spawners_subbasin, 'srt_figures/results/spawners_subbasin.csv')
