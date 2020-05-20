subbasin_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_subbasin <- lapply(subbasin_species, function(sb) {
  read.csv(paste0('outputs/', sb, '/lcm/', str_replace(sb, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = capitalize(str_replace(sb, '_', ' '))) %>%
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
  mutate(length_km = round(length_km, digits = 2),
         restoration_potential = Historical - Current,
         spawners_per_km = round(restoration_potential / length_km, digits = 1),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100), digits = 2) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'first'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv')) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, length_km, Current, Historical, restoration_potential, rank_rest_potential, spawners_per_km,
         rank_spawner_per_km, perc_of_pop, rank_percent)

colnames(spawners_subbasin) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Total Km', 'Current spawners', 'Historical spawners',
                                 'Restoration potential (spawners)', 'Rank (spawners)', 'Spawners per km', 'Rank (spawners/km)', 'Percent change', 
                                 'Rank (percent change)')


write.csv(spawners_subbasin, 'srt_figures/results/spawners_subbasin.csv')
