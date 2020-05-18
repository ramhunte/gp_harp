subbasin_species <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
spawners_subbasin <- lapply(subbasin_species, function(sb) {
  read.csv(paste0('outputs/', sb, '/lcm/', str_replace(sb, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = sb) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species)
  })%>%
  do.call('rbind',.) %>%
  filter(scenario %in% c('Historical', 'Current')) %>%
  spread(scenario, spawners) %>% 
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
  left_join(., subbasin_names) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, Current, Historical, restoration_potential, rank, rank_wt)

  
write.csv(spawners_subbasin, 'srt_figures/results/spawners_subbasin.csv')
