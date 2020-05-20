
# Gather up data ----
spawners_subbasin_df <- lapply(subbasin_species, function(l) {
  read.csv(paste0('outputs/', l, '/lcm/', str_replace(l, '_', '.'), '_abundance_by_subbasin.csv')) %>%
    mutate(species = Hmisc::capitalize(str_replace(l, '_', ' '))) %>%
    rename(Subbasin = natal.basin) %>%
    select(Subbasin, scenario,spawners, species, Pn) %>%
    filter(scenario %in% diag_scenarios) %>%
    group_by(Subbasin, species) %>%
    mutate(Current_spawners = spawners[scenario == 'Current'],
           restoration_potential = spawners - Current_spawners,
           Current_pn = Pn[scenario == 'Current'],
           restoration_potential_pn = round(Pn - Current_pn, 2)) 
}) %>%
  do.call('rbind',.) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(Current_spawners[scenario == 'Current'], na.rm = T)) %>%
  ungroup()



# Rank with Current and historical only ----

spawners_subbasin <- spawners_subbasin_df %>%
  filter(scenario == 'Historical') %>%
  rename(Historical = spawners, Current = Current_spawners) %>%
  left_join(., flowline_subbasin) %>%
  mutate(length_km = round(length_km, digits = 2),
         restoration_potential = Historical - Current,
         spawners_per_km = round(restoration_potential / length_km, digits = 1),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100), digits = 2) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'first'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min'),
         rank_rest_potential_pn = rank(-restoration_potential_pn, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv')) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, length_km, Current, Historical, 
         restoration_potential, rank_rest_potential, spawners_per_km,
         rank_spawner_per_km, perc_of_pop, rank_percent,
         restoration_potential_pn, rank_rest_potential_pn)

colnames(spawners_subbasin) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Total Km', 'Current spawners', 
                                 'Historical spawners','Restoration potential (spawners)', 'Rank (spawners)', 
                                 'Spawners per km', 'Rank (spawners/km)', 'Percent change', 'Rank (percent change)',
                                 'Productivity (Pn) change','Rank (Pn change)')




### Rank with diagnostic scenarios ----


spawners_diagnostics <- spawners_subbasin_df %>%
  filter(!scenario %in% c('Historical', 'Current')) %>%
  left_join(., flowline_subbasin) %>%
  mutate(length_km = round(length_km, digits = 2),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2),
         spawners_per_km = round(Current_spawners / length_km, digits = 1)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min'),
         rank_rest_potential_pn = rank(-restoration_potential_pn, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  left_join(., read.csv('lcm/data/Subbasin_names.csv')) %>%
  select(species, Subbasin, Subbasin_num, EcoRegion, length_km, scenario, 
         Current_spawners, spawners, restoration_potential, rank_rest_potential, 
         spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent,
         restoration_potential_pn, rank_rest_potential_pn) 

colnames(spawners_diagnostics) <- c('Species', 'Subbasin', 'Subbasin number', 'EcoRegion', 'Total Km', 'Scenario', 
                                    'Current spawners', 
                                    'Diagnostic scenario spawners',
                                    'Restoration potential (spawners)', 'Rank (spawners)', 'Spawners per km', 
                                    'Rank (spawners/km)', 'Percent change', 'Rank(percent change)',
                                    'Productivity (Pn) change','Rank (Pn change)')


