
# Gather up data ----

spawners_edr_df <- lapply(spp, function(t) {
  read.csv(paste0('outputs/', t, '/lcm/', str_replace(t, '_', '.'), '_abundance_by_EDR.csv')) %>%
    mutate(species = Hmisc::capitalize(str_replace(t, '_', ' '))) %>%
    select(species,scenario, EcoRegion, basinwide_spawners, Pn) %>%
    filter(scenario %in% diag_scenarios) %>%
    group_by(EcoRegion, species) %>%
    mutate(Current_spawners = basinwide_spawners[scenario == 'Current'],
           restoration_potential = basinwide_spawners - Current_spawners,
           Current_pn = Pn[scenario == 'Current'],
           restoration_potential_pn = round(Pn - Current_pn, 2))
}) %>%
  do.call('rbind',.) %>%
  group_by(species) %>%
  mutate(spawners_basinwide = sum(Current_spawners[scenario == 'Current']))
  

  
#### Rank by Current and historical only ----
  
spawners_edr <- spawners_edr_df %>%
  ungroup() %>%
  filter(scenario == 'Historical') %>%
  rename(Historical = basinwide_spawners, Current = Current_spawners) %>%
  left_join(flowline_edr) %>%
  mutate(length_km = round(length_km, digits = 2),
         restoration_potential = Historical - Current,
         spawners_per_km = round(restoration_potential / length_km, digits = 1),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min'),
         rank_rest_potential_pn = rank(-restoration_potential_pn, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  select(species, EcoRegion, length_km, Current, Historical, restoration_potential, 
         rank_rest_potential, spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent,
         restoration_potential_pn, rank_rest_potential_pn) 

colnames(spawners_edr) <- c('Species', 'EcoRegion', 'Total Km', 'Current spawners', 'Historical spawners', 
                            'Restoration potential (spawners)', 'Rank (spawners)', 'Spawners per km', 
                            'Rank (spawners/km)', 'Percent change', 'Rank (percent change)',
                            'Productivity (Pn) change','Rank (Pn change)')




#### Rank with Diagnostic scenarios ----

spawners_edr_diagnostics2 <- spawners_edr_df %>%
  rename(spawners = basinwide_spawners) %>%
  filter(!scenario %in% c('Current', 'Historical')) %>%
  ungroup() %>%
  left_join(., flowline_edr) %>%
  mutate(length_km = round(length_km, digits = 2),
         perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2),
         spawners_per_km = round(Current_spawners / length_km, digits = 1)) %>%
  group_by(species) %>%
  mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
         rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min'),
         rank_rest_potential_pn = rank(-restoration_potential_pn, ties.method = 'min')) %>%
  ungroup() %>%
  mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
  select(species, EcoRegion, length_km, scenario, Current_spawners, spawners, restoration_potential, 
         rank_rest_potential, spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent, 
         restoration_potential_pn, rank_rest_potential_pn)

colnames(spawners_edr_diagnostics2) <- c('Species', 'EcoRegion', 'Total Km', 'Diagnostic scenario', 
                                        'Current spawners', 'Diagnostic scenario spawners', 
                                        'Restoration potential (spawners)', 
                                        'Rank (spawners)', 'Spawner change (per km)', 
                                        'Rank (spawners/km)', 'Percent change','Rank (percent change)',
                                        'Productivity (Pn) change','Rank (Pn change)')


