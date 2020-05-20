
# Gather up data ----

spawners_edr_df <- lapply(spp, function(t) {
  
  list.files('srt_figures/outputs_v13.1_w_Hist_future/', 
             pattern = '_abundance_by_EDR.csv',
             recursive = TRUE,
             full.names = TRUE) %>%
    str_subset(t) %>%
    read.csv %>%
    mutate(species = Hmisc::capitalize(str_replace(t, '_', ' '))) %>%
    select(species,scenario, EcoRegion, basinwide_spawners, Pn) %>%
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


# Function to filter to arbirary scenarios then rank ----

create_edr_ranks_df <- function(filter_scenarios) {
  
  spawners_edr_scenarios <- spawners_edr_df %>%
    rename(spawners = basinwide_spawners) %>%
    filter(scenario %in% filter_scenarios) %>%
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
  
  colnames(spawners_edr_scenarios) <- c('Species', 'EcoRegion', 'Total Km', 'Diagnostic scenario',
                                          'Current spawners', 'Diagnostic scenario spawners',
                                          'Restoration potential (spawners)',
                                          'Rank (spawners)', 'Spawner change (per km)',
                                          'Rank (spawners/km)', 'Percent change','Rank (percent change)',
                                          'Productivity (Pn) change','Rank (Pn change)')
  
  return(spawners_edr_scenarios)
}



#### Rank with Diagnostic scenarios ----

run_scenarios_diag <- diag_scenarios[-grep('Historical|Current', diag_scenarios)] %>% str_replace('_', '.')
       
spawners_edr_diagnostics <- create_edr_ranks_df(run_scenarios_diag) 



#### Rank with ASRP scenarios ----

run_scenarios_asrp <- str_subset(scenarios$scenario, 'ASRP')

spawners_edr_asrp <- create_edr_ranks_df(run_scenarios_asrp) 

