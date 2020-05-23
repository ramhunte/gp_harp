

# To create spawners_workbook.xlxs source this script



###################################

library(openxlsx)
library(tidyverse)





# Species to run ----
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')


# Subbasin names and numbers ----
subbasin_names <- read.csv('lcm/data/Subbasin_names.csv') %>%
  mutate(EcoRegion = ifelse(str_detect(Subbasin, 'Estuary'),
                            'Estuary', 
                            as.character(EcoRegion)))



# Load plot colors and labels ----
plot.params <- read.csv('lcm/data/scenarios.csv') %>% 
  select(scenario, scenario.label, color) %>%
  mutate_if(is.factor, as.character) %>%
  rowid_to_column()





# Calculate total anadroumous length by EDR and subbasin ----

flowline_anadromous <- list.files(path = 'hab/Inputs/spatial_model_outputs/', 
                                  pattern = "flowline", 
                                  full.names = T) %>%
  read.csv %>%
  filter(
    (cohospawn == 'Yes' | fallspawn == 'Yes' | chumspawn == 'Yes' |
     sprspawn == 'Yes'  | steelspawn == 'Yes')
    ) %>%
  mutate(length_km = Shape_Length / 1000) %>%
  rename(Subbasin_num = noaa_sub_num) %>%
  select(Subbasin_num, noaaid, length_km) %>% 
  left_join(subbasin_names, by = 'Subbasin_num')


# Final length calcs
flowline_edr <- flowline_anadromous %>%
  group_by(EcoRegion) %>%
  summarise(length_km = sum(length_km, na.rm = T))

flowline_subbasin <- flowline_anadromous %>%
  group_by(Subbasin, Subbasin_num) %>%
  summarise(length_km = sum(length_km, na.rm = T))










# Set up groups of scenarios ----
scenarios <- plot.params$scenario

run_scenarios_diag <- str_subset(scenarios, 'ASRP|Hist|Current|FP', negate = TRUE)


run_scenarios_asrp <- c(paste('ASRP.scenario', 1:3, c(2040, 2080), sep = '.'),
                        #paste0('Historical.', c(2040, 2080)),
                        paste0('ASRP.dev.and.climate.', c(2040, 2080)))






# Create helper functions ----

extract_species_from_path <- . %>% 
  dirname %>% 
  dirname %>% 
  basename %>% 
  str_replace('_', ' ') %>% 
  str_to_title




# Main function to load data and rank 
create_ranks_df <- function(filter_scenarios, file_path_list) {
  
  # Read in all files and rbind them together
  file_path_spp <- extract_species_from_path(file_path_list)
  
  df <- map_dfr(file_path_list, read.csv, .id = 'species') %>%
    mutate(species = file_path_spp[as.numeric(species)])
  
  
  # Specifiy EDR or subbasin scale
  if (any(str_detect(colnames(df), 'EcoRegion'))) {
    
    spatial_scale <- as.name('EcoRegion')
    f <- flowline_edr
    df <- rename(df, spawners = basinwide_spawners)
    select_cols <- spatial_scale
    
  } else {
    
    spatial_scale <- as.name('Subbasin')
    f <- flowline_subbasin
    df <- df %>%
      rename(Subbasin = natal.basin) %>%
      mutate(Subbasin = str_remove(Subbasin, 'Unnamed: '))
    
    select_cols <- c(spatial_scale, 'Subbasin_num', 'EcoRegion')
  }
  
  # Calculate the ranking fields
  df_ranked <- df %>%
    select(species, !!spatial_scale, scenario, spawners, Pn) %>%
    group_by(species, !!spatial_scale) %>%
    mutate(Current_spawners = spawners[scenario == 'Current'],
           restoration_potential = spawners - Current_spawners,
           Current_pn = Pn[scenario == 'Current'],
           restoration_potential_pn = round(Pn - Current_pn, 2)) %>%
    group_by(species) %>%
    mutate(spawners_basinwide = sum(Current_spawners[scenario == 'Current'])) %>%
    filter(scenario %in% filter_scenarios) %>%
    ungroup() %>%
    left_join(f) %>%
    mutate(length_km = round(length_km, digits = 2),
           perc_of_pop = round(restoration_potential / spawners_basinwide * 100, digits = 2),
           spawners_per_km = round(restoration_potential / length_km, digits = 1)) %>%
    group_by(species) %>%
    mutate(rank_rest_potential = rank(-restoration_potential, ties.method = 'min'),
           rank_spawner_per_km = rank(-spawners_per_km, ties.method = 'min'),
           rank_rest_potential_pn = rank(-restoration_potential_pn, ties.method = 'min')) %>%
    ungroup() %>%
    mutate(rank_percent = rank(-perc_of_pop, ties.method = 'min')) %>%
    left_join(subbasin_names) %>%
    select(species, !!spatial_scale, contains('Subbasin_num'), contains('EcoRegion'), length_km, scenario, Current_spawners, spawners, restoration_potential,
           rank_rest_potential, spawners_per_km, rank_spawner_per_km, perc_of_pop, rank_percent,
           restoration_potential_pn, rank_rest_potential_pn) %>%
    arrange(species, rank_rest_potential)
  
  
  namekey <- c(species = 'Species', EcoRegion = 'EcoRegion', Subbasin = 'Subbasin', Subbasin_num = 'Subbasin number', 
               length_km = 'Total Km', scenario = 'Scenario', Current_spawners = 'Current spawners', 
               spawners = 'Diagnostic scenario spawners', restoration_potential = 'Restoration potential (spawners)',
               rank_rest_potential = 'Rank (spawners)', spawners_per_km = 'Spawner change (per km)',
               rank_spawner_per_km = 'Rank (spawners/km)', perc_of_pop = 'Percent change', 
               rank_percent = 'Rank (percent change)', restoration_potential_pn = 'Productivity (Pn) change', 
               rank_rest_potential_pn = 'Rank (Pn change)')
  
  
  names(df_ranked) <- namekey[names(df_ranked)]
  
  
  return(df_ranked)
}





cleanup_hist_tab <- . %>% 
  select(-Scenario) %>% 
  rename(`Historical spawners` = `Diagnostic scenario spawners`)






cleanup_asrp_tab <- . %>%
  mutate(Climate = sub('.*\\.', '', Scenario) %>% as.integer(),
         Scenario = str_extract(Scenario,'.*\\.') %>%
           str_replace_all('\\.', ' ') %>%
           trimws(),
         Scenario = ifelse(Scenario == 'ASRP dev and climate', 'No action', Scenario),
         Scenario = ifelse(Scenario == 'Historical', 'Natural Potential', Scenario)) %>%
  select(Species:Scenario, Climate, `Current spawners`:`Rank (Pn change)`)








# Create each tab for the Excel workbook ----

# Define file paths
paths_to_edrs <- list.files('srt_figures/outputs_v13.1_w_Hist_future/', 
                            pattern = '_abundance_by_EDR.csv',
                            recursive = TRUE,
                            full.names = TRUE) %>%
  str_subset('chum', negate = TRUE)



paths_to_subs <- list.files('srt_figures/outputs_v13.1_w_Hist_future/', 
                            pattern = '_abundance_by_subbasin.csv',
                            recursive = TRUE,
                            full.names = TRUE) %>%
  str_subset('chum', negate = TRUE)







# Create EDR scale tabs ----


# tab - Rank Current and Historical EDR 
spawners_edr_hist <- create_ranks_df('Historical', paths_to_edrs) %>% 
  cleanup_hist_tab()

# tab - Rank diagnostic
spawners_edr_diag <- create_ranks_df(run_scenarios_diag, paths_to_edrs)

# tab - Rank ASRP
spawners_edr_asrp <- create_ranks_df(run_scenarios_asrp, paths_to_edrs) %>%
  cleanup_asrp_tab







# Create subbasin scale tabs ----

# tab - Rank Current and Historical EDR 
spawners_sub_hist <- create_ranks_df('Historical', paths_to_subs) %>% 
  cleanup_hist_tab()

# tab - Rank diagnostic
spawners_sub_diag <- create_ranks_df(run_scenarios_diag, paths_to_subs)

# tab - Rank ASRP
spawners_sub_asrp <- create_ranks_df(run_scenarios_asrp, paths_to_subs) %>%
  cleanup_asrp_tab






# Write tabs to workbook ----
wb <- loadWorkbook('srt_figures/spawners_template.xlsx')

writeData(wb, sheet = 1, spawners_edr_hist)
writeData(wb, sheet = 2, spawners_edr_diag)
writeData(wb, sheet = 3, spawners_edr_asrp)
writeData(wb, sheet = 4, spawners_sub_hist)
writeData(wb, sheet = 5, spawners_sub_diag)
writeData(wb, sheet = 6, spawners_sub_asrp)



saveWorkbook(wb, 'srt_figures/spawners_workbook.xlsx', overwrite = TRUE)
