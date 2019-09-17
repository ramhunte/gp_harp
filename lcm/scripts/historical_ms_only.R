# Script to restore mainstem only
# Takes the regular runs, swaps in current for all basins except for mainstem basins

# NOTE: To run this file, first run one of the species, then click source here. The model
# will rerun, but this time each scenario will have historical conditions in the mainstem only
# E.g. Floodplain will now be current conditions in all basins except historical FP in the mainstem

path_to_hab <- file.path('outputs', fishtype, 'hab.scenarios')

# Get Current conditions for basins 1:51

hab_to_long <- function(scenario_name) {
  scenario_name %>%
    list.files(path_to_hab, pattern = ., full.names = TRUE) %>%
    read.csv %>%
    select(-X) %>%
    gather(subbasin, value, X1:X63)
}

# For now, don't do this on the ASRP scenarios
curr <- 'Current.csv' %>%
  hab_to_long()

fp <- 'Floodplain.csv' %>%
  hab_to_long()

subs <- list(paste0('X', 1:51)) %>% append(., paste0('X', 52:63))
names(subs) <- c('X0', paste0('X', 52:63))
# Swap in Current conditions for basins 1:51, leave 52:63 as is

# Replace one ms basin with historical conditions

replace_ms_sub <- function(sub_to_replace) {
  curr %>%
    filter(!subbasin %in% sub_to_replace) %>%
    bind_rows(fp %>%
                filter(subbasin %in% sub_to_replace)) %>%
    mutate(subbasin = as.numeric(str_remove(subbasin, 'X'))) %>%
    spread(subbasin, value) %>%
    write.csv(file.path(path_to_hab,
                        paste0('Floodplain_', sub_to_replace[1], '.csv')))
}


lapply(subs , replace_ms_sub)
  
 
# # Store the original model run
# 
# orig <- read.csv('outputs/coho/lcm/coho_abundance_by_subbasin.csv') #%>%
# #select(natal.basin, scenario, spawners) %>%
# # rename(spawners_orig = spawners)
# 
# source('lcm/LCM.sim.R')
# 
# y <- orig %>%
#   select(natal.basin, scenario, spawners) %>%
#   spread(scenario, spawners) %>%
#   mutate(FP_diff_orig = Floodplain - Current) %>%
#   select(natal.basin, FP_diff_orig)
# 
# 


read.csv('outputs/coho/lcm/coho_abundance_by_subbasin.csv') %>%
  filter(scenario %in% c('Current', 'Floodplain.X1')) %>%
  left_join(subbasins %>% rename(natal.basin = Subbasin)) %>%
  select(scenario, natal.basin, Subbasin_num, spawners) %>%
  spread(scenario, spawners) %>%
  mutate(diff = Floodplain.X1 - Current) %>%
  filter(!Subbasin_num %in% 52:63) %>%
  bind_rows(read.csv('outputs/coho/lcm/coho_abundance_basinwide.csv') %>%
              filter(str_detect(scenario, 'Current|Floodplain')) %>%
              mutate(curr = basinwide_spawners[scenario == 'Current'],
                     diff = basinwide_spawners - curr) %>%
              filter(str_detect(scenario, 'X5|X6')) %>%
              mutate(Subbasin_num = str_extract(scenario, "[^X]+$") %>% as.integer) %>%
              select(Subbasin_num, diff)) %>%
  select(Subbasin_num, diff)
  


