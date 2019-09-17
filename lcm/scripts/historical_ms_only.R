# Script to restore mainstem only
# Takes the regular runs, swaps in current for all basins except for mainstem basins

# NOTE: To run this file, first run one of the species, then click source here. The model
# will rerun, but this time each scenario will have historical conditions in the mainstem only
# E.g. Floodplain will now be current conditions in all basins except historical FP in the mainstem

# WARNING: This will delete all habitat files currently in the directory


# Parameters to set ----

resto_scenario <- 'Shade'


############################## Should run below here ########################################


# Load packages ----

library(sf)

# Setup params ----
path_to_hab <- file.path('outputs', fishtype, 'hab.scenarios')
subs <- paste0('X', 1:63) %>% as.list


# functions ----

hab_to_long <- function(scenario_name) {
  # Make hab scenario long form, so it is easier to manipulate
  scenario_name %>%
    list.files(path_to_hab, pattern = ., full.names = TRUE) %>%
    read.csv %>%
    select(-X) %>%
    gather(subbasin, value, X1:X63)
}

replace_ms_sub <- function(sub_to_replace) {
  # Replace a Current subbasin with the column from resto_scenario
  'Current.csv' %>%
    hab_to_long() %>%
    filter(!subbasin %in% sub_to_replace) %>%
    bind_rows(resto_scenario %>%
                paste0(., '.csv') %>%
                hab_to_long() %>%
                filter(subbasin %in% sub_to_replace)) %>%
    mutate(subbasin = as.numeric(str_remove(subbasin, 'X'))) %>%
    spread(subbasin, value) %>%
    write.csv(file.path(path_to_hab,
                        paste0(resto_scenario,'_', sub_to_replace, '.csv')))
}

# Create hab scenarios with restoration in one basin ---- 
lapply(subs , replace_ms_sub)

# Run the LCM ----
source('lcm/LCM.sim.R')

# Summarize
# Difference between current and ScenarioY.X12 is the differnece made by doing ScenarioY in basin 12
df_diff <- read.csv('outputs/coho/lcm/coho_abundance_by_subbasin_raw.csv') %>%
  select(natal.basin, scenario, spawners) %>%
  group_by(natal.basin) %>%
  mutate(curr = spawners[scenario == 'Current'],
         diff = spawners - curr,
         diff_prcnt = diff/curr, 
         hist_basin = str_extract(scenario, "[^X]+$") %>% as.integer) %>%
  left_join(subbasins %>% rename(natal.basin = Subbasin)) %>%
  filter(hist_basin == Subbasin_num) %>%
  rename(noaa_sub_num = Subbasin_num) %>%
  select(noaa_sub_num, scenario, spawners, curr, diff, diff_prcnt)


# Delete extra files
list.files(path_to_hab, pattern = ".X", full.names = TRUE) %>% unlink

# # Create shapefile
# gdb_in <- '//nwcfile/FE/watershed/Chehalis/Data/10-Habitat data layers/SpatialModel_Archive/20190604/Inputs.gdb'
# 
# st_read(dsn = gdb_in, layer = 'NOAA_subbasins_w_fp') %>%
#   left_join(df_diff) %>%
#   st_write('../maps/Floodplain_resto_benefit.shp')

  


