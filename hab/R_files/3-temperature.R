all_temps <- read.csv(list.files(path = file.path(Inputs, "spatial_model_outputs"), pattern = "flowline", full.names = T)) %>%
  left_join(.,read.csv('hab/Inputs/temperature_inputs/thermalscape_temps.csv', header = TRUE, sep = ',') %>%
              mutate(rear_temp = case_when(fishtype %in% c('spring_chinook', 'fall_chinook') ~ mdm_rear_thermal,
                                           TRUE ~ mwmt_thermal),
                     prespawn_temp = mwmt_thermal), by = 'Reach') %>%
  select(noaaid, prespawn_temp, rear_temp)
