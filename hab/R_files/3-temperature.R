
# Thermalscape_temps ----
thermalscape_temps <- read.csv('hab/Inputs/temperature_inputs/thermalscape_temps.csv', header = TRUE, sep = ',')

if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
  thermalscape_temps <- thermalscape_temps %>%
    rename(
      rear_temp = mdm_rear_thermal_curr,
      rear_temp_mid = mdm_rear_thermal_mid,
      rear_temp_late = mdm_rear_thermal_late,
      prespawn_temp = mwmt_thermal_curr,
      prespawn_temp_mid = mwmt_thermal_mid,
      prespawn_temp_late = mwmt_thermal_late)
} else {
  thermalscape_temps <- thermalscape_temps %>%
    rename(
      rear_temp = mwmt_thermal_curr,
      rear_temp_mid = mwmt_thermal_mid,
      rear_temp_late = mwmt_thermal_late) %>%
    mutate(prespawn_temp = rear_temp,
           prespawn_temp_mid = rear_temp_mid,
           prespawn_temp_late = rear_temp_late)
}


# Create temperature data frame and fill data gaps ----

# Create single temperature dataframe by combining thermalscape_temps and psu_temps dataframes.  Where psu temperature data exist, we use the psu temperatures.
# Elsewhere we use thermalscape temperatures.  


all_temps <- flowline %>%
  select(noaaid, Reach, Seg, Habitat) %>%
  left_join(., thermalscape_temps, by = 'Reach') %>%
  # gather(type, temp, 6:17) %>%
  # spread(type, temp) %>%
  select(noaaid, rear_temp, rear_temp_mid, rear_temp_late, prespawn_temp, prespawn_temp_mid, prespawn_temp_late)
  
rm(thermalscape_temps)
