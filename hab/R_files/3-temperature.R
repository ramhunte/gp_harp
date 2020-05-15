
# Thermalscape_temps ----
thermalscape_temps <- read.csv('hab/Inputs/temperature_inputs/thermalscape_temps.csv', header = TRUE, sep = ',')

if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
  thermalscape_temps <- thermalscape_temps %>%
    mutate(
      rearing_temp_thermal = mdm_rear_thermal,
      prespawn_temp_thermal = mwmt_thermal)
} else {
  thermalscape_temps <- thermalscape_temps %>%
    mutate(
      rearing_temp_thermal = mwmt_thermal,
      prespawn_temp_thermal = mwmt_thermal)
}


# Create temperature data frame and fill data gaps ----

# Create single temperature dataframe by combining thermalscape_temps and psu_temps dataframes.  Where psu temperature data exist, we use the psu temperatures.
# Elsewhere we use thermalscape temperatures.  


all_temps <- flowline %>%
  select(noaaid, Reach, Seg, Habitat) %>%
  left_join(., thermalscape_temps, by = 'Reach') %>%
  mutate(
    rear_temp = rearing_temp_thermal,
    prespawn_temp = prespawn_temp_thermal
    ) %>%
  gather(type, temp, c(rear_temp, prespawn_temp)) %>%
  spread(type, temp) %>%
  select(noaaid, prespawn_temp, rear_temp)
  
rm(thermalscape_temps, psu_temps, psu_temps_mean_daily_max_june, psu_temps_mwmt)
