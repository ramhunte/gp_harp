
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


# PSU temperatures ----

# Calculate mean weekly maximum temperature to be used in rearing life stages ----

psu_temps_mwmt <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  mutate(species = fishtype) %>%
  filter(month == 8,
         Temperature > 0) %>%
  group_by(year,reach_pt) %>%
  mutate(mwmt = zoo::rollmean(Temperature, 7, na.pad = TRUE, align = "right")) %>%
  group_by(year,reach_pt) %>%
  summarize(mwmt = max(mwmt, na.rm = T)) %>%
  group_by(reach_pt) %>%
  summarize(mwmt = max(mwmt, na.rm = T)) %>%
  mutate(prespawn_temp_psu = mwmt)

# June 1-21 mean daily maximum used for chinook downstream migration rearing
psu_temps_mean_daily_max_june <- read.csv("hab/Inputs/temperature_inputs/PSU_Modeled_Temperatures_current.csv") %>%
  separate(col = JDAY, c("month", "day"), sep = "/") %>%
  gather(reach_pt, Temperature, X1:X322) %>%
  filter(month == 6,
         day %in% 1:21,
         Temperature > 0) %>%
  group_by(reach_pt) %>%
  summarize(mdm_june = mean(Temperature, na.rm = T))


psu_temps <- psu_temps_mwmt %>%
  left_join(., psu_temps_mean_daily_max_june, by = "reach_pt") %>%
  mutate(Seg = as.numeric(sub('.', '', reach_pt))) %>%
  select(-reach_pt) %>%
  mutate(
    rearing_temp_psu = case_when(
      fishtype %in% c('spring_chinook', 'fall_chinook') ~ mdm_june,
      fishtype %in% c('coho', 'steelhead') ~ mwmt),
    prespawn_temp_psu  = mwmt)

# write.csv(psu_lyr, 'hab/Inputs/psu_temps.csv')

# Create temperature data frame and fill data gaps ----

# Create single temperature dataframe by combining thermalscape_temps and psu_temps dataframes.  Where psu temperature data exist, we use the psu temperatures.
# Elsewhere we use thermalscape temperatures.  


all_temps <- flowline %>%
  select(noaaid, Reach, Seg, Habitat) %>%
  left_join(., thermalscape_temps, by = 'Reach') %>%
  left_join(., psu_temps, by = 'Seg') %>%
  mutate(
    rearing_temp_psu = ifelse(Habitat == 'LgRiver',
                              rearing_temp_psu,
                              NA),
    prespawn_temp_psu = ifelse(Habitat == 'LgRiver',
                               prespawn_temp_psu,
                               NA),
    rear_temp = rearing_temp_thermal,
      # case_when(
      # !is.na(rearing_temp_psu) ~ rearing_temp_psu,
      # is.na(rearing_temp_psu) & !is.na(rearing_temp_thermal) ~ rearing_temp_thermal),
    prespawn_temp = prespawn_temp_thermal
      # case_when(
      # !is.na(prespawn_temp_psu) ~ prespawn_temp_psu,
      # is.na(prespawn_temp_psu) & !is.na(prespawn_temp_thermal) ~ prespawn_temp_thermal)
    ) %>%
  gather(type, temp, c(rear_temp, prespawn_temp)) %>%
  spread(type, temp) %>%
  select(noaaid, prespawn_temp, rear_temp)
  
rm(thermalscape_temps, psu_temps, psu_temps_mean_daily_max_june, psu_temps_mwmt)
