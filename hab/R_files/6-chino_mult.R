# This script calculates the percent chinook multipliers for spring and fall chinook using escapement data from wdfw

# Calculate escapement by subbasin for spring and fall chinook
chinook_mult <- read.csv('lcm/data/wdfw_observed_by_trib/WDFW_escapement_by_trib.csv') %>%
  left_join(read.csv('lcm/data/wdfw_observed_by_trib/WDFW_reach_name_conversion.csv')) %>%
  gather(year, spawners, X1991:X2013) %>%
  rename(Subbasin = NOAA_Reach) %>%
  mutate(Subbasin = ifelse(Subbasin %in% c("", NA),
                              as.character(WDFW_Reach),
                              as.character(Subbasin))) %>%
  group_by(Subbasin, species) %>%
  summarize(spawners = median(spawners, na.rm = T)) %>%
  spread(species, spawners) 

chinook_mult[is.na(chinook_mult)] <- 0

# Calculate chinook multiplier as percent spring/percent fall of total chinook population
chinook_mult %<>%
  mutate(perc_spr = spring_chinook / (fall_chinook + spring_chinook),
         perc_fall = fall_chinook / (fall_chinook + spring_chinook)) 

# For subbasins with no wdfw escapement data use the average of those subbasins that do have escapement data.
wdfw_mean <- chinook_mult %>%
  filter(perc_spr > 0 & perc_fall > 0) %>%
  ungroup() %>%
  summarize(mean_spr = mean(perc_spr, na.rm = T),
            mean_fall = mean(perc_fall, na.rm = T))

# For mainstem subbasins use the original .81/.19 split
chinook_mult %<>%
  full_join(., subbasin_names) %>%
  mutate(
    perc_spr = case_when(
      perc_spr > 0 & perc_spr < 1 ~ perc_spr,
      Subbasin_num %in% mainstem.subs ~ .19,
      TRUE ~ wdfw_mean$mean_spr),
    perc_fall = case_when(
      perc_fall > 0 & perc_fall < 1 ~ perc_fall,
      Subbasin_num %in% mainstem.subs ~ .81,
      TRUE ~ wdfw_mean$mean_fall)) %>%
  filter(!is.na(Subbasin_num)) %>%
  ungroup() %>%
  select(perc_spr, perc_fall, Subbasin_num)
    
