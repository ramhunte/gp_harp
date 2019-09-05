# This script calculates the percent chinook multipliers for spring and fall chinook using escapement data from wdfw

# Calculate escapement by subbasin for spring and fall chinook
chinook_mult <- read.csv('lcm/data/wdfw_observed_by_trib/WDFW_escapement_by_trib.csv') %>%
  gather(year, spawners, X1991:X2013) %>%
  left_join(read.csv('lcm/data/wdfw_observed_by_trib/WDFW_reach_name_conversion.csv')) %>%
  rename(natal.basin = NOAA_Reach) %>%
  mutate(source = 'wdfw',
         year = sub('X','',year),
         natal.basin = ifelse(natal.basin %in% c("", NA),
                              as.character(WDFW_Reach),
                              as.character(natal.basin))) %>%
  group_by(natal.basin, species) %>%
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
  mutate(
    perc_spr = case_when(
      perc_spr > 0 & perc_spr < 1 ~ perc_spr,
      perc_spr == 0 | perc_spr == 1 ~ wdfw_mean$mean_spr,
      is.na(perc_spr) ~ wdfw_mean$mean_spr),
    perc_fall = case_when(
      perc_fall > 0 & perc_fall < 1 ~ perc_fall,
      perc_fall == 0 | perc_fall == 1 ~ wdfw_mean$mean_fall,
      is.na(perc_fall) ~ wdfw_mean$mean_fall),
    Subbasin = natal.basin) %>%
  full_join(., subbasin_names) %>%
  filter(!is.na(Subbasin_num)) %>%
  mutate(
    perc_spr = case_when(
      is.na(perc_spr) ~
        ifelse(Subbasin_num %in% mainstem.subs,
               .19,
               wdfw_mean$mean_spr),
      !is.na(perc_spr) ~ perc_spr),
    perc_fall = case_when(
      is.na(perc_fall) ~
        ifelse(Subbasin_num %in% mainstem.subs,
               .81,
               wdfw_mean$mean_fall),
      !is.na(perc_fall) ~ perc_fall)) %>%
  ungroup() %>%
  select(perc_spr, perc_fall, Subbasin_num)
    
