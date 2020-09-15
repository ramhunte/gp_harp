flowline_noculv <- flowline %>%
  rename(coho = cohospawn,
         fall_chinook = fallspawn,
         chum = chumspawn,
         spring_chinook = sprspawn,
         steelhead = steelspawn) %>%
  mutate(both_chk = ifelse(fall_chinook == "Yes" & spring_chinook == "Yes",
                           "Yes",
                           "No"),
         Reach_low = tolower(Reach),
         Subbasin_num = ifelse(Habitat == "LgRiver", noaa_sub_num, noaa_sub_nofp_num),
         Habitat = case_when(!Reach %in% mainstem_reaches ~ as.character(Habitat),
                             Reach %in% mainstem_reaches & Subbasin_num == 49 ~ "Tidal",
                             Reach %in% mainstem_reaches & !Subbasin_num == 49 ~ "LgRiver")) %>%
  gather(species, spawn_dist, coho:steelhead) %>%
  filter(species == fishtype) %>%
  mutate(can_ang = ifelse(is.na(can_ang), # We are still working on calculating canopy opening angle in some reaches.  
                          0,              # For reaches without canopy opening angle measurements,
                          can_ang))       # we set canopy opening angle to 0 because the majority of these reaches are far upstream and few of them are clearcut.

flowline <- flowline_noculv %>% 
  left_join(., chinook_mult)
  
# Create column with chinook habitat multiplier.  This will be applied to Area in both_chk & mainstem reaches  
if (fishtype == 'fall_chinook') {
  flowline %<>% 
    mutate(chino_mult = perc_fall)
} else if (fishtype == 'spring_chinook') {
  flowline %<>% 
    mutate(chino_mult = perc_spr)
} else {
  flowline %<>% 
    mutate(chino_mult = 1)
}

