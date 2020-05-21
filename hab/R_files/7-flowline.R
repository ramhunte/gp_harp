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
  gather(species, spawn_dist, coho:steelhead) 

if (fishtype == 'anadromous_network') {
  flowline_noculv %<>%
    filter(spawn_dist == 'Yes') %>%
    select(-species) %>%
    distinct() %>%
    mutate(species = 'anadromous_network')
} else{
  filter(species == fishtype)
}
  
flowline_noculv %<>%
  left_join(., all_temps, by = 'noaaid') %>%
  left_join(., edt_width %>%
              gather(stage, width, width_w:width_s) %>%
              mutate(stage = case_when(year == 2019 & stage == "width_s" ~ "width_s",
                                       year == 2019 & stage == "width_w" ~ "width_w",
                                       year == 1900 & stage == "width_s" ~ "width_s_hist",
                                       year == 1900 & stage == "width_w" ~ "width_w_hist",
                                       year == 2040 & stage == "width_s" ~ "width_s_2040",
                                       year == 2040 & stage == "width_w" ~ "width_w_2040",
                                       year == 2080 & stage == "width_s" ~ "width_s_2080",
                                       year == 2080 & stage == "width_w" ~ "width_w_2080")) %>%
              select(-year) %>%
              spread(stage, width),
            by = "Reach_low") %>%
  mutate(
         width_s = ifelse(is.na(width_s),
                          wet_width,
                          width_s),
         width_w = ifelse(is.na(width_w),
                          wet_width,
                          width_w),
         width_s_hist = ifelse(is.na(width_s_hist),
                               wet_width,
                               width_s_hist),
         width_w_hist = ifelse(is.na(width_w_hist),
                               wet_width,
                               width_w_hist),
         area_s = (Shape_Length * width_s) / 10000,
         area_w = (Shape_Length * width_w) / 10000,
         temp_diff_2040_rear = ifelse(species %in% c('spring_chinook', 'fall_chinook'),
                                      mwmt_to_mdm_func(F2040_temp - curr_temp) + cc_mid_rear,
                                      F2040_temp - curr_temp + cc_mid_rear),
         temp_diff_2080_rear = ifelse(species %in% c('spring_chinook', 'fall_chinook'),
                                      mwmt_to_mdm_func(F2080_temp - curr_temp) + cc_late_rear,
                                      F2080_temp - curr_temp + cc_late_rear),
         temp_diff_rear = ifelse(species %in% c('spring_chinook', 'fall_chinook'),
                            mwmt_to_mdm_func(curr_temp - hist_temp),
                            curr_temp - hist_temp),
         temp_diff_2040_prespawn = F2040_temp - curr_temp + cc_mid_prespawn,
         temp_diff_2080_prespawn = F2080_temp - curr_temp + cc_late_prespawn,
         temp_diff_prespawn = curr_temp - hist_temp,
         curr_temp = rear_temp, 
         hist_temp = curr_temp - temp_diff_rear,
         tm_2040 = curr_temp + temp_diff_2040_rear,
         tm_2080 = curr_temp + temp_diff_2080_rear,
         tm_2040_cc_only = curr_temp + cc_mid_rear,
         tm_2080_cc_only = curr_temp + cc_late_rear,
         curr.tempmult = temp_func(curr_temp),
         hist.tempmult = temp_func(hist_temp),
         can_ang = ifelse(is.na(can_ang), # We are still working on calculating canopy opening angle in some reaches.  For reaches without canopy opening angle measurements, 
                          0, # we set canopy opening angle to 0 because the majority of these reaches are far upstream and few of them are clearcut.
                          can_ang))

flowline <- flowline_noculv %>% 
  select(noaaid,Reach,culv_list) %>%
  filter(culv_list != "") %>%
  mutate(culv_list = str_split(culv_list,',')) %>%
  unnest(culv_list) %>%
  filter(culv_list != 'None') %>%
  mutate(noaa_culv = as.numeric(culv_list)) %>%
  left_join(., culvs %>%
              select(-GSU)) %>%
  group_by(noaaid) %>%
  summarize(pass_tot = prod(FishPass),
            pass_tot_natural = prod(ifelse(FeatureTyp == 'Natural',FishPass,1))) %>%
  right_join(flowline_noculv) %>%
  replace_na(list(pass_tot = 1, pass_tot_natural = 1)) %>%
  ungroup() %>%
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

rm(flowline_noculv, all_temps, culvs)
