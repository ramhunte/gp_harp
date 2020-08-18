# This script calculates the wood impact on large rivers. 
# Wood changes capacity, which we reflect here by changing area

LgRiver_raw_wood <- LgRiver_raw %>%
  left_join(., flowline %>%
              select(noaaid, curr_temp, hist_temp, species, spawn_dist, both_chk, 
                     Subbasin_num, pass_tot, 
                     Area_km2, pass_tot_natural, tm_2040, tm_2080,tm_2040_cc_only, tm_2080_cc_only, can_ang, Reach_low, width_s, width_w, 
                     width_s_hist, width_w_hist, width_s_2040, width_s_2080, width_w_2040, width_w_2080, chino_mult),
            by = "noaaid") %>% 
  gather(value, width, width_s:width_w_2080) %>%
  filter(width < 200) %>% # remove the wynoochee reservoir
  mutate(width_tot = width,
         width = case_when(Habitat %in% c("Bar_boulder", "Bar_gravel", "Bar_sand") ~ 0.087 * width_tot + 2.11,
                           Habitat == "Bank" ~ 0.084 * width_tot + 0.33,
                           Habitat == "HM_Bank" ~ 0.089 * width_tot + .33))
LgRiver_wood <- LgRiver_raw_wood %>%
  bind_rows(., LgRiver_raw_wood %>%
              mutate(center = 'center') %>%
              unite(Habitat, Habitat, center) %>%
              mutate(width = (width_tot/2) - width,
                     width = ifelse(width < 0, # a small number of reaches where edge width is greater than wetted width.  For now, we set the mid-channel width for 
                                    0,         # these to 0
                                    width))) %>%
  select(-width_tot) %>%
  spread(value, width) %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs)

assign('asrp_lr_raw', LgRiver_wood, envir = .GlobalEnv)

# Calculate wood data
wood_data <- LgRiver_wood %>%
  select(Subbasin_num, Habitat, Length_m, noaaid) %>%
  filter(!Habitat %in% c("Bank_center", "HM_Bank_center", "Bar_boulder_center", "Bar_gravel_center","Bar_sand_center")) %>%
  mutate(woodhab = ifelse(Habitat == "Bank", 
                          "Bank", 
                          ifelse(Habitat %in% c("Bar_boulder", 
                                                "Bar_gravel", 
                                                "Bar_sand"), 
                                 "Bar", 
                                 "none"))) %>%
  filter(!woodhab == "none") %>%
  group_by(Subbasin_num, woodhab) %>%
  summarize(length = sum(Length_m)) %>%
  spread(woodhab, length) %>%
  full_join(subbasin_names) %>%
  mutate(woodmult_s = ((Bank * lr_wd_s_bank) + (Bar * 2 * lr_wd_s_bar)) / (Bank + (Bar * 2)),
         woodmult_w = ((Bank * lr_wd_w_bank) + (Bar * 2 * lr_wd_w_bar))/(Bank + (Bar * 2))) %>%
  select(Subbasin_num, woodmult_s, woodmult_w) %>%
  ungroup()

woodmult_w_avg <- wood_data$woodmult_w %>% mean(., na.rm = T)
woodmult_s_avg <- wood_data$woodmult_s %>% mean(., na.rm = T)

wood_data %<>%
  mutate(woodmult_s = ifelse(is.na(woodmult_s),
                             woodmult_s_avg,
                             woodmult_s),
         woodmult_w = ifelse(is.na(woodmult_w),
                             woodmult_w_avg,
                             woodmult_w))
if(fishtype == 'chum') {
  wood_data %<>% 
    mutate(woodmult_s = 1,
           woodmult_w = 1)
} 

rm(LgRiver_raw_wood)
rm(LgRiver_wood)
