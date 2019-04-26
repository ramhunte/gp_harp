# This script calculates the wood impact on large rivers. 
# Wood changes capacity, which we reflect here by changing area

# Clean up names
# Note, this gives a slightly different answer than it did before
# This is becuase before it was reading in LgRiver csv
# which did not have manual subbasin designations (large_river.R lines 12:14)
LgRiver_raw_wood <- LgRiver %>%
  select(
    Subbasin_num,
    # noaa_du,source_hab, Period, Area_ha, curr_temp, hist_temp, EDT_FChino:EDT_Chum, pass_tot, Area_km2, Wtrbdy_wau,Reach, Unit_width
    Habitat,  
    Length_m, 
    noaaid)

# Calculate wood data
wood_data <- LgRiver_raw_wood %>%
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
