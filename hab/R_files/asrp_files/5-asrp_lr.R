#### 2.b. Large River and Backwater ----
asrp_bw <- all_habs_scenario %>%
  filter(Habitat == "Backwater") %>%
  select(Subbasin_num, noaaid, species, spawn_dist, both_chk, Reach, Period, can_ang, sc_mult, Shape_Length, bw_mult, 
         Habitat, Reach_low, slope.class, Scenario_num, year, Area_ha, chino_mult) %>%
  left_join(., asrp_reach_data) %>%
  mutate(Area_ha = ifelse(Period == "Hist",
                          ifelse(Floodplain == "y" & managed_forest == "n",
                                 Area_ha * rest_perc * fp_intensity_scalar,
                                 0),
                          Area_ha),
         area_s = Area_ha,
         area_w = Area_ha)


asrp_lr <- all_habs_scenario %>%
  select(Subbasin_num, noaaid, species, spawn_dist, both_chk, Reach, pass_tot, pass_tot_natural, Period, can_ang, sc_mult, Shape_Length, bw_mult,
         Habitat, Reach_low, slope.class, Scenario_num, year, Area_ha, width_s, width_s_2040, width_s_2080, width_w,
         width_w_2040, width_w_2080, Length_m, chino_mult) %>%
filter(Habitat %in% LgRiver_habs,
       !Habitat == "Backwater",
       Period %in% c("Curr", "Both")) %>%
  left_join(., asrp_reach_data) %>%
  rename(width_s_2019 = width_s,
         width_w_2019 = width_w) %>%
  mutate(
    width_s = case_when(
      year == 2019 ~ width_s_2019,
      year == 2040 ~ width_s_2040,
      year == 2080 ~ width_s_2080),
    width_w = case_when(
      year == 2019 ~ width_w_2019,
      year == 2040 ~ width_w_2040,
      year == 2080 ~ width_w_2080),
    area_s = Length_m * width_s / 10000,
    area_w = Length_m * width_w / 10000) %>%
  bind_rows(., asrp_bw) %>%
  left_join(., asrp_culvs) %>%
  mutate(
    tempmult.asrp = ifelse(species %in% c("spring_chinook", "fall_chinook"), # Added because of spring chinook w/temp survival 
                           1,
                           tempmult.asrp),
    summer.area = area_s * tempmult.asrp * woodmult_s_asrp,
    winter.area = area_w * woodmult_w_asrp) %>%
  gather(life.stage, Area, summer.area:winter.area) %>%
  mutate(life.stage = ifelse(life.stage == "summer.area",
                             "summer",
                             "winter"),
         Area = ifelse(pass_tot_asrp == 0,
                       0,
                       Area)) %>%
  select(Subbasin_num, noaaid, Habitat, GSU, pass_tot_asrp, woodmult_s_asrp, woodmult_w_asrp, tempmult.asrp, life.stage, Area, 
         rest_perc, both_chk, Scenario_num, year, LW, Floodplain, Beaver, Riparian, Barriers, wood_intensity_scalar, chino_mult)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  asrp_lr %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * chino_mult,
                         Area_nochino))
} 

asrp_lr_mvmt <- asrp_lr %>%
  filter(!Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", "Current_asrp", 'dev_and_climate'))

# asrp_lr %<>%
#   filter(Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", "Current_asrp", 'dev_and_climate'))