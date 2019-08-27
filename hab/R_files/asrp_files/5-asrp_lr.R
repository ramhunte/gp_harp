#### 2.b. Large River and Backwater ----
asrp_bw_raw <- Backwater_raw %>%
  left_join(., flowline %>%
              select(noaaid, Reach, Subbasin_num, species, spawn_dist, both_chk, chino_mult, Reach_low)) %>%
  select(Area_ha, Subbasin_num, noaaid, species, spawn_dist, both_chk, Reach, Period, chino_mult, Reach_low) %>%
  bind_rows(., lr_length_raw %>%
              select(Reach, sc_mult) %>%
              left_join(flowline %>%
                          select(noaaid, Reach, Subbasin_num, species, spawn_dist, both_chk, Shape_Length, Reach_low)) %>%
              mutate(bw_mult = sc_mult * bw_scalar,
                     Area_ha = bw_mult * (Shape_Length/1000), # bw_mult is in units of A(ha) / km.  Shape_Length is in m, so Shape_Length / 1000 * bw_mult gives unit of ha/km
                     Period = "Hist")) %>%
  mutate(Habitat = "Backwater") %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
  select(Subbasin_num, noaaid, species, spawn_dist, both_chk, Reach, Period, sc_mult, Shape_Length, bw_mult, 
         Habitat, Reach_low, Area_ha, chino_mult)

asrp_bw_year <- lapply(scenario.years, function(x) {
  asrp_bw_raw %>% 
    mutate(year = x)
}) %>%
  do.call('rbind',.)

asrp_bw <- lapply(scenario.nums, function(y) {
  asrp_bw_year %>%
    mutate(Scenario_num = y)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios)),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], 'floodplain_hist') &
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_reach_data) %>%
  mutate(Area_ha = ifelse(Period == "Hist",
                          ifelse(Floodplain == "y",
                                 Area_ha * rest_perc * fp_intensity_scalar,
                                 0),
                          Area_ha),
         area_s = Area_ha,
         area_w = Area_ha)


asrp_lr_raw <- LgRiver_raw %>%
  mutate(Habitat = case_when(HabUnit %in% c("Bank", "Bank-TM") ~ "Bank",
                             HabUnit %in% c("Bank HM", "Bank HM-TM") ~ "HM_Bank",
                             HabUnit %in% c("Bar-boulder", "Bar-boulder-TM") ~ "Bar_boulder",
                             HabUnit %in% c("Bar-gravel", "Bar-gravel-TM") ~ "Bar_gravel",
                             HabUnit %in% c("Bar-sand", "Bar-sand-TM") ~ "Bar_sand"),
         Period = ifelse(Period == " Hist", 
                         "Hist", 
                         as.character(Period))) %>%
  left_join(., flowline %>%
              select(Subbasin_num, noaaid, species, spawn_dist, both_chk, Reach_low, width_s, width_s_2040, width_s_2080, width_w,
                     width_w_2040, width_w_2080, chino_mult)) %>% 
  gather(value, width, width_s:width_w_2080) %>% 
  filter(width < 200) %>%
  mutate(width = case_when(Habitat %in% c("Bar_boulder", "Bar_gravel", "Bar_sand") ~ 0.087 * width + 2.11,
                           Habitat == "Bank" ~ 0.084 * width + 0.33,
                           Habitat == "HM_Bank" ~ 0.089 * width + .33)) %>%
  spread(value, width) %>%
  mutate(area_s = width_s * Length_m / 10000,
         area_w = width_w * Length_m / 10000) %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs)

asrp_lr_year <- lapply(scenario.years, function(z) {
  asrp_lr_raw %>%
    mutate(year = z)
}) %>%
  do.call('rbind',.)

asrp_lr_scenario <- lapply(scenario.nums, function(a) {
  asrp_lr_year %>%
    mutate(Scenario_num = a)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", growth_scenarios)),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], 'floodplain_hist') &
             year %in% c(2040, 2080)))

assign('asrp_lr_spawn', asrp_lr_scenario, envir = .GlobalEnv)

asrp_lr <- asrp_lr_scenario %>%
  filter(Period %in% c("Curr", "Both")) %>%
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

if (run_single_action == 'no') {
asrp_lr %<>%
  filter(Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", "Current_asrp"))
}

rm(asrp_bw)