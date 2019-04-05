bw <- Backwater_raw %>%
  left_join(., flowline %>%
              select(noaaid, curr_temp, hist_temp, temp_diff, Reach, curr.tempmult, hist.tempmult, Subbasin_num, species, spawn_dist, both_chk, 
                     Area_km2, pass_tot, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang), by = "noaaid") %>%
  select(Wtrbdy_wau, Area_ha, Subbasin_num, noaaid, Area_km2, species, spawn_dist, both_chk, Reach, pass_tot, curr_temp, hist_temp, temp_diff, 
         curr.tempmult, hist.tempmult, pass_tot_natural, tm_2040, tm_2080, Period, tm_2040_cc_only, tm_2080_cc_only, can_ang) %>%
  bind_rows(., lr_length_raw %>%
              select(Reach, sc_mult) %>%
              left_join(flowline %>%
                          select(noaaid, curr_temp, hist_temp, temp_diff, Reach, curr.tempmult, hist.tempmult, Subbasin_num, species, spawn_dist, 
                                 both_chk, Area_km2, pass_tot, pass_tot_natural, tm_2040, tm_2080, Shape_Length, tm_2040_cc_only, tm_2080_cc_only, 
                                 can_ang),
                        by = "Reach") %>%
              mutate(bw_mult = sc_mult * bw_scalar,
                     Area_ha = bw_mult * (Shape_Length/1000), # bw_mult is in units of A(ha) / km.  Shape_Length is in m, so Shape_Length / 1000 * bw_mult gives unit of ha/km
                     Period = "Hist")) %>%
  mutate(Habitat = "Backwater") %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs)

if (fishtype == "spring_chinook") {
   bw <- bw %>%
     filter(Subbasin_num %in% schino_subs)}

assign('asrp_bw_raw', bw , envir = .GlobalEnv) 

curr_bw_scenarios <- c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Shade", "Wood")
hist_bw_scenarios <- c("Floodplain", "FP_wood_comb", "Historical")

bw_curr <- lapply(curr_bw_scenarios, function(x) {
  bw %>% 
    filter(Period == "Both") %>%
    mutate(hab.scenario = x)
}) %>%
  do.call('rbind',.)

bw_hist <- lapply(hist_bw_scenarios, function(y) {
  bw %>%
    filter(Period %in% c("Both", "Hist")) %>%
    mutate(hab.scenario = y)
}) %>%
  do.call('rbind',.)

bw2 <- bind_rows(bw_curr, bw_hist) %>%
  left_join(., wood_data) %>%
  mutate(
    summer = case_when(
      hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Floodplain") ~ Area_ha * curr.tempmult,
      hab.scenario == "Shade" ~ Area_ha * hist.tempmult,
      hab.scenario %in% c("Wood", "FP_wood_comb") ~ Area_ha * curr.tempmult * woodmult_s,
      hab.scenario == "Historical" ~ Area_ha * hist.tempmult * woodmult_s
    ),
    winter = case_when(
      hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Floodplain", "Shade") ~ Area_ha,
      hab.scenario %in% c("Wood", "FP_wood_comb", "Historical") ~ Area_ha * woodmult_w
    )
  ) %>%
  gather(life.stage, Area, summer:winter) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", 
                                           "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, both_chk, pass_tot_natural)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  bw2 <- bw2 %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area * chino_mult,
                         Area))
}