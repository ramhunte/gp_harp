Backwater_raw = list.files(path = Inputs, pattern = "Backwater", full.names = T) %>% 
  read.csv  %>%
  mutate(Period = "Both")

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
  mutate(Habitat = "Backwater")

if (fishtype %in% c("fall_chinook", "steelhead", "coho")) {
  bw <- bw %>%
    filter(spawn_dist == "Yes" | Subbasin_num == 63)}

if (fishtype == "spring_chinook") {
   bw = bw %>%
     filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
            Subbasin_num %in% schino_subs)}

assign('asrp_bw_raw', bw , envir = .GlobalEnv) 

bw_curr_scenarios <- c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers")

bw1 <- lapply(bw_curr_scenarios, function(x){
  bw %>%
    filter(Period == "Both") %>%
    mutate(hab.scenario = x,
           summer.area = Area_ha * curr.tempmult,
           winter.area = Area_ha) %>%
    gather(life.stage, Area, summer.area:winter.area) %>%
    mutate(life.stage = ifelse(life.stage == "summer.area", 
                               "summer", 
                               "winter"))
}) %>%
  do.call('rbind',.)

bw2 <- bw1 %>%
  bind_rows(., bw %>%
              filter(Period == "Both") %>%
              mutate(hab.scenario = "Shade",
                     summer.area = Area_ha * hist.tempmult,
                     winter.area = Area_ha) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., bw %>%
              left_join(., wood_data) %>%
              filter(Period == "Both") %>%
              mutate(hab.scenario = "Wood",
                     summer.area = Area_ha * curr.tempmult * woodmult_s,
                     winter.area = Area_ha * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., bw %>%
              filter(Period %in% c("Both", "Hist")) %>%
              mutate(hab.scenario = "Floodplain",
                     summer.area = Area_ha * curr.tempmult, #* bw_scalar_s,
                     winter.area = Area_ha) %>% #* bw_scalar_w)%>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., bw %>%
              left_join(., wood_data) %>%
              filter(Period %in% c("Both", "Hist")) %>%
              mutate(hab.scenario = "FP_wood_comb",
                     summer.area = Area_ha * curr.tempmult * woodmult_s, #* bw_scalar_s,
                     winter.area = Area_ha * woodmult_w) %>% #* bw_scalar_w)%>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., bw %>%
              filter(Period %in% c("Both", "Hist")) %>%
              left_join(wood_data) %>%
              mutate(hab.scenario = "Historical",
                     summer.area = Area_ha * hist.tempmult * woodmult_s,
                     winter.area = Area_ha * woodmult_w) %>% 
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", "winter"))) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", 
                                           "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, both_chk, pass_tot_natural)

if (fishtype == "spring_chinook") {
  bw2 <- bw2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   schino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}

if (fishtype == "fall_chinook") {
  bw2 <- bw2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   fchino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}
