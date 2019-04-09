LgRiver <- LgRiver_raw %>%
  mutate(source_hab = "LgRiver",
         Habitat = case_when(HabUnit %in% c("Bank", "Bank-TM") ~ "Bank",
                             HabUnit %in% c("Bank HM", "Bank HM-TM") ~ "HM_Bank",
                             HabUnit %in% c("Bar-boulder", "Bar-boulder-TM") ~ "Bar_boulder",
                             HabUnit %in% c("Bar-gravel", "Bar-gravel-TM") ~ "Bar_gravel",
                             HabUnit %in% c("Bar-sand", "Bar-sand-TM") ~ "Bar_sand"),
         Period = ifelse(Period == " Hist", 
                         "Hist", 
                         as.character(Period))) %>%
  left_join(., flowline %>%
              select(noaaid, curr_temp, hist_temp, temp_diff, curr.tempmult, hist.tempmult, species, spawn_dist, both_chk, Subbasin_num, pass_tot, 
                     Area_km2, pass_tot_natural, tm_2040, tm_2080,tm_2040_cc_only, tm_2080_cc_only, can_ang, Reach_low, width_s, width_w, 
                     width_s_hist, width_w_hist, width_s_2040, width_s_2080, width_w_2040, width_w_2080),
            by = "noaaid") %>% 
  gather(value, width, width_s:width_w_2080) %>% 
  mutate(width = case_when(Habitat %in% c("Bar_boulder", "Bar_gravel", "Bar_sand") ~ 0.087 * width + 2.11,
                           Habitat == "Bank" ~ 0.084 * width + 0.33,
                           Habitat == "HM_Bank" ~ 0.089 * width + .33)) %>%
  spread(value, width) %>%
  mutate(area_s = width_s * Length_m / 10000,
         area_w = width_w * Length_m / 10000) %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs)

if (fishtype == "spring_chinook") {
  LgRiver <- LgRiver %>%
    filter(Subbasin_num %in% schino_subs)
}

assign('asrp_lr_raw', LgRiver , envir = .GlobalEnv)

source("hab/R_files/wood_script.R")

lr <- LgRiver %>%
  select(Length_m, Period, Subbasin_num, noaaid, curr_temp, hist_temp, pass_tot, species, spawn_dist, both_chk, Habitat, Reach, 
         Wtrbdy_wau, Area_km2, curr.tempmult, hist.tempmult, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang, area_s, 
         area_w, Reach_low, width_s, width_w, width_s_hist, width_w_hist)

lr_curr_scenarios <- c("Current", "Beaver", "Fine_sediment", "Floodplain", "Barriers", "LR_length", "Wood", "Shade", "FP_wood_comb")
lr_hist_scenarios <- c("Historical", "LR_bank")

lr_curr <- lapply(lr_curr_scenarios, function(x) {
  lr %>% 
    filter(Period %in% c("Curr", "Both")) %>%
    mutate(hab.scenario = x)
}) %>%
  do.call('rbind',.)

lr_hist <- lapply(lr_hist_scenarios, function(y) {
  lr %>%
    filter(Period %in% c("Hist", "Both")) %>%
    mutate(hab.scenario = y)
})

lr_2 <- bind_rows(lr_curr, lr_hist) %>%
  left_join(., lr_length_raw) %>%
  left_join(., wood_data) %>%
  mutate(lr_mult = ifelse(is.na(lr_mult), 
                          1, 
                          lr_mult),
         summer.area = case_when(hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "Floodplain", "Barriers", "LR_bank") ~ area_s * curr.tempmult,
                                 hab.scenario == "LR_length" ~ (Length_m * lr_mult * width_s) / 10000 * curr.tempmult,
                                 hab.scenario %in% c("Wood", "FP_wood_comb") ~ area_s * curr.tempmult * woodmult_s,
                                 hab.scenario == "Shade" ~ area_s * hist.tempmult,
                                 hab.scenario == "Historical" ~ (Length_m * lr_mult * width_s_hist) / 10000 * hist.tempmult * woodmult_s),
         winter.area = case_when(hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "Floodplain", "Barriers", "LR_bank", "Shade") ~ area_w,
                                 hab.scenario == "LR_length" ~ (Length_m * lr_mult * width_w ) / 10000,
                                 hab.scenario %in% c("Wood", "FP_wood_comb") ~ area_w * woodmult_w,
                                 hab.scenario == "Historical" ~ (Length_m * lr_mult * width_w_hist) / 10000 * woodmult_w)) %>%
  gather(life.stage, Area, summer.area:winter.area) %>%
  mutate(life.stage = ifelse(life.stage == "summer.area", 
                             "summer", 
                             "winter")) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, both_chk, pass_tot_natural) 

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  lr_2 <- lr_2 %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area * chino_mult,
                         Area))
}
