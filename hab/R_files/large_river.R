LgRiver_raw = list.files(path = Inputs, pattern = "LgRiver", full.names = T) %>% read.csv(.) %>%
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
                     Area_km2, pass_tot_natural, tm_2040, tm_2080,tm_2040_cc_only, tm_2080_cc_only, can_ang, Reach_low, width_s, width_w, width_s_hist, width_w_hist),
            by = "noaaid") %>% 
  gather(value, width, c(width_s, width_w, width_s_hist, width_w_hist)) %>% 
  mutate(width = case_when(Habitat %in% c("Bar_boulder", "Bar_gravel", "Bar_sand") ~ 0.087 * width + 2.11,
                             Habitat == "Bank" ~ 0.084 * width + 0.33,
                             Habitat == "HM_Bank" ~ 0.089 * width + .33)) %>%
  spread(value, width) %>%
  mutate(area_s = width_s * Length_m / 10000,
         area_w = width_w * Length_m / 10000) %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs)

if (fishtype == "spring_chinook") {
  LgRiver_raw <- LgRiver_raw %>%
    filter(Subbasin_num %in% schino_subs)}

source("hab/R_files/wood_script.R")

lr <- LgRiver_raw %>%
  select(Length_m, Period, Subbasin_num, noaaid, curr_temp, hist_temp, pass_tot, species, spawn_dist, both_chk, Habitat, Reach, 
         Wtrbdy_wau, Area_km2, curr.tempmult, hist.tempmult, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang, area_s, 
         area_w, Reach_low, width_s, width_w, width_s_hist, width_w_hist)

assign('asrp_lr_raw', lr , envir = .GlobalEnv)

lr_curr_scenarios <- c("Current", "Beaver", "Fine_sediment", "Floodplain", "Barriers")

lr_1 <- lapply(lr_curr_scenarios, function(x){
  lr %>%
    filter(Period %in% c("Curr", "Both")) %>%
    mutate(hab.scenario = x,
           summer.area = area_s * curr.tempmult,
           winter.area = area_w ) %>%
    gather(life.stage, Area, summer.area:winter.area) %>%
    mutate(life.stage = ifelse(life.stage == "summer.area", 
                               "summer", 
                               "winter"))
}) %>%
  do.call('rbind',.)

lr_2 <- lr_1 %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Both", "Hist")) %>%
              mutate(hab.scenario = "LR_bank",
                     summer.area = area_s * curr.tempmult,
                     winter.area = area_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              left_join(., lr_length_raw) %>%
              mutate(lr_mult = ifelse(is.na(lr_mult), 
                                      1, 
                                      lr_mult),
                     hab.scenario = "LR_length",
                     summer.area = (Length_m * lr_mult * width_s) / 10000 * curr.tempmult,
                     winter.area = (Length_m * lr_mult * width_w ) / 10000) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              left_join(., wood_data) %>%
              mutate(hab.scenario = "Wood",
                     summer.area = area_s * curr.tempmult * woodmult_s,
                     winter.area = area_w * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              left_join(., wood_data) %>%
              mutate(hab.scenario = "FP_wood_comb",
                     summer.area = area_s * curr.tempmult * woodmult_s,
                     winter.area = area_w * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              mutate(hab.scenario = "Shade",
                     summer.area = area_s * hist.tempmult,
                     winter.area = area_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Hist", "Both")) %>%
              left_join(., wood_data) %>%
              left_join(., lr_length_raw) %>%
              mutate(lr_mult = ifelse(is.na(lr_mult), 
                                      1, 
                                      lr_mult),
                     hab.scenario = "Historical",
                     summer.area = (Length_m * lr_mult * width_s_hist) / 10000 * hist.tempmult * woodmult_s,
                     winter.area = (Length_m * lr_mult * width_w_hist) / 10000 * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, both_chk, pass_tot_natural)


if (fishtype == "spring_chinook") {
  lr_2 <- lr_2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   schino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}

if (fishtype == "fall_chinook") {
  lr_2 <- lr_2 %>%
    mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs, 
                                   fchino_mult, 
                                   1),
           Area = Area * chinook_scalar)
}
