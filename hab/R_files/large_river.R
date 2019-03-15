LgRiver_raw = list.files(path = Inputs, pattern = "LgRiver", full.names = T) %>% read.csv(.) %>%
  mutate(source_hab = "LgRiver",
         Habitat = ifelse(HabUnit == "Bank" | HabUnit == "Bank-TM", 
                          "Bank",
                          ifelse(HabUnit == "Bank HM" | HabUnit == "Bank HM-TM", 
                                 "HM_Bank",
                                 ifelse(HabUnit == "Bar-boulder" | HabUnit == "Bar-boulder-TM", 
                                        "Bar_boulder",
                                        ifelse(HabUnit == "Bar-gravel" | HabUnit == "Bar-gravel-TM", 
                                               "Bar_gravel",
                                               ifelse(HabUnit == "Bar-sand" | HabUnit == "Bar-sand-TM", 
                                                      "Bar_sand", 
                                                      "error"))))),
         Period = ifelse(Period == " Hist", 
                         "Hist", 
                         as.character(Period))) %>%
  left_join(., flowline %>%
              select(noaaid, curr_temp, hist_temp, temp_diff, curr.tempmult, hist.tempmult, species, spawn_dist, both_chk, Subbasin_num, pass_tot, 
                     Area_km2, pass_tot_natural, tm_2040, tm_2080,tm_2040_cc_only, tm_2080_cc_only, can_ang), by = "noaaid")

if (fishtype %in% c("coho", "fall_chinook", "steelhead")) {
  LgRiver_raw = LgRiver_raw %>%
    filter(spawn_dist == "Yes" | Subbasin_num == 63)}

if (fishtype == "spring_chinook") {
  LgRiver_raw = LgRiver_raw %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
           Subbasin_num %in% schino_subs)}

source("hab/R_files/wood_script.R")

lr <- LgRiver_raw %>%
  select(Length_m, Unit_width, Area_ha, Period, Subbasin_num, noaaid, curr_temp, hist_temp, pass_tot, species, spawn_dist, both_chk, Habitat, Reach, 
         Wtrbdy_wau, Area_km2, curr.tempmult, hist.tempmult, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang)

assign('asrp_lr_raw', lr , envir = .GlobalEnv)

lr_curr_scenarios <- c("Current", "Beaver", "Fine_sediment", "Floodplain", "Barriers")

lr_1 <- lapply(lr_curr_scenarios, function(x){
  lr %>%
    filter(Period %in% c("Curr", "Both")) %>%
    mutate(hab.scenario = x,
           summer.area = Area_ha * curr.tempmult,
           winter.area = Area_ha) %>%
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
                     summer.area = Area_ha * curr.tempmult,
                     winter.area = Area_ha) %>%
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
                     summer.area = (Length_m * lr_mult * Unit_width) / 10000 * curr.tempmult,
                     winter.area = (Length_m * lr_mult * Unit_width) / 10000) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              left_join(., wood_data) %>%
              mutate(hab.scenario = "Wood",
                     summer.area = Area_ha * curr.tempmult * woodmult_s,
                     winter.area = Area_ha * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              left_join(., wood_data) %>%
              mutate(hab.scenario = "FP_wood_comb",
                     summer.area = Area_ha * curr.tempmult * woodmult_s,
                     winter.area = Area_ha * woodmult_w) %>%
              gather(life.stage, Area, summer.area:winter.area) %>%
              mutate(life.stage = ifelse(life.stage == "summer.area", 
                                         "summer", 
                                         "winter"))) %>%
  bind_rows(., lr %>%
              filter(Period %in% c("Curr", "Both")) %>%
              mutate(hab.scenario = "Shade",
                     summer.area = Area_ha * hist.tempmult,
                     winter.area = Area_ha) %>%
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
                     summer.area = (Length_m * lr_mult * Unit_width) / 10000 * hist.tempmult * woodmult_s,
                     winter.area = (Length_m * lr_mult * Unit_width) / 10000 * woodmult_w) %>%
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
