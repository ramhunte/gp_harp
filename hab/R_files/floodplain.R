lc.ref = c(rep("Reference", times = 3))
slope.class = c("low", "med", "high")
pool.perc.ref = c(.81, .66, .35)
ss.dist.ref = data.frame(lc.ref, slope.class, pool.perc.ref)

# Create historical side channel data frame from flowline and hist channel data joined by reach
hist_sc <- flowline %>%
  select(Reach, Shape_Length, spawn_dist, species, both_chk, Subbasin_num, noaaid, curr.tempmult, hist.tempmult, curr_temp, hist_temp, tm_2040, 
         tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang) %>%
  left_join(., lr_length_raw, by = "Reach") %>%
  mutate(sc_mult = ifelse(is.na(sc_mult), 
                          0, 
                          sc_mult),
         Area_ha = (Shape_Length * sc_mult * 2) / 10000, # Assume average width of side channels of 2 m
         Length_sc = Shape_Length * sc_mult,
         Period = "Hist",
         NEAR_DIST = 0,
         HabUnit = "Side channel",
         Hist_salm = "Hist salmon") %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
  select(noaaid, sc_mult, Area_ha, Length_sc, Period, NEAR_DIST, HabUnit, Hist_salm, curr.tempmult, hist.tempmult, curr_temp, hist_temp, 
         tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang)

#Create entire floodplain data frame

Floodplain_raw %<>%
  mutate(Length_sc = Shape_Length / 2) %>% #perimeter / 2 ~ Length of each side
  bind_rows(.,hist_sc) %>%
  select(HabUnit, Area_ha, Period, Hist_salm, noaaid, NEAR_FID, NEAR_DIST, ET_ID, Length_sc, wse_intersect)

fp <- Floodplain_raw %>%####fix Length_sc for spawning once non -histsc side channel spawning added
  left_join(., LgRiver_raw %>%
              rename(noaaid_lr = noaaid) %>%
              select(noaaid_lr, ET_ID), by = "ET_ID") %>%
  mutate(noaaid = ifelse(is.na(noaaid_lr), 
                         noaaid, 
                         noaaid_lr)) %>%
  left_join(., flowline %>% 
              select(spawn_dist, species, both_chk, lc, noaaid, slope, pass_tot, Subbasin_num, pass_tot_natural, Reach, curr.tempmult, hist.tempmult,
                     curr_temp, hist_temp, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang), 
            by = "noaaid") %>%
  mutate(curr.tempmult = ifelse(species == 'spring_chinook', 
                                1,
                                curr.tempmult),
         hist.tempmult = ifelse(species == 'spring_chinook',
                                1, 
                                hist.tempmult),
         slope.class = ifelse(slope < .02, 
                              "low",
                              ifelse(slope >= .02 & slope < .04, 
                                     "med", 
                                     "high")),
         Habitat = case_when(HabUnit %in% c("FP channel", "FP Channel") ~ "FP_Channel",
                             HabUnit == "Side channel" ~ "Side_Channel",
                             HabUnit == "Pond" & (Area_ha >.05 & Area_ha < 5) ~ "FP_Pond_lg",
                             HabUnit == "Pond" & Area_ha > 5 ~ "Lake",
                             HabUnit == "Pond" & Area_ha < .05 ~ "FP_Pond_sm",
                             HabUnit == "Slough" & Area_ha > .05 ~ "FP_Pond_lg",
                             HabUnit == "Slough" & Area_ha <= .05 ~ "FP_Pond_sm",
                             HabUnit == "Lake" ~ "Lake",
                             HabUnit == "Marsh" ~ "Marsh"),
         lc = ifelse(lc == "", "Reference", as.character(lc))) %>%
  left_join(., ss.dist) %>% 
  left_join(., wood_data, by = "Subbasin_num")

assign('asrp_fp_raw', fp , envir = .GlobalEnv) 

curr_fp_scenarios <- c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Wood", "Shade")
hist_fp_scenarios <- c("Historical", "Floodplain", "FP_wood_comb")

fp_curr <- lapply(curr_fp_scenarios, function(x) {
  fp %>%
    filter(Period %in% c("Both", "Curr")) %>%
    mutate(hab.scenario = x)
}) %>%
  do.call('rbind',.)

fp_hist <- lapply(hist_fp_scenarios, function(y) {
  fp %>%
    filter(Period %in% c("Both", "Hist")) %>%
    mutate(hab.scenario = y)
}) %>%
  do.call('rbind',.)

fp_join <- bind_rows(fp_curr, fp_hist) %>%
  left_join(., ss.dist.ref)

fp2 <- fp_join %>%
  bind_rows(., fp_join %>%
              filter(Habitat == "Side_Channel") %>%
              mutate(Area_orig = Area_ha,
                     SC_pool = Area_orig * pool.perc, #),
                     SC_riffle = Area_orig * (1 - pool.perc)) %>% #) %>%
              gather(Habitat, Area_new, SC_pool:SC_riffle) %>%
              mutate(Area_ha = Area_new)) %>%
  filter(!Habitat == "Side_Channel") %>%
  mutate(curr.tempmult = ifelse(Subbasin_num %in% mainstem.subs,
                                curr.tempmult,
                                1),
         hist.tempmult = ifelse(Subbasin_num %in% mainstem.subs,
                                hist.tempmult,
                                1),
         summer = case_when(
           hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Floodplain") ~ Area_ha * curr.tempmult,
           hab.scenario == "Shade"~ Area_ha * hist.tempmult, 
           hab.scenario %in% c("FP_wood_comb", "Wood") ~ 
             ifelse(Habitat %in% c("SC_pool", "SC_riffle"),
                    Area_ha * woodmult_s * curr.tempmult,
                    Area_ha * curr.tempmult),
           hab.scenario == "Historical" ~ 
             ifelse(Habitat %in% c("SC_pool", "SC_riffle"),
                    Area_ha * woodmult_s * hist.tempmult,
                    Area_ha * hist.tempmult)),
         winter = case_when(
           hab.scenario %in% c("Current", "Beaver", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Shade", "Floodplain") ~
             ifelse(Habitat == "SC_pool",
                    Area_ha * winter_pool_scalar_warm,
                    ifelse(Habitat == "SC_riffle",
                           Area_ha + ((1 - winter_pool_scalar_warm) * Area_orig * pool.perc),
                           Area_ha)),
           hab.scenario %in% c("FP_wood_comb", "Wood", "Historical") ~
             ifelse(Habitat == "SC_pool", 
                    Area_ha * winter_pool_scalar_warm * woodmult_w,
                    ifelse(Habitat == "SC_riffle", 
                           (Area_ha + ((1 - winter_pool_scalar_warm) * Area_orig * pool.perc)) * woodmult_w, 
                           Area_ha)))
  ) %>%
  gather(life.stage, Area, summer:winter) %>%
  mutate(Area = ifelse(hab.scenario %in% c("Barriers", "Historical"),
                       ifelse(pass_tot_natural == 0, 
                              0, 
                              Area), 
                       ifelse(pass_tot == 0, 
                              0, 
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, Hist_salm, species, spawn_dist, both_chk, NEAR_DIST, ET_ID, pass_tot_natural, 
         curr.tempmult, hist.tempmult, wse_intersect) %>%
  filter(Hist_salm == "Hist salmon",
         ifelse(hab.scenario %in% c("Current", "Beaver", "Barriers", "Fine_sediment", "LR_bank", "LR_length", "Shade", "Wood"),
                spawn_dist == "Yes" & NEAR_DIST < 5  | Subbasin_num %in% mainstem.subs & wse_intersect == "Yes" | spawn_dist == "Yes" & wse_intersect == 'Yes',
                spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs))


if (fishtype == "spring_chinook") {
  fp2 <- fp2 %>%
    filter(Subbasin_num %in% schino_subs)
}

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  fp2 <- fp2 %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area * chino_mult,
                         Area))
}


# if (fishtype == "spring_chinook") {
#   fp2 <- fp2 %>%
#     mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
#                                    schino_mult,
#                                    1),
#            Area = Area * chinook_scalar)
# }
# 
# if (fishtype == "fall_chinook") {
#   fp2 <- fp2 %>%
#     mutate(chinook_scalar = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
#                                    fchino_mult,
#                                    1),
#            Area * chinook_scalar)
# }

