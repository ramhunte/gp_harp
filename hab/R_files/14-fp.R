#### Floodplain ----
# Create historical side channel data frame from flowline and hist channel data joined by reach
hist_sc <- flowline %>%
  select(Reach, Shape_Length, spawn_dist, species, both_chk, Subbasin_num, noaaid, chino_mult) %>%
  left_join(., lr_length_raw, by = "Reach") %>%
  mutate(
         sc_mult = ifelse(is.na(sc_mult), 
                          0, 
                          sc_mult),
         Area_ha = calc_area(Shape_Length, sc_mult, sc_width),
         Area_ha = convert_m_to_ha(Area_ha),
         # Area_ha = (Shape_Length * sc_mult * 2) / 10000, # Assume average width of side channels of 2 m
         Length_sc = Shape_Length * sc_mult,
         Period = "Hist",
         NEAR_DIST = 0,
         HabUnit = "Side channel",
         Hist_salm = "Hist salmon") %>%
  filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
  select(noaaid, sc_mult, Area_ha, Length_sc, Period, NEAR_DIST, HabUnit, Hist_salm)

#Create entire floodplain data frame

asrp_fp_raw <- Floodplain_raw %>%
  mutate(Length_sc = Shape_Length / 2) %>% #perimeter / 2 ~ Length of each side
  bind_rows(., hist_sc) %>%
  select(HabUnit, Area_ha, Period, Hist_salm, noaaid, NEAR_FID, NEAR_DIST, ET_ID, Length_sc, wse_intersect)

asrp_fp_scenario <- create_scenarios(asrp_fp_raw) %>%
  left_join(., LgRiver_raw %>%
              rename(noaaid_lr = noaaid) %>%
              select(noaaid_lr, ET_ID), by = "ET_ID") %>%
  mutate(noaaid = ifelse(is.na(noaaid_lr), 
                         noaaid, 
                         noaaid_lr)) %>%
  left_join(., flowline %>% 
              select(spawn_dist, species, both_chk, lc, noaaid, slope, Subbasin_num, Reach, chino_mult), 
            by = "noaaid") %>%
  mutate(slope.class = ifelse(slope < .02, 
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

assign('asrp_fp_spawn', asrp_fp_scenario, envir = .GlobalEnv)

asrp_fp_precalc1 <- asrp_fp_scenario %>%
  select(Habitat, slope.class, noaaid, year, Scenario_num, Area_ha, both_chk, Subbasin_num, Reach, species, pool.perc, Period,Hist_salm, 
         spawn_dist, NEAR_DIST, wse_intersect, chino_mult) %>%
  # left_join(., ss.dist.ref) %>% 
  left_join(., asrp_reach_data) %>%
  left_join(., asrp_culvs)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  asrp_fp_precalc1 %<>%
    rename(Area_nochino = Area_ha) %>%
    mutate(Area_ha = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                            Area_nochino * chino_mult,
                            Area_nochino))
} 

asrp_fp_precalc1 <- asrp_fp_precalc1 %>%
  mutate(
    Area_ha = ifelse(pass_tot_asrp == 0,
                     0,
                     Area_ha))

asrp_fp_precalc <- asrp_fp_precalc1 %>%
  bind_rows(., asrp_fp_precalc1 %>%
              filter(Habitat == "Side_Channel") %>%
              mutate(pool.perc.asrp = pool.perc,
                     Area_orig = Area_ha,
                     SC_pool = Area_orig * pool.perc.asrp,
                     SC_riffle = Area_orig * (1 - pool.perc.asrp)) %>%
              gather(Habitat, Area_new, SC_pool:SC_riffle) %>%
              select(-Area_ha) %>%
              rename(Area_ha = Area_new)) %>% 
  filter(!Habitat == "Side_Channel") %>%
  mutate(summer = Area_ha,
         winter = ifelse(Habitat == "SC_pool",
                         Area_ha * winter_pool_scalar_warm,
                         ifelse(Habitat == "SC_riffle",
                                (Area_ha + ((1 - winter_pool_scalar_warm) * Area_orig * pool.perc)),
                                Area_ha))) %>%
  gather(life.stage, Area, summer:winter)

asrp_fp_curr <- asrp_fp_precalc %>%
  filter(Period %in% c("Curr", "Both"),
         Hist_salm == "Hist salmon",
         spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs & wse_intersect == "Yes" | spawn_dist == "Yes" & wse_intersect == "Yes")

asrp_fp_hist <- asrp_fp_precalc %>%
  filter(Period %in% c("Hist", "Both"),
         Hist_salm == "Hist salmon",
         spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs)

if (fishtype == "spring_chinook") {
  asrp_fp_curr %<>% 
    filter(Subbasin_num %in% schino_subs)
  
  asrp_fp_hist %<>% 
    filter(Subbasin_num %in% schino_subs)
}

asrp_fp <- asrp_fp_curr %>%
  ungroup() %>%
  group_by(noaaid, Habitat, life.stage, year, Scenario_num) %>%
  summarize(curr_area = sum(Area, na.rm = T)) %>%
  ungroup() %>%
  full_join(., asrp_fp_hist %>%
              ungroup() %>%
              group_by(noaaid, Habitat, life.stage, year, Scenario_num) %>%
              summarize(hist_area = sum(Area, na.rm = T))) %>%
  ungroup() %>%
  left_join(., asrp_reach_data) %>%
  mutate(curr_area = ifelse(is.na(curr_area),
                            0,
                            curr_area),
         hist_area = ifelse(is.na(hist_area),
                            0,
                            hist_area),
         woodmult_s_asrp = ifelse(Habitat %in% c("SC_pool", "SC_riffle"),
                                  woodmult_s_asrp,
                                  1),
         woodmult_w_asrp = ifelse(Habitat %in% c("SC_pool", "SC_riffle"),
                                 woodmult_w_asrp,
                                 1),
         tempmult.asrp = ifelse(species %in% c("coho", "steelhead"),
                                ifelse(Subbasin_num %in% mainstem.subs,
                                       tempmult.asrp,
                                       1),
                                1),
         Area = ifelse(Floodplain == 'y',
                       ifelse(life.stage == "summer",
                              (curr_area + ((hist_area - curr_area) * rest_perc * fp_intensity_scalar)) * woodmult_s_asrp * tempmult.asrp,
                              (curr_area + ((hist_area - curr_area) * rest_perc * fp_intensity_scalar)) * woodmult_w_asrp),
                       ifelse(life.stage == 'summer',
                              curr_area * tempmult.asrp * woodmult_s_asrp,
                              curr_area * woodmult_w_asrp))) 

rm(asrp_fp_curr, asrp_fp_hist, asrp_fp_precalc, asrp_fp_precalc1, asrp_fp_raw, Floodplain_raw, LgRiver_raw)


asrp_fp_mvmt <- asrp_fp %>%
  filter(Scenario_num %in% single_action_mvmt_scenarios)

if (run_single_action == 'no') {
asrp_fp %<>%
  filter(Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", 'dev_and_climate', diag_scenarios))
}

asrp_fp_spawn %<>%
  filter(Habitat == 'Side_Channel')