#### 2.c. Floodplain ----

asrp_fp_precalc1 <- asrp_fp_raw %>%
  left_join(., ss.dist.ref) %>% 
  left_join(., fl_to_gsu, by = "noaaid") %>%
  left_join(., asrp_culvs)

if (fishtype == "spring_chinook") {
  asrp_fp_precalc1 %<>%
    rename(Area_nochino = Area_ha) %>%
    mutate(Area_ha = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                            Area_nochino * schino_mult,
                            Area_nochino))
} else if (fishtype == "fall_chinook") {
  asrp_fp_precalc1 %<>%
    rename(Area_nochino = Area_ha) %>%
    mutate(Area_ha = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                            Area_nochino * fchino_mult,
                            Area_nochino))
}

asrp_fp_precalc1 <- asrp_fp_precalc1 %>%
  mutate(Area_ha = ifelse(pass_tot_asrp == 0,
                          0,
                          Area_ha),
         GSU = ifelse(primary_cr_only == "y",
                      ifelse(Reach %in% primary_cr,
                             GSU,
                             paste0(GSU, "_np")),
                      as.character(GSU)))

asrp_fp_precalc <- asrp_fp_precalc1 %>%
  bind_rows(., asrp_fp_precalc1 %>%
              filter(Habitat == "Side_Channel") %>%
              mutate(pool.perc.asrp = pool.perc,
                     Area_orig = Area_ha,
                     SC_pool = Area_orig * pool.perc.asrp,
                     SC_riffle = Area_orig * (1 - pool.perc.asrp)) %>%
              gather(Habitat, Area_new, SC_pool:SC_riffle) %>%
              rename(Area_ha = Area_new)) %>% 
  filter(!Habitat == "Side_Channel") %>%
  mutate(summer = Area_ha,
         winter = ifelse(Habitat == "SC_pool",
                         Area_ha * winter_pool_scalar_warm,
                         ifelse(Habitat == "SC_riffle",
                                Area_ha + ((1 - winter_pool_scalar_warm) * Area_orig * pool.perc),
                                Area_ha))) %>%
  gather(life.stage, Area, summer:winter)

if (fishtype == "spring_chinook") {
  
  asrp_fp_curr <- asrp_fp_precalc %>%
    filter(Period %in% c("Curr", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs & ET_ID > 0,
           Subbasin_num %in% schino_subs)
  
  asrp_fp_hist <- asrp_fp_precalc %>%
    filter(Period %in% c("Hist", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs,
           Subbasin_num %in% schino_subs)
} else if (fishtype == "coho") {
  
  asrp_fp_curr <- asrp_fp_precalc %>%
    filter(Period %in% c("Curr", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
  
  asrp_fp_hist <- asrp_fp_precalc %>%
    filter(Period %in% c("Hist", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
} else if (fishtype == "fall_chinook") {
  
  asrp_fp_curr <- asrp_fp_precalc %>%
    filter(Period %in% c("Curr", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
  
  asrp_fp_hist <- asrp_fp_precalc %>%
    filter(Period %in% c("Hist", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
} else if (fishtype == "steelhead") {
  
  asrp_fp_curr <- asrp_fp_precalc %>%
    filter(Period %in% c("Curr", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
  
  asrp_fp_hist <- asrp_fp_precalc %>%
    filter(Period %in% c("Hist", "Both"),
           Hist_salm == "Hist salmon",
           spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs & ET_ID > 0)
}

asrp_fp <- asrp_fp_curr %>%
  group_by(GSU, Habitat, life.stage, forest, Subbasin_num) %>%
  summarize(curr_area = sum(Area, na.rm = T)) %>%
  ungroup() %>%
  left_join(., asrp_fp_hist %>%
              group_by(GSU, Habitat, life.stage, forest) %>%
              summarize(hist_area = sum(Area, na.rm = T)) %>%
              ungroup()) %>%
  left_join(.,asrp_scenarios %>%
              select(GSU, rest_perc_nf, rest_perc_f), 
            by = "GSU") %>%
  mutate(rest_perc_nf = ifelse(is.na(rest_perc_nf),
                               0,
                               rest_perc_nf),
         Area = ifelse(GSU %in% floodplain_gsu,
                       ifelse(forest == "y",
                              curr_area + ((hist_area - curr_area * rest_perc_f * fp_intensity_scalar_f)),
                              curr_area + ((hist_area - curr_area) * rest_perc_nf * fp_intensity_scalar_nf)),
                       curr_area)) %>%
  group_by(GSU, Habitat, life.stage, Subbasin_num) %>%
  summarize(Area = sum(Area, na.rm = T )) %>%
  ungroup()
# %>%
  # left_join(., density) 
# %>%
  # mutate(capacity = Area * Density)
