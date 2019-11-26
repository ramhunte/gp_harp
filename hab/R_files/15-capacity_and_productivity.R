# Capacity calculation ----

asrp_cap <- bind_rows(asrp_lr, asrp_ss, asrp_fp) %>%
  select(-GSU, -pass_tot_asrp, -both_chk, -Barriers, -Floodplain, -Beaver, -Riparian, -chino_mult, -lc, -slope.class, -curr_area, -hist_area,
         -curr_temp, -Reach, -species, -can_ang, -prespawn_temp,
         -hist_temp, -managed_forest, -temp_intensity_scalar, -fp_intensity_scalar, -beaver_intensity_scalar, -primary_cr_only, 
         -woodmult_s, -woodmult_w, -prespawn_temp_asrp, -future_imperv)

# Create second year for steelhead rearing

if (fishtype == "steelhead") {
  asrp_cap %<>%
    bind_rows(., asrp_cap %>%
                filter(life.stage %in% c("summer", "winter")) %>%
                mutate(life.stage = ifelse(life.stage == "summer",
                                           "summer.2",
                                           "winter.2")))
}

# Duplicate spring chinook rearing data.  This allows us to calculate a second survival value with the temperature multiplier applied.  
# This second survival is assigned to surv_s_2

if (fishtype %in% c("spring_chinook", 'fall_chinook')) {
  asrp_cap <- asrp_cap %>%
    bind_rows(., asrp_cap %>%
                mutate(life.stage = ifelse(life.stage == 'summer',
                                           'summer.2',
                                           'winter.2'))) %>%
    select(-tempmult.asrp) %>%
    left_join(., asrp_reach_data %>%
                select(noaaid, Scenario_num, year, tempmult.asrp)) %>%
    mutate(tempmult.asrp = ifelse(life.stage %in% c('summer', 'winter'),
                                  1,
                                  tempmult.asrp))
}

if (fishtype == 'chum') {
  asrp_cap %<>%
    bind_rows(., asrp_cap %<>%
                mutate(life.stage = ifelse(life.stage == 'summer',
                                           'fry.colonization',
                                           'summer.2')))
}

asrp_cap %<>%
  left_join(., density) %>%
  mutate(capacity = Area * Density)

source("hab/R_files/16-movement.R", local = TRUE)

rm(asrp_fp_mvmt, asrp_ss_mvmt, asrp_lr_mvmt, asrp_mvmt_1)

# Productivity calculations ----

asrp_prod_step_1 <- asrp_cap %>%
  left_join(., survival) %>%
  mutate(
    surv.base.asrp.summer = case_when(
      Habitat %in% LgRiver_habs ~ surv.base * woodmult_s_asrp * tempmult.asrp,
      !Habitat %in% LgRiver_habs ~
        ifelse(LW == 'y',
               (surv.base + ((wood.surv.base - surv.base) * rest_perc * wood_intensity_scalar)) * tempmult.asrp,
               surv.base * tempmult.asrp)
    ),
    surv.base.asrp.winter = case_when(
      Habitat %in% LgRiver_habs ~ surv.base * woodmult_w_asrp,
      !Habitat %in% LgRiver_habs ~ 
        ifelse(LW == 'y',
               (surv.base + ((wood.surv.base - surv.base) * rest_perc * wood_intensity_scalar)),
               surv.base)
    ),
    surv.base.asrp = ifelse(life.stage %in% c("summer", "summer.2"),
                            surv.base.asrp.summer,
                            surv.base.asrp.winter)) %>%
  select(-surv.base.asrp.summer, -surv.base.asrp.winter)

  asrp_prod <- asrp_prod_step_1 %<>%
    group_by(Subbasin_num, life.stage, year, Scenario_num) %>%
    mutate(cap.sum.sub = sum(capacity, na.rm = T),
           cap.perc = capacity / cap.sum.sub,
           survival = cap.perc * surv.base.asrp) %>%
    ungroup()
  
  asrp_prod_adjustment <- asrp_prod
  
  asrp_prod <- asrp_prod %>%
    left_join(., asrp_prod_adjustment %>%
                filter(Habitat %in% LgRiver_habs,
                       life.stage %in% c('summer', 'summer.2')) %>%
                group_by(Subbasin_num, Scenario_num, year, life.stage) %>%
                summarize(survival = sum(survival, na.rm = T),
                          cap.perc = sum(cap.perc, na.rm = T)) %>%
                group_by(Subbasin_num, life.stage, year) %>%
                mutate(survival.curr = ifelse(Scenario_num == 'Current',
                                              survival,
                                              0),
                       survival.curr = sum(survival.curr, na.rm = T),
                       surv.adj = (survival - survival.curr) * cap.perc) %>%
                select(Subbasin_num, year, Scenario_num, surv.adj)) %>%
    group_by(Subbasin_num, Scenario_num, year, life.stage) %>%
    summarize(capacity = sum(capacity, na.rm = T),
              survival = sum(survival, na.rm = T),
              surv.adj = unique(surv.adj, na.rm = T)) %>%
    group_by(Subbasin_num, life.stage, year) %>%
    mutate(survival.curr = ifelse(Scenario_num == 'Current',
                                  survival,
                                  0),
           survival.curr = sum(survival.curr, na.rm = T),
           survival = ifelse(life.stage %in% c('summer', 'summer.2') & Scenario_num %in% c('Shade', 'LR_bank', 'LR_length')
                             & Subbasin_num %in% mainstem.subs,
                             survival.curr + surv.adj,
                             survival)) %>%
    select(Scenario_num, year, life.stage, capacity, survival) 
if (fishtype == 'chum') {
  asrp_prod %<>% 
    filter(life.stage != 'summer') %>%
    bind_rows(., asrp_prod_step_1 %>%
                filter(life.stage == 'summer') %>%
                group_by(Subbasin_num, life.stage, year, Scenario_num) %>%
                summarize(survival = mean(surv.base.asrp, na.rm = T)))
}

rm(asrp_fp, asrp_lr, asrp_ss, asrp_prod_adjustment, mvmt_data, mvmt_data_curr, asrp_cap)
