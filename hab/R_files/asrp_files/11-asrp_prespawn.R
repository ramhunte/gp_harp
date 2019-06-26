#### Prespawn survival ----
if (fishtype == "spring_chinook") {
  prespawn_asrp <- asrp_reach_data %>%
    # filter(!Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", "scenario_2_fp_only", 
                                # "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", "scenario_3_beaver_only",
                                # 'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only')) %>%
    left_join(., flowline %>%
                select(noaaid, Subbasin_num, spawn_dist, mn_imperv, prespawn_temp, temp_diff_2040_cc_only, temp_diff_2040, 
                       temp_diff_2080_cc_only, temp_diff_2080, Habitat)) %>%
    filter(curr_temp > 0,
           ifelse(Habitat == "LgRiver",
                  spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
                  spawn_dist == "Yes" & Subbasin_num %in% schino_subs)
           ) %>%
    left_join(., asrp_culvs) %>% 
    left_join(., egg_cap_weight_asrp) %>%
    mutate(temp_diff_2019 = 0,
           temp_diff_2019_cc_only = 0,
           prespawn_temp_curr = prespawn_temp,
           prespawn_temp_asrp = case_when(
             year == 2019 ~ prespawn_temp_curr, # This case is needed because of the prespawn_temp_intercept
             year == 2040 ~
               ifelse(Scenario_num %in% c('scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only'),
                      ifelse(!Riparian == 'y' & can_ang > 170,
                             prespawn_temp_curr * prespawn_temp_slope - prespawn_temp_intercept,
                             prespawn_temp_curr + (temp_diff_2040 - temp_diff_2040_cc_only) * prespawn_temp_slope - prespawn_temp_intercept),
                      ifelse(!Riparian == 'y' & can_ang > 170,
                             prespawn_temp_curr + temp_diff_2040_cc_only * prespawn_temp_slope - prespawn_temp_intercept,
                             prespawn_temp_curr + temp_diff_2040 * prespawn_temp_slope - prespawn_temp_intercept)),
             year == 2080 ~ 
               ifelse(Scenario_num %in% c('scenario_1_riparian_only', 'scenario_2_riparian_only', 'scenario_3_riparian_only'),
                      ifelse(!Riparian == 'y' & can_ang > 170,
                             prespawn_temp_curr * prespawn_temp_slope - prespawn_temp_intercept,
                             prespawn_temp_curr + (temp_diff_2080 - temp_diff_2080_cc_only) * prespawn_temp_slope - prespawn_temp_intercept),
               ifelse(!Riparian == 'y' & can_ang > 170,
                      prespawn_temp_curr + temp_diff_2080_cc_only * prespawn_temp_slope - prespawn_temp_intercept,
                      prespawn_temp_curr + temp_diff_2080 * prespawn_temp_slope - prespawn_temp_intercept))),
           pass_tot_asrp_weight = pass_tot_asrp * eggs_weight) %>%
    group_by(Subbasin_num, year, Scenario_num) %>%
    summarize(prespawn_temp_asrp = mean(prespawn_temp_asrp, na.rm = T),
              pass_tot_asrp_weight = sum(pass_tot_asrp_weight, na.rm = T)) %>%
    ungroup() %>%
    mutate(survival = cramer.prespawn(prespawn_temp_asrp) * pass_tot_asrp_weight) %>%
    # mutate(survival = prespawn.chin.func(prespawn_temp_asrp) * pass_tot_asrp_weight) %>%
    select(-prespawn_temp_asrp, -pass_tot_asrp_weight) %>%
    mutate(life.stage = "prespawn")
} else {
  prespawn_asrp <- asrp_reach_data %>%
    # filter(!Scenario_num %in% c("scenario_1_wood_only", "scenario_2_wood_only", "scenario_3_wood_only", "scenario_1_fp_only", "scenario_2_fp_only", 
                                # "scenario_3_fp_only", "scenario_1_beaver_only",  "scenario_2_beaver_only", "scenario_3_beaver_only",
                                # 'scenario_1_barrier_only', 'scenario_2_barrier_only', 'scenario_3_barrier_only')) %>%
    left_join(., flowline %>% 
                select(noaaid, Subbasin_num, spawn_dist, mn_imperv, pass_tot_natural, pass_tot)) %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
    left_join(., egg_cap_weight_asrp) %>%
    mutate(
      imperv_mult = case_when(
        Scenario_num  == 'Current_asrp' ~ calc_coho_imperv(mn_imperv),
        !Scenario_num == 'Current_asrp' ~ calc_coho_imperv(future_imperv + mn_imperv)),
      
      pass_tot_asrp = ifelse(Barriers == 'y',
                             pass_tot_natural,
                             pass_tot),
      survival = prespawn_surv_raw * pass_tot_asrp * imperv_mult * eggs_weight) %>%
    group_by(Subbasin_num, year, Scenario_num) %>%
    summarize(survival = sum(survival, na.rm = T)) %>%
    ungroup() %>%
    select(Subbasin_num, survival, year, Scenario_num) %>%
    mutate(life.stage = "prespawn")
}




