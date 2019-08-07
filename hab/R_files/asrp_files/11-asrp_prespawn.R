#### Prespawn survival ----
if (run_single_action == 'no') {
  asrp_reach_data %<>%
    filter(!Scenario_num %in% single_action_scenarios)
}

if (fishtype == "spring_chinook") {
  prespawn_asrp <- asrp_reach_data %>%
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
    mutate(pass_tot_asrp_weight = pass_tot_asrp * eggs_weight) %>%
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




