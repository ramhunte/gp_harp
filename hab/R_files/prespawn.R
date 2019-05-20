# Purpose: This script is where prespawn productivity is calculated
# For all species we augment prespawn by the passage percent (barrier impact on prod)
# For spring chinook, we use temperature to augment prespawn
# For coho, use mean % impervious (mn_imperv) to augment prespawn
# Prespawn is now weighted by the egg capacity of each reach


if (fishtype == "spring_chinook") {
  
  prespawn <- flowline %>%
    filter(curr_temp > 0,
           hist_temp > 0,
           ifelse(Habitat == "LgRiver",
                  spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
                  spawn_dist == "Yes" & Subbasin_num %in% schino_subs)) 
  
  curr.prespawn <- c("Beaver", "Current", "Fine_sediment", 
                     "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  
  prespawn.surv <- lapply(diag_scenarios, function(x) {prespawn %>% mutate(hab.scenario = x)}) %>%
    do.call('rbind',.) %>%
    left_join(., egg_cap_weight) %>%
    mutate(prespawn_temp_curr = prespawn_temp,
           prespawn_temp_hist = prespawn_temp_curr - temp_diff,
           pass_tot_weight = pass_tot * eggs_weight,
           pass_tot_nat_weight = pass_tot_natural * eggs_weight) %>%
    group_by(Subbasin_num, hab.scenario) %>%
    summarize(prespawn_temp_curr = mean(prespawn_temp_curr, na.rm = T),
              prespawn_temp_hist = mean(prespawn_temp_hist, na.rm = T),
              pass_tot_weight = sum(pass_tot_weight, na.rm = T),
              pass_tot_nat_weight = sum(pass_tot_nat_weight, na.rm = T)) %>%
    ungroup() %>%
    mutate(survival = case_when(
      hab.scenario %in% curr.prespawn ~ cramer.prespawn(prespawn_temp_curr) * pass_tot_weight,
      hab.scenario  ==  "Barriers"    ~ cramer.prespawn(prespawn_temp_curr) * pass_tot_nat_weight,
      hab.scenario  ==  "Shade"       ~ cramer.prespawn(prespawn_temp_hist) * pass_tot_weight,
      hab.scenario  ==  "Historical"  ~ cramer.prespawn(prespawn_temp_hist) * pass_tot_nat_weight
    )
    # mutate(survival = case_when(
    #   hab.scenario %in% curr.prespawn ~ prespawn.chin.func(prespawn_temp_curr) * pass_tot_weight,
    #   hab.scenario  ==  "Barriers"    ~ prespawn.chin.func(prespawn_temp_curr) * pass_tot_nat_weight,
    #   hab.scenario  ==  "Shade"       ~ prespawn.chin.func(prespawn_temp_hist) * pass_tot_weight,
    #   hab.scenario  ==  "Historical"  ~ prespawn.chin.func(prespawn_temp_hist) * pass_tot_nat_weight
    # )
    ) %>%
    mutate(life.stage = "prespawn") %>%
    select(hab.scenario, Subbasin_num, life.stage, survival)
  
} else {# species other than spring chinook
  
  
  prespawn <- flowline %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
    mutate(imperv_mult = calc_coho_imperv(mn_imperv)) # For all species other than coho, use imperv_mult of 1
  
  curr.prespawn <- c("Shade", "Beaver", "Current", "Fine_sediment", 
                     "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  
  prespawn.surv <- lapply(diag_scenarios, function(x) {prespawn %>% mutate(hab.scenario = x)}) %>%
    do.call('rbind',.) %>%
    left_join(., egg_cap_weight) %>%
    mutate(survival = case_when(
      hab.scenario %in% curr.prespawn ~ prespawn_surv_raw * pass_tot * imperv_mult * eggs_weight,
      hab.scenario  ==  'Barriers'    ~ prespawn_surv_raw * pass_tot_natural * imperv_mult * eggs_weight,
      hab.scenario  ==  'Historical'  ~ prespawn_surv_raw * pass_tot_natural * eggs_weight
    )
    ) %>%
    group_by(Subbasin_num, hab.scenario) %>%
    summarize(survival = sum(survival, na.rm = T)) %>%
    mutate(life.stage = "prespawn") %>%
    ungroup()
  
}
