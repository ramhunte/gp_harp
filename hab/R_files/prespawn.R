# Purpose: This script is where prespawn productivity is calculated
# For all species we augment prespawn by the passage percent (barrier impact on prod)
# For spring chinook, we use temperature to augment prespawn



if (fishtype == "spring_chinook") {
  prespawn <- flowline %>%
    filter(curr_temp > 0,
           hist_temp > 0,
           ifelse(Habitat == "LgRiver",
                  spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
                  spawn_dist == "Yes" & Subbasin_num %in% schino_subs)) %>%
    mutate(prespawn_temp_curr = ifelse(!is.na(mdm), 
                                       mdm,
                                       ifelse(!is.na(edt_mdm_temp), 
                                              edt_mdm_temp, 
                                              15)),
           prespawn_temp_hist = prespawn_temp_curr - temp_diff) %>%
    group_by(Subbasin_num) %>%
    summarize(prespawn_temp_curr = mean(prespawn_temp_curr, na.rm = T),
              prespawn_temp_hist = mean(prespawn_temp_hist, na.rm = T),
              pass_tot = mean(pass_tot, na.rm = T),
              pass_tot_natural = mean(pass_tot_natural, na.rm = T)) %>%
    ungroup()
  
  curr.prespawn <- c("Barriers", "Beaver", "Current", "Fine_sediment", 
                     "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  
  prespawn.surv <- lapply(all_scenarios, function(x) {prespawn %>% mutate(hab.scenario = x)}) %>%
    do.call('rbind',.) %>%
    mutate(survival = case_when(
      hab.scenario %in% curr.prespawn ~ cramer.prespawn(prespawn_temp_curr) * pass_tot,
      hab.scenario  ==  "Barriers"    ~ cramer.prespawn(prespawn_temp_curr) * pass_tot_natural,
      hab.scenario  ==  "Shade"       ~ cramer.prespawn(prespawn_temp_hist) * pass_tot,
      hab.scenario  ==  "Historical"  ~ cramer.prespawn(prespawn_temp_hist) * pass_tot_natural
      )
    ) %>%
    mutate(life.stage = "prespawn") %>%
    select(hab.scenario, Subbasin_num, life.stage, survival)
  
} else {
  
  prespawn <- flowline %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) 
  
  curr.prespawn <- c("Shade", "Beaver", "Current", "Fine_sediment", 
                     "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  
  hist.prespawn <- c("Historical", "Barriers")
  
  prespawn.surv <- lapply(all_scenarios, function(x) {prespawn %>% mutate(hab.scenario = x)}) %>%
    do.call('rbind',.) %>%
    mutate(survival = case_when(
      hab.scenario %in% curr.prespawn ~ prespawn_surv_raw * pass_tot,
      hab.scenario %in% hist.prespawn ~ prespawn_surv_raw * pass_tot_natural
      )
    ) %>%
    group_by(Subbasin_num, hab.scenario) %>%
    summarize(survival = mean(survival, na.rm = T)) %>%
    mutate(life.stage = "prespawn") %>%
    ungroup()
  
}
