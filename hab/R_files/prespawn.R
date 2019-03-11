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
  
  curr.prespawn <- c("Barriers", "Beaver", "Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  hist.prespawn <- c("Historical", "Shade")
  
  prespawn.surv <- bind_rows(lapply(curr.prespawn, function(x){
    prespawn_surv_calc <- prespawn %>%
      mutate(hab.scenario = x,
             survival = cramer.prespawn(prespawn_temp_curr) * pass_tot)
  }) %>%
    do.call('rbind',.),
  lapply(hist.prespawn, function(x){
    prespawn_surv_calc <- prespawn %>%
      mutate(hab.scenario = x,
             survival = cramer.prespawn(prespawn_temp_hist) * pass_tot_natural)
  }) %>%
    do.call('rbind',.)) %>%
    select(-prespawn_temp_curr, -prespawn_temp_hist, -pass_tot, -pass_tot_natural) %>%
    mutate(life.stage = "prespawn")
} else {
  
  prespawn <- flowline %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) 
  
  curr.prespawn <- c("Shade", "Beaver", "Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
  hist.prespawn <- c("Historical", "Barriers")
  
  prespawn.surv <- bind_rows(lapply(curr.prespawn, function(x) {
    prespawn_surv_calc <- prespawn %>%
      mutate(hab.scenario = x,
             survival = prespawn_surv_raw * pass_tot) %>%
      group_by(Subbasin_num, hab.scenario) %>%
      summarize(survival = mean(survival, na.rm = T)) %>%
      ungroup()
  }) %>% 
    do.call('rbind',.),
  lapply(hist.prespawn, function(x) {
    prespawn_surv_calc <- prespawn %>%
      mutate(hab.scenario = x,
             survival = prespawn_surv_raw * pass_tot_natural) %>%
      group_by(Subbasin_num, hab.scenario) %>%
      summarize(survival = mean(survival, na.rm = T)) %>%
      ungroup()
  }) %>%
    do.call('rbind',.)) %>%
    select(hab.scenario, Subbasin_num, survival) %>%
    mutate(life.stage = "prespawn") 
  
}
