#### Prespawn survival ----
if (fishtype == "spring_chinook") {
  prespawn_asrp <- flowline %>%
    left_join(., fl_to_gsu) %>%
    filter(curr_temp > 0,
           hist_temp > 0,
           ifelse(Habitat == "LgRiver",
                  spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs,
                  spawn_dist == "Yes" & Subbasin_num %in% schino_subs)) %>%
    mutate(temp_diff_2019 = 0,
           temp_diff_2019_cc_only = 0,
           prespawn_temp_curr = ifelse(!is.na(mdm),
                                       mdm,
                                       ifelse(!is.na(edt_mdm_temp),
                                              edt_mdm_temp,
                                              15)),
           prespawn_temp_hist = prespawn_temp_curr - UQ(as.name(paste0("temp_diff_", x))),
           prespawn_temp_asrp = ifelse(!GSU %in% shade_gsu & can_ang > 170,
                                       prespawn_temp_curr + ((UQ(as.name(paste0("temp_diff_", x, "_cc_only"))) * prespawn_temp_slope) - 
                                                               prespawn_temp_intercept),
                                       prespawn_temp_curr + ((UQ(as.name(paste0("temp_diff_", x))) * prespawn_temp_slope) - 
                                                               prespawn_temp_intercept)),
           pass_tot_asrp = ifelse(GSU %in% barrier_gsu,
                                  pass_tot_natural,
                                  pass_tot)) %>%
    group_by(Subbasin_num) %>%
    summarize(prespawn_temp_asrp = mean(prespawn_temp_asrp, na.rm = T),
              pass_tot_asrp = mean(pass_tot_asrp, na.rm = T)) %>%
    ungroup() %>%
    mutate(productivity = cramer.prespawn(prespawn_temp_asrp) * pass_tot_asrp) %>%
    select(-prespawn_temp_asrp, -pass_tot_asrp) %>%
    mutate(life.stage = "prespawn")
} else {
  prespawn_asrp <- flowline %>%
    filter(spawn_dist == "Yes" | Subbasin_num %in% mainstem.subs) %>%
    left_join(., fl_to_gsu) %>%
    mutate(pass_tot_asrp = ifelse(GSU %in% barrier_gsu,
                                  pass_tot_natural,
                                  pass_tot),
           productivity = prespawn_surv_raw * pass_tot_asrp) %>%
    group_by(Subbasin_num) %>%
    summarize(productivity = mean(productivity, na.rm = T)) %>%
    ungroup() %>%
    select(Subbasin_num, productivity) %>%
    mutate(life.stage = "prespawn")
}




