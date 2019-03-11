#### Spawning Calculations ----

asrp_spawn_ss <- asrp_ss_raw %>%
  filter(slope < .03) %>%
  left_join(., asrp_culvs) %>%
  left_join(., fl_to_gsu) %>%
  mutate(Shape_Length = ifelse(GSU %in% beaver_gsu & !forest == "y",
                               Shape_Length * (1 - (.15 * rest_perc_nf * beaver_intensity_scalar_nf)),
                               Shape_Length),
         eggs = ifelse(slope < .01,
                       Shape_Length * pass_tot_asrp * PR_redd_density / 1000 * fecundity,
                       ifelse(lc == "Forest",
                              Shape_Length * pass_tot_asrp * F_redd_density / 1000 * fecundity,
                              ifelse(GSU %in% wood_gsu,
                                     ifelse(forest == 'y',
                                            Shape_Length * pass_tot_asrp * ((F_redd_density - NF_redd_density) * rest_perc_f * 
                                                                              wood_intensity_scalar_f) / 1000 * fecundity,
                                            Shape_Length * pass_tot_asrp * ((F_redd_density - NF_redd_density) * rest_perc_nf * 
                                                                              wood_intensity_scalar_nf) / 1000 * fecundity),
                                     Shape_Length * pass_tot_asrp * NF_redd_density / 1000 * fecundity))))

asrp_spawn_fp <- asrp_fp_raw %>%
  filter(Habitat == "Side_Channel",
         Hist_salm == "Hist salmon",
         ifelse(Period == "Hist", 
                spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs,
                spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs)) %>%
  left_join(., fl_to_gsu) %>%
  left_join(., asrp_culvs) %>%
  mutate(eggs = ifelse(Period == "Hist",
                       ifelse(GSU %in% floodplain_gsu & !forest == "y",
                              Length_sc * rest_perc_nf * fp_intensity_scalar_nf * pass_tot_asrp * PR_redd_density / 1000 * fecundity,
                              0),
                       Length_sc * pass_tot_asrp * PR_redd_density / 1000 * fecundity))
if (fishtype == "spring_chinook") {
  asrp_spawn_fp %<>%
    filter(Subbasin_num %in% schino_subs)
} 

asrp_spawn_lr <- lgr_sp_area_asrp %>%
  left_join(., fl_to_gsu) %>%
  left_join(., asrp_culvs) %>%
  mutate(eggs = ifelse(GSU %in% wood_gsu,
                       ifelse(forest == 'y',
                              spawn_area * pass_tot_asrp / redd_area * fecundity * (1 + (wood_spawn_mult - 1) * rest_perc_f * 
                                                                                      wood_intensity_scalar_f),
                              spawn_area * pass_tot_asrp / redd_area * fecundity * (1 + (wood_spawn_mult - 1) * rest_perc_nf * 
                                                                                      wood_intensity_scalar_nf)),
                       spawn_area * pass_tot_asrp / redd_area * fecundity))

asrp_spawn_tot <- bind_rows(asrp_spawn_ss, asrp_spawn_fp, asrp_spawn_lr) %>%
  group_by(Subbasin_num) %>%
  summarize(eggs = sum(eggs, na.rm = T),
            adults = sum(eggs / fecundity * adult_per_redd, na.rm = T)) %>%
  ungroup() %>%
  gather(life.stage, capacity, c(eggs, adults))