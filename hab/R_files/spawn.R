fp_spawn <- fp %>%
  filter(Habitat == "Side_Channel",
         ifelse(Period == "Hist",
                spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs,
                spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs))

if (fishtype == "spring_chinook") {
  fp_spawn <- fp_spawn %>%
    filter(Subbasin_num %in% schino_subs)
}

fps <- fp_spawn %>%
  filter(Period %in% c("Curr", "Both")) %>%
  mutate(hab.scenario = "Current",
         eggs = Length_sc * pass_tot * PR_redd_density / 1000 * fecundity)

fps1 <- fps %>%
  bind_rows(., fps %>%
              mutate(hab.scenario = "Fine_sediment")) %>%
  bind_rows(., fps %>%
              mutate(hab.scenario = "LR_bank")) %>%
  bind_rows(., fps %>%
              mutate(hab.scenario = "LR_length")) %>%
  bind_rows(., fps %>%
              mutate(hab.scenario = "Shade")) %>%
  bind_rows(., fps %>%
              mutate(hab.scenario =  "Wood")) %>%
  bind_rows(., fps %>%
              mutate(hab.scenario = "Beaver")) %>%
  bind_rows(fp_spawn %>%
              filter(Period %in% c("Hist", "Both")) %>%
              mutate(hab.scenario = "Floodplain",
                     eggs = Length_sc * pass_tot_natural * PR_redd_density / 1000 * fecundity)) %>%
  bind_rows(fp_spawn %>%
              filter(Period %in% c("Hist", "Both")) %>%
              mutate(hab.scenario = "FP_wood_comb",
                     eggs = Length_sc * pass_tot_natural * PR_redd_density / 1000 * fecundity)) %>%
  bind_rows(fp_spawn %>%
              filter(Period %in% c("Curr", "Both")) %>%
              mutate(hab.scenario = "Barriers",
                     eggs = Length_sc * pass_tot_natural * PR_redd_density / 1000 * fecundity)) %>%
  bind_rows(fp_spawn %>%
              filter(Period %in% c("Hist", "Both")) %>%
              mutate(hab.scenario = "Historical",
                     eggs = Length_sc * pass_tot_natural * PR_redd_density / 1000 * fecundity)) %>%
  mutate(adults = eggs / fecundity * adult_per_redd) 


ss_spawn <- ss %>%
filter(slope < .03)

ss_sp <- ss_spawn %>%
  mutate(hab.scenario = "Current",
         eggs = ifelse(slope < .01, 
                       Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
                       ifelse(lc == "Forest", 
                              Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * curr_beaver_mult, 
                              Shape_Length * pass_tot * NF_redd_density / 1000 * fecundity * curr_beaver_mult)))

ss_sp1 <- ss_sp %>%
  bind_rows(., ss_sp %>%
              mutate(hab.scenario = "Fine_sediment")) %>%
  bind_rows(., ss_sp %>%
              mutate(hab.scenario = "LR_bank")) %>%
  bind_rows(., ss_sp %>%
              mutate(hab.scenario = "LR_length")) %>%
  bind_rows(., ss_sp %>%
              mutate(hab.scenario = "Shade")) %>%
  bind_rows(., ss_sp %>%
              mutate(hab.scenario = "Floodplain")) %>%
  bind_rows(., ss_spawn %>%
              mutate(hab.scenario = "Barriers",
                     eggs = ifelse(slope < .01, 
                                   Shape_Length * pass_tot_natural * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
                                   ifelse(lc == "Forest", 
                                          Shape_Length * pass_tot_natural * F_redd_density / 1000 * fecundity * curr_beaver_mult,
                                          Shape_Length * pass_tot_natural * NF_redd_density / 1000 * fecundity * curr_beaver_mult)))) %>%
  bind_rows(., ss_spawn %>%
              mutate(hab.scenario = "Wood",
                     eggs = ifelse(slope < .01, 
                                   Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
                                   Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * curr_beaver_mult))) %>%
  bind_rows(., ss_spawn %>%
              mutate(hab.scenario = "FP_wood_comb",
                     eggs = ifelse(slope < .01, 
                                   Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
                                   Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * curr_beaver_mult))) %>%
  bind_rows(., ss_spawn %>%
              mutate(hab.scenario = "Beaver",
                     eggs = ifelse(slope < .01, 
                                   Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * hist_beaver_mult,
                                   ifelse(lc == "Forest", 
                                          Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * hist_beaver_mult,
                                          Shape_Length * pass_tot * NF_redd_density / 1000 * fecundity * hist_beaver_mult)))) %>%
  bind_rows(., ss_spawn %>%
              mutate(hab.scenario = "Historical",
                     eggs = ifelse(slope < .01, 
                                   Shape_Length * pass_tot_natural * PR_redd_density / 1000 * fecundity * hist_beaver_mult,
                                   Shape_Length * pass_tot_natural * F_redd_density / 1000 * fecundity * hist_beaver_mult))) %>%
  mutate(adults = eggs / fecundity * adult_per_redd)
  

lrsp <- lgr_spawning_area %>% 
  mutate(hab.scenario = "Current",
         eggs = spawn_area_passable / redd_area * fecundity)

lrsp1 <- lrsp %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Fine_sediment")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "LR_bank")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "LR_length")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Shade")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Floodplain")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Beaver")) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Wood",
                     eggs = eggs * wood_spawn_mult)) %>%  
  bind_rows(lrsp %>%
              mutate(hab.scenario = "FP_wood_comb",
                     eggs = eggs * wood_spawn_mult)) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Barriers",
                     eggs = spawn_area_passable_nat / redd_area * fecundity)) %>%
  bind_rows(lrsp %>%
              mutate(hab.scenario = "Historical",
                     eggs = spawn_area_passable_hist / redd_area * fecundity * wood_spawn_mult)) %>%   
  mutate(adults = eggs / fecundity * adult_per_redd)

spawn_tot <- bind_rows(fps1, ss_sp1, lrsp1) %>%
  group_by(hab.scenario, Subbasin_num) %>%
  summarize(eggs = sum(eggs, na.rm = T),
            adults = sum(adults, na.rm = T)) %>%
  gather(life.stage, capacity, c(eggs, adults)) %>%
  ungroup()
  