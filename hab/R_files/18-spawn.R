#### Spawning Calculations ----
if (run_single_action == 'no') {
  asrp_ss_spawn %<>%
    filter(!Scenario_num %in% single_action_scenarios)
  asrp_fp_spawn %<>%
    filter(!Scenario_num %in% single_action_scenarios)
}

if (fishtype != 'steelhead') {
  asrp_spawn_ss <- asrp_ss_spawn %>%
  left_join(., asrp_culvs) %>%
  left_join(., asrp_reach_data) %>%
  filter(slope < .03) %>%
  mutate(Shape_Length = ifelse(Beaver == 'y',
                               Shape_Length * (curr_beaver_mult - ((curr_beaver_mult - hist_beaver_mult) * rest_perc * beaver_intensity_scalar)),
                               Shape_Length * curr_beaver_mult),
         eggs = ifelse(slope < .01,
                       Shape_Length * pass_tot_asrp * PR_redd_density / 1000 * fecundity,
                       ifelse(lc == "Forest",
                              Shape_Length * pass_tot_asrp * F_redd_density / 1000 * fecundity,
                              ifelse(LW == 'y',
                                     Shape_Length * pass_tot_asrp * (NF_redd_density + (F_redd_density - NF_redd_density) * rest_perc * 
                                                                       wood_intensity_scalar) / 1000 * fecundity,
                                     Shape_Length * pass_tot_asrp * NF_redd_density / 1000 * fecundity))))
} else {
  asrp_spawn_ss <- asrp_ss_spawn %>%
    left_join(., asrp_culvs) %>%
    left_join(., asrp_reach_data) %>%
    filter(slope < .03) %>%
    
    mutate(Shape_Length = ifelse(Beaver == 'y',
                                          Shape_Length * (curr_beaver_mult - ((curr_beaver_mult - hist_beaver_mult) * rest_perc * beaver_intensity_scalar)),
                                          Shape_Length * curr_beaver_mult),
           psp = case_when(slope < .01 ~ ifelse(LW == 'y'| lc == 'Forest',
                                                psp_hwls,
                                                psp_lwls),
                           slope >= .01 ~ ifelse(LW == 'y' | lc == 'Forest',
                                                 psp_hwhs,
                                                 psp_lwhs)),
           spawn_area = (Shape_Length / (width_w * psp)) * width_w * (width_w * .5),
           eggs = spawn_area / redd_area * pass_tot_asrp * fecundity)
}

asrp_spawn_fp_raw <- asrp_fp_spawn %>%
  left_join(., asrp_culvs) %>%
  left_join(., asrp_reach_data) %>%
  filter(Habitat == "Side_Channel",
         Hist_salm == "Hist salmon")

asrp_spawn_fp_curr <- asrp_spawn_fp_raw %>%
  filter(Period %in% c('Curr', 'Both'),
         spawn_dist == 'Yes' & NEAR_DIST < 5) %>%
  group_by(noaaid, year, Scenario_num) %>%
  summarize(Length_sc_curr = sum(Length_sc, na.rm = TRUE)) %>%
  ungroup()

asrp_spawn_fp_hist <- asrp_spawn_fp_raw %>%
  filter(Period %in% c('Hist', 'Both'),
         spawn_dist == 'Yes' & NEAR_DIST < 500) %>%
  group_by(noaaid, year, Scenario_num) %>%
  summarize(Length_sc_hist = sum(Length_sc, na.rm = TRUE)) %>%
  ungroup()


asrp_spawn_fp <- full_join(asrp_spawn_fp_curr, asrp_spawn_fp_hist) %>%
  left_join(., asrp_reach_data) %>%
  left_join(., asrp_culvs) %>%
  mutate(Length_sc_curr = ifelse(is.na(Length_sc_curr),
                                 0,
                                 Length_sc_curr),
         Length_sc_hist = ifelse(is.na(Length_sc_hist),
                                 0,
                                 Length_sc_hist),
         Length_sc = ifelse(Floodplain == 'y',
                            Length_sc_curr + ((Length_sc_hist - Length_sc_curr) * rest_perc * fp_intensity_scalar),
                            Length_sc_curr))
if (fishtype != 'steelhead') {
  asrp_spawn_fp %<>%
         mutate(eggs = Length_sc * pass_tot_asrp * PR_redd_density / 1000 * fecundity)
} else {
  asrp_spawn_fp %<>%
    mutate(spawn_area = ifelse(LW == 'y' | lc == 'Forest',
                      (Length_sc / (2 * psp_hwls)) * 2 * (2 * .5),
                      (Length_sc / (2 * psp_lwls)) * 2 * (2 * .5)),
           eggs = spawn_area * pass_tot_asrp * fecundity)
}

if (fishtype == "spring_chinook") {
  asrp_spawn_fp %<>%
    filter(Subbasin_num %in% schino_subs)
}

asrp_spawn_lr_year <- lapply(scenario.years, function(r) {
  lgr_sp_area_asrp %>% 
    mutate(year = r)
}) %>%
  do.call('rbind',.)

asrp_spawn_lr <- lapply(scenario.nums, function(n){
  asrp_spawn_lr_year %>%
    mutate(Scenario_num = as.character(n))
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", 'dev_and_climate', growth_scenarios)),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_scenarios) & 
             year %in% c(2040, 2080))) %>%
  left_join(., asrp_culvs) %>%
  left_join(., asrp_reach_data) %>%
  mutate(
    spawn_area = ifelse(Scenario_num == 'hist_test',
                        spawn_area_hist,
                        spawn_area),
    eggs = ifelse(LW == 'y',
                  spawn_area * pass_tot_asrp / redd_area * fecundity * (1 + (wood_spawn_mult - 1) * rest_perc * 
                                                                          wood_intensity_scalar),
                  spawn_area * pass_tot_asrp / redd_area * fecundity)) %>%
  ungroup()

if (run_single_action == 'no') {
  asrp_spawn_lr %<>%
    filter(!Scenario_num %in% single_action_scenarios)
}

asrp_spawn_tot <- bind_rows(asrp_spawn_ss, asrp_spawn_fp, asrp_spawn_lr) %>%
  group_by(Subbasin_num, year, Scenario_num) %>%
  summarize(eggs = sum(eggs, na.rm = T),
            adults = sum(eggs / fecundity * adult_per_redd, na.rm = T)) %>%
  ungroup() %>%
  gather(life.stage, capacity, c(eggs, adults))


egg_cap_weight_asrp <- bind_rows(asrp_spawn_ss, asrp_spawn_fp, asrp_spawn_lr) %>%
  group_by(Subbasin_num, noaaid, year, Scenario_num) %>% 
  summarize(eggs = sum(eggs, na.rm = T)) %>% # egg cap per noaaid
  group_by(Subbasin_num, year, Scenario_num) %>%
  mutate(eggs_by_sub = sum(eggs), # egg cap per subbasin
         eggs_weight = eggs / eggs_by_sub) %>% # egg cap weights
  select(-eggs, -eggs_by_sub)

rm(asrp_spawn_fp, asrp_spawn_fp_curr, asrp_spawn_fp_hist, asrp_spawn_fp_raw, asrp_spawn_lr, asrp_spawn_lr_year, asrp_spawn_ss, asrp_fp_spawn, asrp_ss_spawn)

