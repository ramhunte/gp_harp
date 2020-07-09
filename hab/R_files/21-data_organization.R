asrp_results_inputs <- bind_rows(asrp_prod, asrp_spawn_tot,prespawn_asrp, ef.surv.asrp) %>%
  mutate(hab.scenario = case_when(
    Scenario_num %in% diag_scenarios ~ Scenario_num,
    !Scenario_num %in% diag_scenarios ~ paste0(Scenario_num, '_', year)))

data <- asrp_results_inputs %>% 
  select(-year, -Scenario_num)
assign('data', data , envir = .GlobalEnv)

life.stage.nm <- c("egg.to.fry.survival", "adults.capacity", "eggs.capacity", "prespawn.survival", "summer.capacity", "summer.survival", 
                   "winter.capacity", "winter.survival","winter.movement", "summer.2.capacity", "summer.2.survival", "winter.2.capacity", 
                   "winter.2.survival", 'fry.colonization.capacity', 'fry.colonization.survival')

life.stage <- life.stage.nm
stage_nm <- c("eggtofry_surv", "adults", "eggs", "prespawn_surv", "capacity_s", "surv_s", "capacity_w", "surv_w", "movement", "capacity_s_2",
              "surv_s_2", "capacity_w_2", "surv_w_2", 'fry_colonization_cap', 'fry_colonization_surv')
stage_nums <- c(1, 2, 3, 8, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)
ls.to.names <- data.frame(life.stage, stage_nm, stage_nums)

asrp_results <- asrp_results_inputs %>%
  left_join(., asrp_mvmt) %>%
  mutate(movement = ifelse(Scenario_num %in% c(food_scenarios, "Current", 'dev_and_climate', 'Barriers', 'Shade', 'LR',
                                               'Fine_sediment', single_action_scenarios[!single_action_scenarios %in% single_action_mvmt_scenarios]),
                           11,
                           ifelse(Scenario_num %in% c('Floodplain', 'Beaver', 'FP_wood_comb', 'Historical'),
                                  3,
                                  ifelse(Scenario_num == 'Wood',
                                         7,
                                         movement)))) %>%
  gather(life.stage2, num, c(capacity, survival, movement)) %>%
  unite(life.stage, life.stage2, col = life.stage, sep = ".") %>%
  left_join(.,ls.to.names) %>%
  filter(!Subbasin_num %in% c(50, 51)) %>%
  full_join(., subbasin_names %>%
              select(Subbasin_num)) %>%
  filter(Subbasin_num %in% 1:63) %>%
  spread(Subbasin_num, num) %>%
  filter(life.stage %in% life.stage.nm) %>%
  arrange(stage_nums) %>%
  select(-life.stage, -stage_nums, -hab.scenario) %>%
  mutate(
    hab.scenario = case_when(
      Scenario_num %in% diag_scenarios ~ Scenario_num,
      !Scenario_num %in% diag_scenarios ~
        paste('ASRP', Scenario_num, year, sep = '_'))) %>%
  ungroup() %>%
  select(-year, -Scenario_num)

if (fishtype == "coho") {
  asrp_results %<>%
    filter(!stage_nm %in% c("capacity_s_2", "surv_s_2", "capacity_w_2", "surv_w_2"))
}

if (fishtype == "steelhead") {
  asrp_results %<>%
    filter(!stage_nm == 'movement')
}

if (fishtype %in% c('spring_chinook','fall_chinook')) {
  asrp_results %<>%
    filter(!stage_nm %in% c("capacity_s_2", "capacity_w_2", "surv_w_2", 'capacity_w', 'surv_w'))
}

if (fishtype == 'chum') {
  asrp_results %<>%
    filter(stage_nm %in% c('adults', 'eggs', 'eggtofry_surv', 'prespawn_surv', 'fry_colonization_cap', 'fry_colonization_surv', 'surv_s'))
}


asrp_results_outputs <- lapply(unique(asrp_results$hab.scenario), function(b) {
  f <- asrp_results %>%
    filter(hab.scenario == b) %>%
    ungroup() %>%
    select(-hab.scenario)
  write.csv(f, file = file.path(outputs_hab, paste0(b, ".csv")))
})

write.csv(data, file.path(outputs_hab, "outputs_long", "habmodel_outputs.csv"))
