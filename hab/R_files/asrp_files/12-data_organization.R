asrp_results_inputs <- bind_rows(asrp_prod, asrp_spawn_tot,prespawn_asrp, ef.surv.asrp) %>%
  mutate(hab.scenario = paste0(Scenario_num, '_', year))

data <- data %>%
  bind_rows(asrp_results_inputs) %>% 
  select(-year, -Scenario_num)
assign('data', data , envir = .GlobalEnv)

life.stage.nm <- c("egg.to.fry.survival", "adults.capacity", "eggs.capacity", "prespawn.survival", "summer.capacity", "summer.survival", 
                   "winter.capacity", "winter.survival","winter.movement", "summer.2.capacity", "summer.2.survival", "winter.2.capacity", 
                   "winter.2.survival")

life.stage <- life.stage.nm
stage_nm <- c("eggtofry_surv", "adults", "eggs", "prespawn_surv", "capacity_s", "surv_s", "capacity_w", "surv_w", "movement", "capacity_s_2",
              "surv_s_2", "capacity_w_2", "surv_w_2")
stage_nums <- c(1, 2, 3, 8, 4, 5, 6, 7, 9, 10, 11, 12, 13)
ls.to.names <- data.frame(life.stage, stage_nm, stage_nums)

asrp_results <- asrp_results_inputs %>%
  left_join(., asrp_mvmt) %>%
  mutate(movement = ifelse(Scenario_num %in% c("Current_asrp", 'dev_and_climate', 'barrier_test'),
                           11,
                           ifelse(Scenario_num %in% c('fp_test', 'beaver_test', 'lw_flp_test'),
                                  3,
                                  ifelse(Scenario_num == 'wood_test',
                                         7,
                                         movement)))) %>%
  # mutate(movement = 11) %>%
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
  mutate(ASRP = "ASRP") %>%
  unite(hab.scenario, ASRP, Scenario_num, year, sep = '_') %>%
  mutate(hab.scenario = ifelse(hab.scenario == "ASRP_Current_asrp_2019",
                               "ASRP_Current_asrp",
                               hab.scenario))

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


asrp_results_outputs <- lapply(unique(asrp_results$hab.scenario), function(b) {
  f <- asrp_results %>%
    filter(hab.scenario == b) %>%
    ungroup() %>%
    select(-hab.scenario)
  write.csv(f, file = file.path(outputs_hab, paste0(b, ".csv")))
})

