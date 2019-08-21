data <- bind_rows(ss_2, lr_2, bw2, fp2) %>%
  left_join(wood_data %>%
              gather(life.stage, woodmult, woodmult_s:woodmult_w) %>%
              mutate(life.stage = ifelse(life.stage == "woodmult_s", 
                                         "summer", 
                                         "winter")))
if (fishtype == "steelhead") {
  data <- data %>%
    bind_rows(., data %>%
                mutate(life.stage = ifelse(life.stage == "summer", 
                                           "summer.2", 
                                           "winter.2")))}

if (fishtype %in% c('spring_chinook', 'fall_chinook')) {
  data <- data %>%
    bind_rows(., data %>% 
                mutate(life.stage = ifelse(life.stage == 'summer',
                                           'summer.2',
                                           'winter.2'))) %>%
    select(-curr.tempmult, -hist.tempmult) %>%
    left_join(., flowline %>% 
                select(noaaid, curr.tempmult, hist.tempmult), 
              by = "noaaid") %>%
    mutate(curr.tempmult = ifelse(life.stage %in% c('summer', 'winter'),
                                  1,
                                  curr.tempmult),
           hist.tempmult = ifelse(life.stage %in% c('summer', 'winter'),
                                  1,
                                  hist.tempmult))
}

data <- data %>%
  left_join(., density) %>%
  mutate(capacity = Area * Density) %>%
  left_join(., survival) %>%
  mutate(
    surv.base.summer = case_when(
      Habitat %in% LgRiver_habs & hab.scenario == "Historical" ~ surv.base * woodmult * hist.tempmult,
      !Habitat %in% LgRiver_habs & hab.scenario == "Historical" ~ wood.surv.base * hist.tempmult,
      Habitat %in% LgRiver_habs & hab.scenario %in% c("Wood", "FP_wood_comb") ~ surv.base * woodmult * curr.tempmult,
      !Habitat %in% LgRiver_habs & hab.scenario %in% c("Wood", "FP_wood_comb") ~ wood.surv.base * curr.tempmult,
      hab.scenario == "Shade" ~ surv.base * hist.tempmult,
      hab.scenario %in% c("Current", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Beaver", "Floodplain") ~
        surv.base * curr.tempmult
    ),
    surv.base.winter = case_when(
      Habitat %in% LgRiver_habs & hab.scenario %in% c("Historical", "Wood", "FP_wood_comb") ~ surv.base * woodmult,
      !Habitat %in% LgRiver_habs & hab.scenario %in% c("Historical", "Wood", "FP_wood_comb") ~ wood.surv.base,
      hab.scenario %in% c("Current", "Fine_sediment", "LR_bank", "LR_length", "Barriers", "Beaver", "Floodplain", "Shade") ~ surv.base
    ),
    surv.base = ifelse(life.stage %in% c("summer", "summer.2"),
                       surv.base.summer,
                       surv.base.winter)) %>%
  select(-surv.base.summer, -surv.base.winter) %>%
  group_by(Subbasin_num, hab.scenario, life.stage) %>%
  mutate(cap.sum.sub = sum(capacity, na.rm = T),
         cap.perc = capacity / cap.sum.sub,
         survival = cap.perc * surv.base) %>%
  ungroup()

data.lr <- data

data <- data %>%
  left_join(., data.lr %>%
              filter(Habitat %in% LgRiver_habs,
                     life.stage %in% c("summer", "summer.2")) %>%
              group_by(Subbasin_num, hab.scenario, life.stage) %>%
              summarize(survival = sum(survival, na.rm = T),
                        cap.perc = sum(cap.perc, na.rm = T)) %>%
              group_by(Subbasin_num, life.stage) %>%
              mutate(survival.curr = ifelse(hab.scenario == "Current", survival, 0),
                     survival.curr = sum(survival.curr, na.rm = T),
                     surv.adj = (survival - survival.curr) * cap.perc) %>%
              select(Subbasin_num, hab.scenario, surv.adj)) %>%
  group_by(Subbasin_num, hab.scenario, life.stage) %>%
  summarize(capacity = sum(capacity, na.rm = T),
            survival = sum(survival, na.rm = T),
            surv.adj = unique(surv.adj, na.rm = T)) %>%
  group_by(Subbasin_num, life.stage) %>%
  mutate(survival.curr = ifelse(hab.scenario == "Current", 
                                survival, 
                                0),
         survival.curr = sum(survival.curr, na.rm = T),
         survival = ifelse(life.stage %in% c("summer", "summer.2") & hab.scenario %in% c("LR_bank", "LR_length", "Shade") & 
                             Subbasin_num %in% mainstem.subs,
                           survival.curr + surv.adj, 
                           survival)) %>%
  select(hab.scenario, Subbasin_num, life.stage, capacity, survival) %>%
  bind_rows(spawn_tot) %>%
  bind_rows(prespawn.surv) %>%
  bind_rows(ef.surv) %>%
  ungroup()


scenarios <- unique(data$hab.scenario)
life.stage.nm <- c("egg.to.fry.survival", "adults.capacity", "eggs.capacity", "prespawn.survival", "summer.capacity", "summer.survival", 
                   "winter.capacity", "winter.survival", "summer.2.capacity", "summer.2.survival", "winter.2.capacity", "winter.2.survival")

life.stage <- life.stage.nm
stage_nm <- c("eggtofry_surv", "adults", "eggs", "prespawn_surv", "capacity_s", "surv_s", "capacity_w", "surv_w", "capacity_s_2", "surv_s_2", 
              "capacity_w_2", "surv_w_2")
stage_nums <- c(1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11)
ls.to.names <- data.frame(life.stage, stage_nm, stage_nums)

data.spread <- lapply(scenarios, function(x){
  y <- data %>%
    filter(hab.scenario == x,
           Subbasin_num %in% c(1:63),
           !Subbasin_num %in% c(50,51)) %>%
    select(-1) %>%
    gather(life.stage2, num, capacity:survival) %>%
    unite(life.stage, life.stage2, col = life.stage, sep = ".") %>%
    left_join(., ls.to.names) %>%
    full_join(., subbasin_names %>%
                select(Subbasin_num)) %>%
    spread(Subbasin_num, num) %>%
    filter(life.stage %in% life.stage.nm) %>%
    arrange(stage_nums) %>%
    
    select(-life.stage, -stage_nums)
  if (fishtype == "coho") {
    y <- y %>% 
      filter(!stage_nm %in% c("capacity_s_2", "surv_s_2", "capacity_w_2", "surv_w_2"))
  }
  if (fishtype %in% c("spring_chinook", 'fall_chinook')) {
    y <- y %>% 
      filter(!stage_nm %in% c("capacity_w", "surv_w", "capacity_s_2", "capacity_w_2", "surv_w_2"))
  }
  write.csv(y, file = file.path(outputs_hab, paste0(x, ".csv")))
}) %>%
  do.call('rbind',.)

if (dir.exists(file.path(outputs_hab, "outputs_long")) == F) {
  dir.create(file.path(outputs_hab, "outputs_long"), recursive = T)
}

if (run_asrp == "no") {
  write.csv(data, file.path(outputs_hab, "outputs_long", "habmodel_outputs.csv"))
}

rm(bw)
rm(bw_curr)
rm(bw_hist)

rm(data.lr)

rm(Floodplain_raw) 
rm(fp)
rm(fp_hist)
rm(fp_curr)
rm(fp_join)
rm(fp_spawn)
rm(fps_curr)
rm(fps_hist)
#rm(fps1)

rm(LgRiver)
rm(LgRiver_raw)
rm(LgRiver_raw_wood)
rm(lr)
rm(lr_curr)
rm(lr_hist)
rm(lrsp)
#rm(lrsp1)

rm(SmStream_raw)
rm(ss)
rm(ss_spawn)
#rm(ss_sp1)
