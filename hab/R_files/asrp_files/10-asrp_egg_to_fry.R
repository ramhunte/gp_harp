ef.surv.asrp <- egg_cap_weight_asrp %>%
  left_join(flowline %>%
              select(noaaid, Subbasin_num, ef_surv_current, ef_surv_hist)) %>%
  mutate(ef_surv_asrp = case_when(
    Scenario_num %in% c('fine_sed_test') ~ ef_surv_hist * eggs_weight,
    !Scenario_num %in% c('fine_sed_test') ~ ef_surv_current * eggs_weight)) %>%
  group_by(Subbasin_num, year, Scenario_num) %>%
  summarize(survival = sum(ef_surv_asrp, na.rm = T)) %>%
  mutate(life.stage = "egg.to.fry")
