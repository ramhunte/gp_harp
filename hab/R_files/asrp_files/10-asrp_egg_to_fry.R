ef.surv.asrp <- egg_cap_weight_asrp %>%
  left_join(flowline %>%
              select(noaaid, Subbasin_num, ef_surv_current)) %>%
  mutate(ef_surv_asrp = ef_surv_current * eggs_weight) %>%
  group_by(Subbasin_num, year, Scenario_num) %>%
  summarize(survival = sum(ef_surv_asrp, na.rm = T)) %>%
  mutate(life.stage = "egg.to.fry")
