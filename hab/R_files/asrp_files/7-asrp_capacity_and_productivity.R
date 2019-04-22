# Capacity calculation ----

asrp_cap <- bind_rows(asrp_lr, asrp_ss, asrp_fp)

if (fishtype == "steelhead") {
  asrp_cap %<>%
    bind_rows(., asrp_cap %>%
                filter(life.stage %in% c("summer", "winter")) %>%
                mutate(life.stage = ifelse(life.stage == "summer",
                                           "summer.2",
                                           "winter.2")))
}

asrp_cap %<>%
  left_join(., density) %>%
  mutate(capacity = Area * Density)

source("hab/R_files/asrp_files/8-movement.R", local = TRUE)

# Productivity calculations ----

asrp_prod <- asrp_cap %>%
  left_join(., survival) %>%
  mutate(
    surv.base.asrp.summer = case_when(
      Habitat %in% LgRiver_habs ~ surv.base * woodmult_s_asrp * tempmult.asrp,
      !Habitat %in% LgRiver_habs ~
        ifelse(LW == 'y',
               (surv.base + ((wood.surv.base - surv.base) * rest_perc * wood_intensity_scalar)) * tempmult.asrp,
               surv.base * tempmult.asrp)
    ),
    surv.base.asrp.winter = case_when(
      Habitat %in% LgRiver_habs ~ surv.base * woodmult_w_asrp,
      !Habitat %in% LgRiver_habs ~ 
        ifelse(LW == 'y',
               (surv.base + ((wood.surv.base - surv.base) * rest_perc * wood_intensity_scalar)),
               surv.base)
    ),
    surv.base.asrp = ifelse(life.stage %in% c("summer", "summer.2"),
                            surv.base.asrp.summer,
                            surv.base.asrp.winter)
  ) %>%
  group_by(Subbasin_num, life.stage, year, Scenario_num) %>%
  mutate(cap.sum.sub = sum(capacity, na.rm = T),
         cap.perc = capacity / cap.sum.sub,
         survival = cap.perc * surv.base.asrp) %>%
  summarize(capacity = sum(capacity, na.rm = T),
            survival = sum(survival, na.rm = T)) %>%
  ungroup()
