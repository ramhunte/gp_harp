#### Productivity calculations ----

asrp_prod <- asrp_cap %>%
  left_join(., survival) %>%
  mutate(surv.base.asrp = ifelse(Habitat %in% LgRiver_habs,
                                 ifelse(life.stage %in% c("summer", "summer.2"),
                                        surv.base * woodmult_s_asrp * tempmult.asrp,
                                        surv.base * woodmult_w_asrp),
                                 ifelse(Habitat %in% SmStream_habs,
                                        ifelse(life.stage %in% c("summer", "summer.2"),
                                               ifelse(GSU %in% wood_gsu,
                                                      ifelse(forest == "y",
                                                             (surv.base + ((wood.surv.base - surv.base) * rest_perc_f * wood_intensity_scalar_f)) * 
                                                               tempmult.asrp,
                                                             (surv.base + ((wood.surv.base - surv.base) * rest_perc_nf * wood_intensity_scalar_nf)) *
                                                               tempmult.asrp),
                                                      surv.base * tempmult.asrp),
                                               ifelse(GSU %in% wood_gsu,
                                                      ifelse(forest == "y",
                                                             (surv.base + ((wood.surv.base - surv.base) * rest_perc_f * wood_intensity_scalar_f)),
                                                             (surv.base + ((wood.surv.base - surv.base) * rest_perc_nf * wood_intensity_scalar_nf))),
                                                      surv.base)),
                                        surv.base))) %>%
  group_by(Subbasin_num, life.stage) %>%
  mutate(cap.sum.sub = sum(capacity, na.rm = T),
         cap.perc = capacity / cap.sum.sub,
         productivity = cap.perc * surv.base.asrp) %>%
  summarize(capacity = sum(capacity, na.rm = T),
            productivity = sum(productivity, na.rm = T)) %>%
  ungroup()