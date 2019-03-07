# Attach GSU information to culverts so that we are able to affect passage percentage when barriers are removed from particular GSUs ----
asrp_culvs <- asrp_culvs_raw %>%
  left_join(., list.files(path = Inputs, pattern = "culvs_gsu_", full.names = T) %>%
              read.csv(.) %>%
              select(OBS_UNIQ, GSU),  
            by = "OBS_UNIQ") %>%
  mutate(pass_tot_asrp = ifelse(GSU %in% barrier_gsu & !FeatureTyp == "Natural",
                                1,
                                FishPass)) %>%
  group_by(noaaid) %>%
  summarize(pass_tot_asrp = prod(pass_tot_asrp, na.rm = T)) %>%
  full_join(flowline %>%
              select(noaaid)) %>%
  mutate(pass_tot_asrp = ifelse(is.na(pass_tot_asrp),
                                1,
                                pass_tot_asrp)) %>%
  ungroup()
