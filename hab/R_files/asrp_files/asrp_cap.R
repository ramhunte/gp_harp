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