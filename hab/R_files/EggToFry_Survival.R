# Purpose: Egg to fry survival per reach is calculated in the spatial model
# This is where we weight egg to fry survival by egg capacity, then summarize per subbasin.

# Calculate the egg cap weighted egg to fry survival per scenario-subbasin ----
# Weighting by egg capacity (egg_cap_weight) is calculated at the end of the spawn.R script

curr.ef <- c("Barriers", "Beaver", "Current", "Shade", "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
hist.ef <- c("Historical", "Fine_sediment")

ef.surv <- flowline %>%
  select(noaaid, Subbasin_num, ef_surv_current:ef_surv_hist) %>%
  left_join(egg_cap_weight) %>%
  mutate(ef_surv_current_w = ef_surv_current * eggs_weight,
         ef_surv_hist_w = ef_surv_hist * eggs_weight) %>%
  group_by(hab.scenario, Subbasin_num) %>%
  summarize(survival_curr = sum(ef_surv_current_w, na.rm = T),
            survival_hist = sum(ef_surv_hist_w, na.rm = T)) %>%
  filter(!is.na(hab.scenario)) %>%
  mutate(survival = ifelse(hab.scenario %in% curr.ef,
                           survival_curr,
                           survival_hist),
         life.stage = "egg.to.fry") %>%
  select(-survival_curr, -survival_hist)

