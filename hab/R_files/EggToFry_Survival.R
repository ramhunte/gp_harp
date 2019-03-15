# Purpose: Egg to fry survival per reach is calculated in the spatial model
# This is where we weight egg to fry survival by egg capacity, then summarize per subbasin.


# Create spawning capacity per reach (noaaid) to weight egg to fry surv ---

e2f_weight <- bind_rows(fps1, ss_sp1, lrsp1) %>%
  group_by(hab.scenario, Subbasin_num, noaaid) %>% 
  summarize(eggs = sum(eggs, na.rm = T)) %>% # egg cap per noaaid
  group_by(hab.scenario, Subbasin_num) %>%
  mutate(eggs_by_sub = sum(eggs), # egg cap per subbasin
         egg_to_fry_weight = eggs / eggs_by_sub) %>% # egg cap weights
  select(-eggs, -eggs_by_sub)

# Calculate the egg cap weighted egg to fry survival per scenario-subbasin ----

curr.ef <- c("Barriers", "Beaver", "Current", "Shade", "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
hist.ef <- c("Historical", "Fine_sediment")

ef.surv <- flowline %>%
  select(noaaid, Subbasin_num, ef_surv_current:ef_surv_hist) %>%
  left_join(e2f_weight) %>%
  mutate(ef_surv_current_w = ef_surv_current * egg_to_fry_weight,
         ef_surv_hist_w = ef_surv_hist * egg_to_fry_weight) %>%
  group_by(hab.scenario, Subbasin_num) %>%
  summarize(survival_curr = sum(ef_surv_current_w, na.rm = T),
            survival_hist = sum(ef_surv_hist_w, na.rm = T)) %>%
  filter(!is.na(hab.scenario)) %>%
  mutate(survival = ifelse(hab.scenario %in% curr.ef,
                           survival_curr,
                           survival_hist),
         life.stage = "egg.to.fry") %>%
  select(-survival_curr, -survival_hist)

