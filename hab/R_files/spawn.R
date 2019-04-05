# Floodplain side channel spawning ----

fp_spawn <- fp %>%
  filter(Habitat == "Side_Channel",
         ifelse(Period == "Hist",
                spawn_dist == "Yes" & NEAR_DIST < 500 | Subbasin_num %in% mainstem.subs,
                spawn_dist == "Yes" & NEAR_DIST < 5 | Subbasin_num %in% mainstem.subs))

if (fishtype == "spring_chinook") {
  fp_spawn <- fp_spawn %>%
    filter(Subbasin_num %in% schino_subs)
}

curr_fp_spawn <- c("Current", "Fine_sediment", "LR_bank", "LR_length", "Shade", "Wood", "Beaver", "Barriers")
hist_fp_spawn <- c("Floodplain", "FP_wood_comb", "Historical")

fps_curr <- lapply(curr_fp_spawn, function(x) {
  fp_spawn %>%
    filter(Period %in% c("Curr", "Both")) %>%
    mutate(hab.scenario = x)
}) %>%
  do.call('rbind',.)

fps_hist <- lapply(hist_fp_spawn, function(y) {
  fp_spawn %>% 
    filter(Period %in% c("Hist", "Both")) %>%
    mutate(hab.scenario = y)
}) %>% 
  do.call('rbind',.)

fps1 <- bind_rows(fps_curr, fps_hist) %>%
  mutate(
    eggs = case_when(
      hab.scenario %in% c("Current", "Fine_sediment", "LR_bank", "LR_length", "Shade", "Wood", "Beaver") ~ 
        Length_sc * pass_tot * PR_redd_density / 1000 * fecundity,
      hab.scenario %in% c("Floodplain", "FP_wood_comb", "Barriers", "Historical") ~
        Length_sc * pass_tot_natural * PR_redd_density / 1000 * fecundity),
    adults = eggs / fecundity * adult_per_redd
  )

# Small stream spawning ----
ss_spawn <- lapply(diag_scenarios, function(z) {
  ss %>%
    filter(slope < .03) %>%
    mutate(hab.scenario = z)
}) %>%
  do.call('rbind',.)

ss_sp1 <- ss_spawn %>%
  mutate(
    eggs = case_when(
      hab.scenario %in% c("Current", "Fine_sediment", "LR_bank", "LR_length", "Shade", "Floodplain") ~
        ifelse(slope < .01, 
               Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
               ifelse(lc == "Forest", 
                      Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * curr_beaver_mult, 
                      Shape_Length * pass_tot * NF_redd_density / 1000 * fecundity * curr_beaver_mult)),
      hab.scenario == "Barriers" ~ 
        ifelse(slope < .01, 
               Shape_Length * pass_tot_natural * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
               ifelse(lc == "Forest", 
                      Shape_Length * pass_tot_natural * F_redd_density / 1000 * fecundity * curr_beaver_mult,
                      Shape_Length * pass_tot_natural * NF_redd_density / 1000 * fecundity * curr_beaver_mult)),
      hab.scenario %in% c("Wood", "FP_wood_comb") ~
        ifelse(slope < .01, 
               Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * curr_beaver_mult,
               Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * curr_beaver_mult),
      hab.scenario == "Beaver" ~
        ifelse(slope < .01, 
               Shape_Length * pass_tot * PR_redd_density / 1000 * fecundity * hist_beaver_mult,
               ifelse(lc == "Forest", 
                      Shape_Length * pass_tot * F_redd_density / 1000 * fecundity * hist_beaver_mult,
                      Shape_Length * pass_tot * NF_redd_density / 1000 * fecundity * hist_beaver_mult)),
      hab.scenario == "Historical" ~
        ifelse(slope < .01, 
               Shape_Length * pass_tot_natural * PR_redd_density / 1000 * fecundity * hist_beaver_mult,
               Shape_Length * pass_tot_natural * F_redd_density / 1000 * fecundity * hist_beaver_mult)
    ),
    adults = eggs / fecundity * adult_per_redd
  )

# Large river spawning ----

lrsp <- lapply(diag_scenarios, function(a) {
  lgr_spawning_area %>% 
    mutate(hab.scenario = a)
}) %>%
  do.call('rbind',.)

lrsp1 <- lrsp %>%
  mutate(
    eggs = case_when(
      hab.scenario %in% c("Current", "Fine_sediment", "LR_length", "Shade", "Beaver", "LR_bank", "Floodplain") ~ 
        spawn_area_passable / redd_area * fecundity,
      hab.scenario %in% c("Wood", "FP_wood_comb") ~ spawn_area_passable / redd_area * fecundity * wood_spawn_mult,
      hab.scenario == "Barriers" ~ spawn_area_passable_nat / redd_area * fecundity,
      hab.scenario == "Historical" ~ spawn_area_passable_hist / redd_area * fecundity * wood_spawn_mult
    ),
    adults = eggs / fecundity * adult_per_redd
  )


spawn_tot <- bind_rows(fps1, ss_sp1, lrsp1) %>%
  group_by(hab.scenario, Subbasin_num) %>%
  summarize(eggs = sum(eggs, na.rm = T),
            adults = sum(adults, na.rm = T)) %>%
  gather(life.stage, capacity, c(eggs, adults)) %>%
  ungroup()
