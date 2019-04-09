#### Pool/riffle ratios based on landcover and slope defined: ----
lc = c(rep(c("Agriculture", "BareShrubGrass", "Developed", "Forest", "Wetland", "Reference"), times = 3))
slope.class = c(rep("low", times = 6), rep("med", times = 6), rep("high", times = 6))
pool.perc = c(c(.92, .83, .74, .75, .89, .81), c(.60, .50, .51, .48, .53, .66), c(.31, .35, .54, .34, NA, .35))
ss.dist = data.frame(lc, slope.class, pool.perc)

SmStream_raw <- flowline %>% 
  filter(Habitat == 'SmStream',
         !fp_overlap == "Yes") %>%
  select(noaaid, curr_temp, hist_temp, temp_diff, curr.tempmult, hist.tempmult, Subbasin_num, pass_tot, Reach, Shape_Length, slope, lc, 
         spawn_dist, species, both_chk, pass_tot_natural, tm_2040, tm_2080, tm_2040_cc_only, tm_2080_cc_only, can_ang, area_s, area_w,
         Reach_low, width_s_hist, width_w_hist, wet_width)

ss <- SmStream_raw %>%
  mutate(slope.class = case_when(slope < .02 ~ "low",
                                 slope >= .02 & slope < .04 ~ "med",
                                 slope >= .04 ~ "high"),
         lc = ifelse(lc == "", 
                     "Reference", 
                     as.character(lc))) %>%
  filter(spawn_dist == "Yes")

if (fishtype == "spring_chinook") {
  ss <- ss %>%
    filter(Subbasin_num %in% schino_subs)
}

assign('asrp_ss_raw', ss , envir = .GlobalEnv) 

ss_2 <- lapply(diag_scenarios, function(x) {
  ss %>%
    mutate(hab.scenario = x)
}) %>% 
  do.call('rbind',.) %>%
  mutate(lc = ifelse(hab.scenario %in% c("Wood", "FP_wood_comb", "Historical"),
                     "Reference",
                     lc)) %>%
  left_join(., ss.dist) %>%
  left_join(., wood_data) %>% 
  mutate(Pool = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers") ~ area_s * pool.perc * 
                            curr.tempmult * curr_beaver_mult,
                          hab.scenario == "Shade" ~ area_s * pool.perc * hist.tempmult * curr_beaver_mult,
                          hab.scenario %in% c("Wood", "FP_wood_comb") ~ area_s * pool.perc * curr.tempmult * woodmult_s * curr_beaver_mult,
                          hab.scenario == "Beaver" ~ area_s * pool.perc * hist_beaver_mult * curr.tempmult,
                          hab.scenario == "Historical" ~ (Shape_Length * width_s_hist) / 10000 * pool.perc * hist_beaver_mult * hist.tempmult *
                            woodmult_s),
         Riffle = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers") ~ area_s * 
                              (1 - pool.perc) * curr.tempmult * curr_beaver_mult,
                            hab.scenario == "Shade" ~ area_s * (1 - pool.perc) * hist.tempmult * curr_beaver_mult,
                            hab.scenario %in% c("Wood", "FP_wood_comb") ~ area_s * (1 - pool.perc) * curr.tempmult * woodmult_s * curr_beaver_mult,
                            hab.scenario == "Beaver" ~ area_s * (1 - pool.perc) * hist_beaver_mult * curr.tempmult ,
                            hab.scenario == "Historical" ~ (Shape_Length * width_s_hist) / 10000 * (1 - pool.perc) * hist_beaver_mult * hist.tempmult * woodmult_s),
         Beaver.Pond = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers") ~ 
                                   (Shape_Length * curr_pond_area_per_m) / 10000 * curr.tempmult,
                                 hab.scenario == "Shade" ~ (Shape_Length * curr_pond_area_per_m) / 10000 * hist.tempmult,
                                 hab.scenario %in% c("Wood", "FP_wood_comb") ~ (Shape_Length * curr_pond_area_per_m) / 10000 * curr.tempmult * woodmult_s,
                                 hab.scenario == "Beaver" ~ (Shape_Length * hist_pond_area_per_m)/10000 * curr.tempmult,
                                 hab.scenario == "Historical" ~ (Shape_Length * hist_pond_area_per_m) / 10000 * hist.tempmult * woodmult_s),
         winter.pool = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers", "Shade") ~ 
                                   area_w * pool.perc * winter_pool_scalar_warm * curr_beaver_mult,
                                 hab.scenario %in% c("Wood", "FP_wood_comb") ~ area_w * pool.perc * winter_pool_scalar_warm * woodmult_w * curr_beaver_mult,
                                 hab.scenario == "Beaver" ~ area_w * pool.perc * winter_pool_scalar_warm * hist_beaver_mult,
                                 hab.scenario == "Historical" ~ (Shape_Length * width_w_hist) / 10000 * pool.perc * winter_pool_scalar_warm * woodmult_w * hist_beaver_mult),
         winter.riffle = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers", "Shade") ~
                                     area_w * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * area_w * pool.perc * curr_beaver_mult),
                                   hab.scenario %in% c("Wood", "FP_wood_comb") ~ 
                                     (area_w * (1 - pool.perc) * curr_beaver_mult + ((1 - winter_pool_scalar_warm) * area_w * 
                                                                                       pool.perc * curr_beaver_mult)) * woodmult_w,
                                   hab.scenario == "Beaver" ~ 
                                     area_w * (1 - pool.perc) * hist_beaver_mult + ((1 - winter_pool_scalar_warm) * area_w * pool.perc * hist_beaver_mult),
                                   hab.scenario == "Historical" ~ ((Shape_Length * width_w_hist) / 10000 * (1 - pool.perc) * hist_beaver_mult + 
                                                                     ((1 - winter_pool_scalar_warm) * (Shape_Length * width_w_hist) / 10000 * pool.perc *hist_beaver_mult)) * 
                                     woodmult_w),
         winter.beaver.pond = case_when(hab.scenario %in% c("Current", "Fine_sediment", "Floodplain", "LR_bank", "LR_length", "Barriers", "Shade") ~
                                          (Shape_Length * curr_pond_area_per_m) / 10000,
                                        hab.scenario %in% c("Wood", "FP_wood_comb") ~  (Shape_Length * curr_pond_area_per_m) / 10000 * woodmult_w,
                                        hab.scenario == "Beaver" ~ (Shape_Length * hist_pond_area_per_m) / 10000,
                                        hab.scenario == "Historical" ~ (Shape_Length * hist_pond_area_per_m)/10000 * woodmult_w))%>%
  gather(Habitat, Area, Pool:winter.beaver.pond) %>%
  mutate(life.stage = case_when(Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ "summer",
                                Habitat %in% c("winter.pool", "winter.riffle", "winter.beaver.pond") ~ "winter"),
         Habitat = case_when(Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ Habitat,
                             Habitat == "winter.pool" ~ "Pool",
                             Habitat == "winter.riffle" ~ "Riffle",
                             Habitat == "winter.beaver.pond" ~ "Beaver.Pond"),
         Area = ifelse(hab.scenario %in% c("Barriers", "Historical") & pass_tot_natural == 0,
                       0,
                       ifelse(!hab.scenario %in% c("Barriers", "Historical") & pass_tot == 0,
                              0,
                              Area))) %>%
  select(noaaid, Subbasin_num, hab.scenario, Habitat, Area, life.stage, curr.tempmult, hist.tempmult, pass_tot, both_chk, pass_tot_natural)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  ss_2 <- ss_2 %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area * chino_mult,
                         Area))
}
