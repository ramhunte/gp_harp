# Calculate area of each reach in each scenario ----
lc = c(rep(c("Agriculture", "BareShrubGrass", "Developed", "Forest", "Wetland", "Reference"), times = 3))
slope.class = c(rep("low", times = 6), rep("med", times = 6), rep("high", times = 6))
pool.perc = c(c(.92, .83, .74, .75, .89, .81), c(.60, .50, .51, .48, .53, .66), c(.31, .35, .54, .34, NA, .35))
ss.dist = data.frame(lc, slope.class, pool.perc)

lc.ref = c(rep("Reference", times = 3))
slope.class = c("low", "med", "high")
pool.perc.ref = c(.81, .66, .35)
ss.dist.ref = data.frame(lc.ref, slope.class, pool.perc.ref)

asrp_ss_raw <- flowline %>%
  filter(Habitat == "SmStream",
         !fp_overlap == 'Yes',
         spawn_dist == 'Yes') %>%
  mutate(slope.class = case_when(slope < .02 ~ 'low',
                                 slope >= .02 & slope < .04 ~ 'med',
                                 slope >= .04 ~ 'high'),
         lc = ifelse(lc == '',
                     'Reference',
                     as.character(lc))) %>%
  select(noaaid, Subbasin_num, Reach, Shape_Length, slope, lc, spawn_dist, species, both_chk,
         Reach_low, slope.class, Habitat, wet_width, can_ang, chino_mult, width_s, width_s_2040, width_s_2080, width_s_hist, width_w, width_w_2040,
         width_w_2080, width_w_hist)

asrp_ss_year <- lapply(scenario.years, function(x) {
  asrp_ss_raw %>%
    mutate(year = x)
}) %>%
  do.call('rbind',.)

asrp_ss_scenario <- lapply(scenario.nums, function(y) {
  asrp_ss_year %>%
    mutate(Scenario_num = y)
}) %>%
  do.call('rbind',.) %>%
  filter(!(year == 2019 & Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", 'dev_and_climate', growth_scenarios, 'cc_only', 
                                              'rip_and_climate', 'fp_temp', 'rip_and_flp')),
         !(Scenario_num %in% c(single_action_scenarios[!single_action_scenarios %in% growth_scenarios], diag_scenarios) &
             year %in% c(2040, 2080)))

assign('asrp_ss_spawn', asrp_ss_scenario, envir = .GlobalEnv)

asrp_ss <- asrp_ss_scenario %>%
  # left_join(., edt_width) %>%
  left_join(., asrp_reach_data) %>%
  left_join(., ss.dist) %>%
  left_join(., ss.dist.ref) %>%
  left_join(., asrp_culvs) %>%
  rename(width_s_curr = width_s,
         width_w_curr = width_w) %>%
  mutate(
    width_s = case_when(
      year == 2019 ~ 
        ifelse(Scenario_num == 'Historical',
               width_s_hist,
               width_s_curr),
      year == 2040 ~ width_s_2040,
      year == 2080 ~ width_s_2080),
    width_w = case_when(
      year == 2019 ~
        ifelse(Scenario_num == 'Historical',
               width_w_hist,
               width_w_curr),
      year == 2040 ~ width_w_2040,
      year == 2080 ~ width_w_2080),
    tempmult.asrp = ifelse(species %in% c("spring_chinook", "fall_chinook", 'chum'), # Added because of spring chinook w/temp survival 
                           1,
                           tempmult.asrp),
    width_s = ifelse(is.na(width_s),
                     wet_width,
                     width_s),
    width_w = ifelse(is.na(width_w),
                     wet_width,
                     width_w),
    area_s = (Shape_Length * width_s) / 10000,
    area_w = (Shape_Length * width_w) / 10000,
    pool.perc.asrp = ifelse(LW == "y",
                            ifelse(is.na(pool.perc),
                                   pool.perc.ref * rest_perc * wood_intensity_scalar,
                                   pool.perc + ((pool.perc.ref - pool.perc) * rest_perc * wood_intensity_scalar)),
                            pool.perc),
    riffle.perc.asrp = ifelse(is.na(pool.perc),
                              (1 - pool.perc.asrp) * rest_perc,
                              1 - pool.perc.asrp),
    beaver_mult_asrp = ifelse(Beaver == "y",
                              curr_beaver_mult - ((curr_beaver_mult - hist_beaver_mult) * rest_perc * beaver_intensity_scalar),
                              curr_beaver_mult),
    
    # Begin habitat area calculations ----
    
    Pool = area_s * pool.perc.asrp * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp,
    Riffle = area_s * riffle.perc.asrp * tempmult.asrp * woodmult_s_asrp * beaver_mult_asrp, 
    Beaver.Pond = ifelse(Beaver == "y",
                         ((Shape_Length * curr_pond_area_per_m / 10000) + (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 
                                                                             10000 * rest_perc * beaver_intensity_scalar)) * 
                           tempmult.asrp * woodmult_s_asrp,
                         (Shape_Length * curr_pond_area_per_m) / 10000 * tempmult.asrp * woodmult_s_asrp),
    winter.pool = area_w * pool.perc.asrp * winter_pool_scalar_warm * woodmult_w_asrp * beaver_mult_asrp,
    winter.riffle = (area_w * riffle.perc.asrp * beaver_mult_asrp + ((1 - winter_pool_scalar_warm) * area_w * pool.perc.asrp) * beaver_mult_asrp) * 
      woodmult_w_asrp,
    winter.beaver.pond = ifelse(Beaver == "y",
                                ((Shape_Length * curr_pond_area_per_m / 10000) + 
                                   (Shape_Length * (hist_pond_area_per_m - curr_pond_area_per_m) / 10000 * rest_perc * 
                                      beaver_intensity_scalar)) * woodmult_w_asrp,
                                (Shape_Length * curr_pond_area_per_m) / 10000 * woodmult_w_asrp)) %>%
  gather(Habitat, Area, Pool:winter.beaver.pond) %>%
  mutate(
    life.stage = case_when(
      Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ "summer",
      Habitat %in% c("winter.pool", "winter.riffle", "winter.beaver.pond") ~ "winter"),
    Habitat = case_when(
      Habitat %in% c("Pool", "Riffle", "Beaver.Pond") ~ Habitat,
      Habitat == "winter.pool" ~ "Pool",
      Habitat == "winter.riffle" ~ "Riffle",
      Habitat == "winter.beaver.pond" ~ "Beaver.Pond"),
    Area = ifelse(pass_tot_asrp == 0,
                  0,
                  Area)) %>%
  select(noaaid, Subbasin_num, pass_tot_asrp, GSU, woodmult_s_asrp, woodmult_w_asrp, tempmult.asrp, Habitat, Area, life.stage, lc, 
         slope.class, rest_perc, rest_perc, both_chk, Scenario_num, year, LW, Floodplain, Beaver, Riparian, Barriers, wood_intensity_scalar, 
         wood_intensity_scalar, chino_mult)

if (fishtype %in% c("spring_chinook", "fall_chinook")) {
  asrp_ss %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * chino_mult,
                         Area_nochino))
}

asrp_ss_mvmt <- asrp_ss %>%
  filter(Scenario_num %in% single_action_mvmt_scenarios)

if (run_single_action == 'no') {
  asrp_ss %<>%
    filter(Scenario_num %in% c("scenario_1", "scenario_2", "scenario_3", 'dev_and_climate', diag_scenarios, food_scenarios, 'cc_only', 'rip_and_climate', 'fp_temp', 
                               'rip_and_flp'))
}

rm(asrp_ss_raw, asrp_ss_scenario, asrp_ss_year)

asrp_ss_spawn %<>%
  filter(slope < .03)
